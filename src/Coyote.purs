module Coyote where

import Prelude

import Control.Monad.State (State, get, gets, modify_, runState)
import Data.Array (fold, foldl, head, index, length, null, range, replicate, tail, updateAtIndices, (:))
import Data.Enum (class Enum)
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (_1, _2, over)
import Data.List as L
import Data.Map (Map, filter, insert, size, values)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Semigroup.Foldable (foldMap1Default)
import Data.Traversable (for, maximum, sum, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Effect.Ref (modify, new, read)
import Partial.Unsafe (unsafePartial)

data SpeciaCard
  = Question
  | Max0
  | MaxNeg
  | X2
  | Night
derive instance genericSpeciaCard :: Generic SpeciaCard _
instance eqSpeciaCard :: Eq SpeciaCard where
  eq = genericEq
instance showSpeciaCard :: Show SpeciaCard where
  show = genericShow
instance ordSpecialCard :: Ord SpeciaCard where
  compare = genericCompare

data Card 
  = Feather Int
  | SpecialCard SpeciaCard
derive instance genericCard :: Generic Card _
instance showCard :: Show Card where
  show = genericShow

data Move 
  = Bid Int
  | Coyote
derive instance genericMove :: Generic Move _
instance showMove :: Show Move where
  show = genericShow

type Player = Int

type PlayerState = 
  { card :: Maybe Card
  , coins :: Int
  }

type GameState =
  { deck :: Array Card
  , discardPile :: Array Card
  , currentPlayer :: Player
  , hands :: Map Player PlayerState
  , currentBid :: Maybe (Tuple Player Int)
  }

unshuffledDeck :: Array Card
unshuffledDeck 
  =  replicate 4 (Feather 1) 
  <> replicate 4 (Feather 2) 
  <> replicate 4 (Feather 3) 
  <> replicate 4 (Feather 4) 
  <> replicate 4 (Feather 5) 
  <> replicate 3 (Feather 10) 
  <> replicate 2 (Feather 15)
  <> replicate 1 (Feather 20)
  <> replicate 2 (Feather (-5))
  <> replicate 2 (Feather (-10))
  <> map SpecialCard [Question, Max0, MaxNeg, X2, Night]

initialGame :: Effect GameState
initialGame = do
  s <- shuffle unshuffledDeck
  pure 
    { deck: s
    , discardPile: mempty
    , currentPlayer: 0
    , hands: mempty
    , currentBid: Nothing
    }

initialPlayer :: PlayerState
initialPlayer =
  { card: Nothing
  , coins: 0
  }

addPlayer :: GameState -> GameState
addPlayer gs = gs
  { hands= insert (size gs.hands) initialPlayer gs.hands
  }

playersStillIngame :: GameState -> Map Player PlayerState
playersStillIngame {hands} = filter (\{coins} -> coins < 3) hands

nextPlayer :: GameState -> Player
nextPlayer gs@{currentPlayer, hands} = (currentPlayer + 1 + addBy) `mod` (size hands)
  where
    nextP = (currentPlayer + 1) `mod` (size hands)
    looped = L.drop nextP (values hands) <> values hands
    addBy = L.length $ L.takeWhile (\{coins} -> coins >= 3) looped

bleh = runState processTotal

processTotal :: State GameState Int
processTotal = do
  {hands} <- get
  map sum $ processAllSpecial $ sortSpecial $ seperateCards $ values hands
  where
    processSpecial :: L.List Int -> SpeciaCard -> State GameState (L.List Int)
    processSpecial feathers c = do
      {deck,discardPile} <- get
      let
          drawTop = _
            { deck= fromMaybe [] $ tail deck
            , discardPile= if null deck
                then fromMaybe [] $ tail discardPile
                else discardPile
            }
          topOfDeck = case fromMaybe (fromMaybe (Feather 0) $ head discardPile) $ head deck of
            SpecialCard _ -> 0
            Feather x -> x
          maxI = fromMaybe 0 $ L.elemIndex maxF feathers
          maxF = fromMaybe 0 $ maximum feathers
      case c of
        Question -> do
          modify_ drawTop
          pure $ topOfDeck L.: feathers
        Max0 -> pure $ L.delete maxF feathers
        MaxNeg -> pure $ fromMaybe feathers $ L.updateAt maxI (negate maxF) feathers
        X2 -> pure $ map (\x -> if x >= 0 then 2 * x else x) feathers
        Night -> pure feathers
    processAllSpecial {special,feather} = foldM processSpecial feather special
    sortSpecial s@{special,feather}= s{special= L.sort special}
    seperateCards = fold <<< L.mapMaybe (\{card} -> seperate <$> card)
    seperate (Feather x) = {feather: L.singleton x,special: mempty}
    seperate (SpecialCard x) = {feather:mempty, special: L.singleton x}

makeMove :: GameState -> Move -> GameState
makeMove gs (Bid bid) = gs
  { currentBid= Just (Tuple gs.currentPlayer bid)
  , currentPlayer= nextPlayer gs
  }
makeMove gs Coyote = gs

shuffle :: forall a. Array a -> Effect (Array a)
shuffle xs = do
  ar <- new xs
  void $ for (range 0 (length xs - 1)) \i -> do
    j <- randomInt i (length xs - 1)
    swap' ar i j
  read ar
  where 
    swap' ar i j = do
      vi <- index' ar i
      vj <- index' ar j
      modify (updateAtIndices [Tuple i vj, Tuple j vi]) ar
    index' ar i = do
      mar <- read ar
      pure $ unsafePartial $ fromJust $ index mar i

test1 :: GameState
test1 = { currentBid: Nothing, currentPlayer: 0, deck: [(Feather 2),(Feather (-5)),(Feather 3),(SpecialCard Question),(Feather 15),(SpecialCard X2),(SpecialCard Night),(Feather 3),(Feather 4),(Feather 5),(Feather 5),(Feather 1),(Feather 3),(Feather 2),(Feather 4),(Feather 1),(Feather 4),(Feather (-10)),(SpecialCard MaxNeg),(Feather 20),(Feather 1),(Feather 3),(Feather 2),(Feather 2),(Feather 10),(Feather 5),(Feather 1),(Feather (-10)),(SpecialCard Max0),(Feather 15),(Feather (-5)),(Feather 10),(Feather 10),(Feather 5),(Feather 4)], discardPile: [], hands: (M.fromFoldable [(Tuple 0 { card: Just (Feather 5), coins: 0 }),(Tuple 1 { card: Just (Feather (-10)), coins: 0 }), (Tuple 2 { card: Just (SpecialCard X2), coins: 0 })]) }