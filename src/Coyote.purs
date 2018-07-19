module Coyote where

import Prelude

import Control.Monad.State (StateT, get, gets, modify_, runStateT)
import Data.Array as A
import Data.Foldable (fold, foldM)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List as L
import Data.Map (Map, filter, insert, size, values)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Traversable (for, maximum, sum)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref as Ref
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
  { card :: Array Card
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
  =  A.replicate 4 (Feather 1) 
  <> A.replicate 4 (Feather 2) 
  <> A.replicate 4 (Feather 3) 
  <> A.replicate 4 (Feather 4) 
  <> A.replicate 4 (Feather 5) 
  <> A.replicate 3 (Feather 10) 
  <> A.replicate 2 (Feather 15)
  <> A.replicate 1 (Feather 20)
  <> A.replicate 2 (Feather (-5))
  <> A.replicate 2 (Feather (-10))
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
  { card: []
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

bleh = runStateT processTotal

drawTop :: StateT GameState Effect Card
drawTop = do
  {deck} <- get
  when (A.null deck) $ do
    {discardPile} <- get
    newDeck <- liftEffect $ shuffle discardPile
    modify_ _
      { deck= newDeck
      , discardPile= []
      }
  {head,tail} <- gets \s -> unsafePartial $ fromJust $ A.uncons s.deck
  modify_ _{deck= tail}
  pure head

processTotal :: StateT GameState Effect Int
processTotal = do
  {hands} <- get
  qProcessed <- processQ $ seperateCards $ M.toUnfoldable hands
  allProcessed <- processNonQ $ sortSpecial qProcessed
  pure $ sum allProcessed
  where
    processQ s = case s.q of
      Just (Additive pl) -> do
        c <- drawTop
        --Add this card to the persons hand
        modify_ \st -> st{hands= M.update (\hs -> Just $ hs{card= c A.: hs.card}) pl st.hands}
        case c of
          Feather x -> pure s{feather= x L.: s.feather }
          --This can't be a Question because there is only one in the deck
          SpecialCard x -> pure s{nonQ= x L.: s.nonQ }
      Nothing -> pure s

    processNonQ s = foldM processSpecial s.feather s.nonQ
    
    sortSpecial s= s{nonQ= L.sort s.nonQ}
    
    --Abusing the monoid instance of records
    seperateCards = fold <<< A.concatMap (\(Tuple pl {card}) -> map (seperate pl) card)
      where
        seperate _ (Feather x) = {feather: L.singleton x,nonQ: mempty,q: mempty}
        seperate pl (SpecialCard Question) = {feather: mempty,nonQ: mempty,q: Just (Additive pl)}
        seperate _ (SpecialCard x) = {feather:mempty, nonQ: L.singleton x,q: mempty}

    processSpecial :: L.List Int -> SpeciaCard -> StateT GameState Effect (L.List Int)
    processSpecial feathers = case _ of
      --We've processed the Question if it existed, so it can't be here anyway
      Question -> pure feathers
      Max0 -> pure $ L.delete maxF feathers
      MaxNeg -> pure $ fromMaybe feathers $ L.updateAt maxI (negate maxF) feathers
      X2 -> pure $ map (\x -> if x >= 0 then 2 * x else x) feathers
      Night -> pure feathers
      where 
        maxI = fromMaybe 0 $ L.elemIndex maxF feathers
        maxF = fromMaybe 0 $ maximum feathers

makeMove :: GameState -> Move -> GameState
makeMove gs (Bid bid) = gs
  { currentBid= Just (Tuple gs.currentPlayer bid)
  , currentPlayer= nextPlayer gs
  }
makeMove gs Coyote = gs

shuffle :: forall a. Array a -> Effect (Array a)
shuffle xs = do
  ar <- Ref.new xs
  void $ for (A.range 0 (A.length xs - 1)) \i -> do
    j <- randomInt i (A.length xs - 1)
    swap' ar i j
  Ref.read ar
  where 
    swap' ar i j = do
      vi <- index' ar i
      vj <- index' ar j
      Ref.modify (A.updateAtIndices [Tuple i vj, Tuple j vi]) ar
    index' ar i = do
      mar <- Ref.read ar
      pure $ unsafePartial $ fromJust $ A.index mar i

test1 :: GameState
test1 = { currentBid: Nothing, currentPlayer: 0, deck: [(SpecialCard X2),(Feather 2),(Feather (-5)),(Feather 3),(SpecialCard Question),(Feather 15),(SpecialCard Night),(Feather 3),(Feather 4),(Feather 5),(Feather 5),(Feather 1),(Feather 3),(Feather 2),(Feather 4),(Feather 1),(Feather 4),(Feather (-10)),(SpecialCard MaxNeg),(Feather 20),(Feather 1),(Feather 3),(Feather 2),(Feather 2),(Feather 10),(Feather 5),(Feather 1),(Feather (-10)),(SpecialCard Max0),(Feather 15),(Feather (-5)),(Feather 10),(Feather 10),(Feather 5),(Feather 4)], discardPile: [], hands: (M.fromFoldable [(Tuple 0 { card: [Feather 5], coins: 0 }),(Tuple 1 { card: [Feather (-10)], coins: 0 }), (Tuple 2 { card: [SpecialCard Question], coins: 0 })]) }