module Coyote where

import Prelude

import Data.Array (index, length, range, replicate, updateAtIndices)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as L
import Data.Map (Map, filter, insert, size, values)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Effect.Ref (modify, new, read)
import Partial.Unsafe (unsafePartial)

data Card 
  = Feather Int
  | Question
  | Max0
  | MaxNeg
  | X2
  | Night
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
  , discard :: Array Card
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
  <> [Question, Max0, MaxNeg, X2, Night]

initialGame :: Effect GameState
initialGame = do
  s <- shuffle unshuffledDeck
  pure 
    { deck: s
    , discard: mempty
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
test1 = { currentBid: Nothing, currentPlayer: 0, deck: [(Feather (-5)),MaxNeg,(Feather 5),(Feather 15),(Feather 1),(Feather 2),(Feather 3),Night,(Feather 10),(Feather (-10)),(Feather 20),(Feather (-10)),(Feather 3),Max0,(Feather 3),(Feather 1),(Feather 10),(Feather 15),(Feather 5),(Feather 4),(Feather 5),(Feather 4),(Feather 4),(Feather 5),X2,(Feather 4),(Feather 10),Question,(Feather 2),(Feather 1),(Feather (-5)),(Feather 1),(Feather 2),(Feather 2),(Feather 3)], discard: [], hands: (M.fromFoldable [(Tuple 0 { card: Nothing, coins: 0 }),(Tuple 1 { card: Nothing, coins: 3 }),(Tuple 2 { card: Nothing, coins: 0 })]) }
test3 :: GameState
test3 = { currentBid: Nothing, currentPlayer: 1, deck: [(Feather (-5)),MaxNeg,(Feather 5),(Feather 15),(Feather 1),(Feather 2),(Feather 3),Night,(Feather 10),(Feather (-10)),(Feather 20),(Feather (-10)),(Feather 3),Max0,(Feather 3),(Feather 1),(Feather 10),(Feather 15),(Feather 5),(Feather 4),(Feather 5),(Feather 4),(Feather 4),(Feather 5),X2,(Feather 4),(Feather 10),Question,(Feather 2),(Feather 1),(Feather (-5)),(Feather 1),(Feather 2),(Feather 2),(Feather 3)], discard: [], hands: (M.fromFoldable [(Tuple 0 { card: Nothing, coins: 0 }),(Tuple 1 { card: Nothing, coins: 0 }),(Tuple 2 { card: Nothing, coins: 3 })]) }