module Coyote where

import Prelude

import Data.Array (index, length, range, replicate, updateAtIndices)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, filter, insert, size)
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
  , currentBid :: Maybe Int
  }

initialGame :: Effect GameState
initialGame = do
  d <- shuffle $ replicate 4 (Feather 1) 
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
  pure 
    { deck: d
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
addPlayer gs = gs{
  hands= insert (size gs.hands) initialPlayer gs.hands
}

nextPlayer :: GameState -> Player
nextPlayer {currentPlayer, hands} = currentPlayer + 1 `mod` playersStillIngame
  where playersStillIngame = size $ filter (\{coins} -> coins < 3) hands

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