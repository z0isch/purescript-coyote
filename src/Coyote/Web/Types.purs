module Coyote.Web.Types where

import Prelude

import Coyote.Types (Card, GameState, Player, Round, PlayerState)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Halogen.HTML (a)

type TupleObj a b = {a :: a, b :: b}

type UserId = String

type GameId = String

type CoyoteCookie = 
  { id :: GameId 
  , userId :: UserId
  }

type WebGame = 
  { id :: GameId
  , playerMap :: Map UserId Player
  , state :: GameState
  }
  
type WebGameDTO = 
  { id :: GameId
  , playerMap :: Array (TupleObj UserId Player)
  , state :: GameStateDTO
  }
  
type RoundDTO = 
  { bid :: TupleObj Player Int
  , winner :: Player
  , loser :: Player
  , total :: Int
  , hands :: Array (TupleObj Int (Array Card))
  }

type GameStateDTO =
  { deck :: Array Card
  , discardPile :: Array Card
  , currentPlayer :: Player
  , players :: Array (TupleObj Player PlayerState)
  , currentBid :: Maybe (TupleObj Player Int)
  , previousRounds :: Array RoundDTO
  }

toTuple :: forall a b.
  { a :: a
  , b :: b
  }
  -> Tuple a b
toTuple {a,b} = Tuple a b

fromTuple :: forall a b.
  Tuple a b
  -> { a :: a
     , b :: b
     }
fromTuple (Tuple a b) = {a,b}

fromMap :: forall a b. Map a b -> Array (TupleObj a b)
fromMap = map fromTuple <<< M.toUnfoldable

toMap :: forall a b. Ord a => Array (TupleObj a b) -> Map a b
toMap =  M.fromFoldable <<< map toTuple

fromRound :: Round -> RoundDTO
fromRound c = c
  { bid= fromTuple c.bid
  , hands= fromMap c.hands
  }
toRound :: RoundDTO -> Round
toRound c = c
  { bid= toTuple c.bid
  , hands= toMap c.hands
  }

fromGameState :: GameState -> GameStateDTO
fromGameState c = c
  { players= fromMap c.players
  , currentBid= map fromTuple c.currentBid
  , previousRounds= map fromRound c.previousRounds
  }
toGameState :: GameStateDTO -> GameState
toGameState c = c
  { players= toMap c.players
  , currentBid = map toTuple c.currentBid
  , previousRounds= map toRound c.previousRounds
  }

fromWebGame :: WebGame -> WebGameDTO
fromWebGame g = g
  { playerMap= fromMap g.playerMap
  , state= fromGameState g.state
  }
toWebGame :: WebGameDTO -> WebGame
toWebGame g = g
  { playerMap= toMap g.playerMap
  , state= toGameState g.state
  }