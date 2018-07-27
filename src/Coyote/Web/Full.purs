module Coyote.Web.Full where

import Prelude

import Data.Maybe (Maybe)
import Coyote.Full
import Coyote.Web.Types

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

fromWebGame :: WebGame GameState -> WebGameDTO GameStateDTO
fromWebGame g = g
  { playerMap= fromMap g.playerMap
  , state= fromGameState g.state
  }
toWebGame :: WebGameDTO GameStateDTO -> WebGame GameState
toWebGame g = g
  { playerMap= toMap g.playerMap
  , state= toGameState g.state
  }