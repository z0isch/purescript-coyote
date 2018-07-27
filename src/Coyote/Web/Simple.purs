module Coyote.Web.Simple where

import Prelude

import Coyote.Web.Types as CoyoteWeb
import Coyote.Full as Full
import Coyote.Simple as Simple

type RoundDTO = 
  { hands :: Array (CoyoteWeb.TupleObj Int (Array Full.Card))
  , total :: Int
  }

type GameStateDTO =
  { deck :: Array Full.Card
  , discardPile :: Array Full.Card
  , players :: Array (CoyoteWeb.TupleObj Full.Player Simple.PlayerState)
  , previousRounds :: Array RoundDTO
  }

fromRound :: Simple.Round -> RoundDTO
fromRound c = c
  { hands= CoyoteWeb.fromMap c.hands
  }
toRound :: RoundDTO -> Simple. Round
toRound c = c
  {  hands= CoyoteWeb.toMap c.hands
  }

fromGameState :: Simple.GameState -> GameStateDTO
fromGameState c = c
  { players= CoyoteWeb.fromMap c.players
  , previousRounds= map fromRound c.previousRounds
  }

toGameState :: GameStateDTO -> Simple.GameState
toGameState c = c
  { players= CoyoteWeb.toMap c.players
  , previousRounds= map toRound c.previousRounds
  }

fromWebGame :: CoyoteWeb.WebGame Simple.GameState -> CoyoteWeb.WebGameDTO GameStateDTO
fromWebGame g = g
  { playerMap= CoyoteWeb.fromMap g.playerMap
  , state= fromGameState g.state
  }
toWebGame :: CoyoteWeb.WebGameDTO GameStateDTO -> CoyoteWeb.WebGame Simple.GameState
toWebGame g = g
  { playerMap= CoyoteWeb.toMap g.playerMap
  , state= toGameState g.state
  }