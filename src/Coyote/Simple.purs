module Coyote.Simple where

import Prelude

import Control.Monad.State (StateT, gets, modify_)
import Coyote.Full as Full
import Data.Foldable (any)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(Just))
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Array as A

type PlayerState = 
  { hand :: Full.Hand
  }

type Round =
  { hands :: Map Full.Player Full.Hand
  , total :: Int
  }

type GameState = 
  { deck :: Array Full.Card
  , discardPile :: Array Full.Card
  , players :: Map Full.Player PlayerState
  , previousRounds :: Array Round
  }

data Move
  = DrawCard Full.Player
  | Coyote
  
addPlayer :: GameState -> GameState
addPlayer gs = gs
  { players= M.insert (M.size gs.players) {hand:mempty} gs.players
  }

initialGame :: Effect GameState
initialGame = do
  s <- Full.shuffle Full.unshuffledDeck
  pure 
    { deck: s
    , discardPile: mempty
    , players: mempty
    , previousRounds: mempty
    }

makeMove :: Move -> StateT GameState Effect Unit
makeMove (DrawCard pl) = do
  c <- Full.drawTop
  modify_ \g -> g{ players= M.update (Just <<< _{hand= [c]}) pl g.players}
makeMove Coyote = do
  total <- Full.processTotal
  needsShuffled <- gets \g -> any (any ((==) (Full.SpecialCard Full.Night)) <<< _.hand) g.players
  modify_ \g -> g
    { players= map _{hand= []} g.players
    , discardPile=  (A.concatMap _.hand (A.fromFoldable (M.values g.players))) <> g.discardPile
    , previousRounds= g.previousRounds <> [
      { total
      , hands: map _.hand g.players
      }]
    }
  when needsShuffled do
    shuffled <- (gets \g -> g.deck <> g.discardPile) >>= liftEffect <<< Full.shuffle
    modify_ _
      { discardPile= []
      , deck= shuffled
      }