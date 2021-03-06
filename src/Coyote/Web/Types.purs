module Coyote.Web.Types where

import Prelude

import Coyote.Full (Player)
import Data.Map (Map)
import Data.Map as M

import Data.Tuple (Tuple(..))

type TupleObj a b = {a :: a, b :: b}

type UserId = String

type GameId = String

type StateHash = Int

type CoyoteCookie = 
  { id :: GameId 
  , userId :: UserId
  }

type WebGame s = 
  { playerMap :: Map UserId Player
  , state :: s
  , stateHash :: StateHash
  }
  
type WebGameDTO s = 
  { playerMap :: Array (TupleObj UserId Player)
  , state :: s
  , stateHash :: StateHash
  }


toTuple :: ∀ a b.
  { a :: a
  , b :: b
  }
  -> Tuple a b
toTuple {a,b} = Tuple a b

fromTuple :: ∀ a b.
  Tuple a b
  -> { a :: a
     , b :: b
     }
fromTuple (Tuple a b) = {a,b}

fromMap :: ∀ a b. Map a b -> Array (TupleObj a b)
fromMap = map fromTuple <<< M.toUnfoldable

toMap :: ∀ a b. Ord a => Array (TupleObj a b) -> Map a b
toMap =  M.fromFoldable <<< map toTuple