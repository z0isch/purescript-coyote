module Sub where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, error, killFiber)
import Effect.Aff.Lock (Lock, newLock, withLock)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as R

type Sub = 
  { fiber :: Ref (Maybe (Fiber Unit))
  , lock :: Lock
  }

newSub :: Aff Sub
newSub = do
  fiber <- liftEffect (R.new Nothing)
  lock <- newLock
  pure {fiber, lock}

updateSub :: Sub -> Effect (Fiber Unit) -> Aff Unit
updateSub sub f = withLock sub.lock do
    killRef sub.fiber
    fiber <- liftEffect f
    liftEffect $ R.write (Just fiber) sub.fiber

killSub :: Sub -> Aff Unit
killSub {lock,fiber} = withLock lock $ killRef fiber

killRef :: Ref (Maybe (Fiber Unit)) -> Aff Unit
killRef fiber = liftEffect (R.read fiber) >>= case _ of
    Nothing -> pure unit
    Just g -> killFiber (error "Unsubscribing to game") g