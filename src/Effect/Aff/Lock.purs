module Effect.Aff.Lock where

import Prelude

import Effect.AVar (AVar)
import Effect.Aff (Aff, finally)
import Effect.Aff.AVar (new, take, put)

-- | A lock that can be exclusively acquired with 'acquireLock'.
newtype Lock = Lock (AVar Unit)

-- | Create a new lock.
newLock :: Aff Lock
newLock = Lock <$> new unit

-- | Block until the lock is available, then grab it. Something that acquires
-- the lock should at some point subsequently relinquish it with 'releaseLock'.
-- Consider using 'withLock' instead unless you need more fine-grained control.
acquireLock :: Lock -> Aff Unit
acquireLock (Lock v) = take v

-- | Release a lock that you have previously acquired with 'acquireLock'.
releaseLock :: Lock -> Aff Unit
releaseLock (Lock v) = put unit v

-- | Acquire the lock, perform some action while the lock is held, then
-- release the lock. You can use this instead of manually calling 'acquireLock'
-- and 'releaseLock'.
withLock :: Lock -> Aff ~> Aff
withLock lock action = finally (releaseLock lock) (acquireLock lock *> action)