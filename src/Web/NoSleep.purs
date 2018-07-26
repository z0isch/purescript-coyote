module Web.NoSleep where

import Prelude

import Effect (Effect)

foreign import _enable :: Effect Unit
foreign import _disable :: Effect Unit