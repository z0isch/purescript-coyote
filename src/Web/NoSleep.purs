module Web.NoSleep where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import _enable :: EffectFnAff Unit
foreign import _disable :: Effect Unit

enable :: Aff Unit
enable = fromEffectFnAff _enable

disable :: Effect Unit
disable = _disable