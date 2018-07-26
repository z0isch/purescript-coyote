module Web.Zingtouch where

import Prelude

import Data.Function.Uncurried (Fn1, Fn3, runFn1, runFn3)
import Effect (Effect)
import Foreign (Foreign)
import Web.HTML (HTMLElement)

foreign import _bind :: Fn3 HTMLElement String (Foreign -> Effect Unit) (Effect Unit)
foreign import _unbind :: Fn1 HTMLElement (Effect Unit)

bindPan :: HTMLElement -> (Foreign -> Effect Unit) -> Effect Unit
bindPan el f = runFn3 _bind el "pan" f

unbind :: HTMLElement -> Effect Unit
unbind = runFn1 _unbind