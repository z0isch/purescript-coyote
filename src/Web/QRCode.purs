module Web.QRCode where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Web.HTML (HTMLElement)

foreign import _insert :: Fn2 HTMLElement String (Effect Unit)

insertQRCode :: HTMLElement -> String -> Effect Unit
insertQRCode = runFn2 _insert