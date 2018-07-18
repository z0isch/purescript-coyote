module Main where

import Prelude

import Components.Button as B
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Firebase (foo)
import Foreign (Foreign)
import Foreign.Keys (keys)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

bar :: CR.Producer Foreign Aff Unit
bar = CRA.produce \emitter -> foo (CRA.emit emitter)

tee :: (B.Query ~> Aff) -> CR.Consumer Foreign Aff Unit
tee query = CR.consumer \msg -> do
  liftEffect $ logShow $ runExcept $ keys msg
  query $ H.action B.Toggle
  pure Nothing

main :: Effect Unit
main = do
  HA.runHalogenAff do
    b <- HA.awaitBody
    io <- runUI B.myButton unit b
    CR.runProcess (bar CR.$$ tee io.query)