module Main where

import Prelude

import Components.Main as M
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Coyote.Web.Types (GameId, WebGame)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Firebase (subscribeToGame, writeGame)

firebaseProducer :: GameId -> CR.Producer WebGame Aff Unit
firebaseProducer gId = CRA.produce \emitter -> subscribeToGame gId (CRA.emit emitter)

firebaseConsumer :: (M.Query ~> Aff) -> CR.Consumer WebGame Aff Unit
firebaseConsumer query = CR.consumer \game -> do
  query $ H.action $ M.GameUpdate game
  pure Nothing

main :: Effect Unit
main = do
  HA.runHalogenAff do
    b <- HA.awaitBody
    io <- runUI M.ui unit b
    io.subscribe $ CR.consumer $ case _ of
      M.PushGameUpdate c g-> do
        H.liftEffect $ writeGame c.id g
        pure Nothing
      M.SubscribeToGame c -> do
        H.liftEffect $ launchAff_ $ CR.runProcess (firebaseProducer c.id CR.$$ firebaseConsumer io.query)
        pure Nothing
    io.query $ H.action $ M.Initialize