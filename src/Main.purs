module Main where

import Prelude

import Components.Main (cookieName)
import Components.Main as M
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Coyote.Types (addPlayer)
import Coyote.Web.Types (GameId, WebGame, CoyoteCookie)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_, runAff, runAff_)
import Effect.Aff.Lock (newLock, withLock)
import Effect.Class.Console as Console
import Effect.Ref as R
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routes (Routes(..), myRoute)
import Routing (match)
import Routing.Hash (getHash)
import Simple.JSON (readJSON, writeJSON)
import Web.Cookies (getCookie, setCookie)
import Web.Firebase (getGame, subscribeToGame, writeGame)

firebaseProducer :: GameId -> CR.Producer WebGame Aff Unit
firebaseProducer gId = CRA.produce $ subscribeToGame gId <<< CRA.emit

firebaseConsumer :: (M.Query ~> Aff) -> CR.Consumer WebGame Aff Unit
firebaseConsumer query = CR.consumer \game -> do
  query $ H.action $ M.GameUpdate game
  pure Nothing

newCookie :: String -> Effect Unit -> Effect Unit
newCookie gId f = do
  userId <- show <$> genUUID
  runAff_ (const f) $ getGame gId >>= case _ of
    Nothing -> Console.error "Can't find that game!"
    Just game -> H.liftEffect $ do
      writeGame gId game
        { state= addPlayer game.state
        , playerMap= Map.insert userId (Map.size (game.state.players)) game.playerMap
        }
      setCookie cookieName (writeJSON {id:gId,userId}) Nothing
  
main :: Effect Unit
main = do
  
  --Race condition inside of firebase, need some kind of db lock!
  getHash >>= match myRoute >>> case _ of
    Left err -> Console.error err
    Right (Join gId) -> do
      getCookie cookieName >>= case _ of
        Nothing -> newCookie gId runHalogen
        Just cookieE -> do
          case readJSON cookieE of
            Left err -> Console.error $ "Can't parse cookie: "<> show err
            Right (cookie :: CoyoteCookie) -> if (cookie.id /= gId) 
              then newCookie gId runHalogen
              else runHalogen
    _ -> runHalogen

runHalogen :: Effect Unit
runHalogen = do
  subscription <- R.new Nothing
  HA.runHalogenAff do
    subscriptionLock <- newLock
    b <- HA.awaitBody
    io <- runUI M.ui unit b

    io.subscribe $ CR.consumer $ case _ of
      M.UnsubscribeFromGame c -> withLock subscriptionLock do
        H.liftEffect (R.read subscription) >>= case _ of
          Nothing -> pure unit
          Just fiber -> killFiber (error "Unsubscribing to game") fiber
        pure Nothing
      M.PushGameUpdate c g-> do
        H.liftEffect $ writeGame c.id g
        pure Nothing
      M.SubscribeToGame c -> do
        withLock subscriptionLock do
          H.liftEffect (R.read subscription) >>= case _ of
            Nothing -> pure unit
            Just fiber -> killFiber (error "Unsubscribing to game") fiber
          fiber <- H.liftEffect $ launchAff $ CR.runProcess (firebaseProducer c.id CR.$$ firebaseConsumer io.query)
          H.liftEffect $ R.write (Just fiber) subscription
          pure Nothing

    io.query $ H.action $ M.Initialize
    