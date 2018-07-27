module Main where

import Prelude

import Components.Simple as SimpleComponent
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT)
import Coyote.Simple as Simple
import Coyote.Web.Simple as SimpleWeb
import Coyote.Web.Types (CoyoteCookie, WebGame, GameId)
import Data.Array as A
import Data.Either (Either(..), either, note)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routes (Routes(..), myRoute)
import Routing (match)
import Routing.Hash (getHash, setHash)
import Simple.JSON (readJSON, writeJSON)
import Sub (Sub)
import Sub as Sub
import Web.Cookies (deleteCookie, getCookie, setCookie)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Firebase as Firebase
import Web.HTML (window)
import Web.HTML.Location (href)
import Web.HTML.Window (location)

cookieName :: String
cookieName = "coyote-game"

subToGame :: Sub -> (SimpleComponent.Query ~> Aff) -> String -> Aff Unit
subToGame sub query id = Sub.updateSub sub $ launchAff $ CR.runProcess (firebaseProducer id CR.$$ firebaseConsumer query)

firebaseProducer :: GameId -> CR.Producer (WebGame Simple.GameState) Aff Unit
firebaseProducer gId = CRA.produce $ Firebase.subscribeToGame gId SimpleWeb.toWebGame <<< CRA.emit

firebaseConsumer :: (SimpleComponent.Query ~> Aff) -> CR.Consumer (WebGame Simple.GameState) Aff Unit
firebaseConsumer query = CR.consumer \game -> do
  query $ H.action $ SimpleComponent.GameUpdate game
  pure Nothing

processMsgs :: forall a. Sub -> String ->  (SimpleComponent.Query ~> Aff) -> SimpleComponent.Message -> Aff (Maybe a)
processMsgs sub baseUrl query = case _ of
  SimpleComponent.UnsubscribeFromGame -> do
    H.liftEffect $ deleteCookie cookieName
    Sub.killSub sub
    query $ H.action $ SimpleComponent.HandleInput {cookie: Nothing, baseUrl}
    pure Nothing
  SimpleComponent.DrawCard c -> do
    err <- runExceptT do
      game <- ExceptT $ note "Can't find that game" <$> Firebase.getGame c.id SimpleWeb.toWebGame
      pl <- except $ note "You're not in that game" $ Map.lookup c.userId game.playerMap
      {hand} <- except $ note "You're not in that game" $ Map.lookup pl game.state.players
      when (A.null hand) $ lift $ Firebase.drawCard c
      pure unit
    either Console.error pure err
    pure Nothing
  SimpleComponent.CallCoyote c -> do
    Firebase.callCoyote c
    pure Nothing
  SimpleComponent.CreateNewGame -> do
    c <- H.liftEffect $ (\i1 i2 -> {id:show i1,userId: show i2}) <$> genUUID <*> genUUID
    state <- H.liftEffect $ Simple.initialGame
    stateHash <- H.liftEffect $ show <$> genUUID
    H.liftEffect $ Firebase.newGame c.id
      { state
      , playerMap: mempty
      , stateHash
      } SimpleWeb.fromWebGame
    H.liftEffect $ Firebase.joinGame c do
      setCookie cookieName (writeJSON c) Nothing
      launchAff_ do
        query $ H.action $ SimpleComponent.HandleInput {cookie: Just c, baseUrl}
        subToGame sub query c.id
    pure Nothing

runHalogen :: Effect Unit
runHalogen = HA.runHalogenAff do
    sub <- Sub.newSub

    baseUrl <- H.liftEffect $ window >>= location >>= href
    cookie <- H.liftEffect do
      cookieE <- getCookie cookieName 
      case map readJSON cookieE of
        Nothing -> pure Nothing
        (Just (Left err)) -> do
          Console.error $ "Can't parse cookie: "<> show err
          pure Nothing
        (Just (Right cookie)) -> pure $ Just cookie
    
    _ <- HA.awaitBody
    HA.selectElement (QuerySelector "#coyote") >>= case _ of
      Nothing -> Console.error "Can't find div"
      Just el -> do
        io <- runUI SimpleComponent.ui {cookie,baseUrl} el
    
        io.subscribe $ CR.consumer $ processMsgs sub baseUrl io.query
        
        case cookie of
          Nothing -> pure unit
          Just c -> subToGame sub io.query c.id

joinAndSetCookie :: String -> Effect Unit
joinAndSetCookie gId = do
  userId <- show <$> genUUID
  let c = {id: gId, userId}
  Firebase.joinGame c do
    setCookie cookieName (writeJSON c) Nothing
    runHalogen

main :: Effect Unit
main = getHash >>= match myRoute >>> case _ of
  Left err -> Console.error err
  Right (Join gId) -> do
    setHash ""
    getCookie cookieName >>= case _ of
      Nothing -> joinAndSetCookie gId
      Just cookieE -> do
        case readJSON cookieE of
          Left err -> Console.error $ "Can't parse cookie: "<> show err
          Right (cookie :: CoyoteCookie) ->
            if (cookie.id /= gId) 
            then joinAndSetCookie gId
            else runHalogen
  _ -> runHalogen