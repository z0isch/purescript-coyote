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

subToGame :: Sub -> (SimpleComponent.Query ~> Aff) -> GameId -> Aff Unit
subToGame sub query id = Sub.updateSub sub $ launchAff $ CR.runProcess (firebaseProducer id CR.$$ firebaseConsumer query)

firebaseProducer :: GameId -> CR.Producer (WebGame Simple.GameState) Aff Unit
firebaseProducer gId = CRA.produce \emitter -> Firebase.subscribeToGame gId \dto -> CRA.emit emitter (SimpleWeb.toWebGame dto)

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
  SimpleComponent.DrawCard c webGame -> do
    err <- runExceptT do
      game <- map SimpleWeb.toWebGame (ExceptT $ note "Can't find that game" <$> Firebase.getGame c.id)
      pl <- except $ note "You're not in that game" $ Map.lookup c.userId game.playerMap
      {hand} <- except $ note "You're not in that game" $ Map.lookup pl game.state.players
      when (A.null hand) $ lift $ Firebase.drawCard c webGame
    either Console.error pure err
    pure Nothing
  SimpleComponent.CallCoyote c webGame -> do
    Firebase.callCoyote c webGame
    pure Nothing
  SimpleComponent.CreateNewGame -> do
    c <- H.liftEffect $ (\i1 i2 -> {id:show i1,userId: show i2}) <$> genUUID <*> genUUID
    state <- H.liftEffect $ Simple.initialGame
    let webGame =
          { state
          , playerMap: mempty
          , stateHash: 0
          }
    H.liftEffect $ Firebase.newGame c.id (SimpleWeb.fromWebGame webGame)
    Firebase.joinGame c webGame
    H.liftEffect $ setCookie cookieName (writeJSON c) Nothing
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

joinAndSetCookie :: GameId -> Aff Unit
joinAndSetCookie gId = do
  userId <- H.liftEffect $ show <$> genUUID
  let c = {id: gId, userId}
  Firebase.getGame gId >>= case _ of
    Nothing -> Console.error "Can't find that game"
    Just g -> do
      Firebase.joinGame c (SimpleWeb.toWebGame g)
      H.liftEffect $ setCookie cookieName (writeJSON c) Nothing
      H.liftEffect $ runHalogen

main :: Effect Unit
main = do
  getHash >>= match myRoute >>> case _ of
    Left err -> Console.error err
    Right (Join gId) -> do
      setHash ""
      getCookie cookieName >>= case _ of
        Nothing -> launchAff_ $ joinAndSetCookie gId
        Just cookieE -> do
          case readJSON cookieE of
            Left err -> Console.error $ "Can't parse cookie: "<> show err
            Right (cookie :: CoyoteCookie) ->
              if (cookie.id /= gId)
              then launchAff_ $ joinAndSetCookie gId
              else runHalogen
    _ -> runHalogen
