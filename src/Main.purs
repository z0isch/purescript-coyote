module Main where

import Prelude

import Components.Main (cookieName)
import Components.Main as M
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Coyote.Types (addPlayer, initialGame)
import Coyote.Web.Types (CoyoteCookie, GameId, WebGame)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_, runAff_)
import Effect.Aff.Lock (newLock, withLock)
import Effect.Class.Console (logShow)
import Effect.Class.Console as Console
import Effect.Ref as R
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routes (Routes(..), myRoute)
import Routing (match)
import Routing.Hash (getHash, setHash)
import Simple.JSON (readJSON, writeJSON)
import Web.Cookies (deleteCookie, getCookie, setCookie)
import Web.Firebase (getGame, newGame, subscribeToGame, updateGame)
import Web.HTML (window)
import Web.HTML.Location (href)
import Web.HTML.Window (location)

firebaseProducer :: GameId -> CR.Producer WebGame Aff Unit
firebaseProducer gId = CRA.produce $ subscribeToGame gId <<< CRA.emit

firebaseConsumer :: (M.Query ~> Aff) -> CR.Consumer WebGame Aff Unit
firebaseConsumer query = CR.consumer \game -> do
  query $ H.action $ M.GameUpdate game
  pure Nothing

joinGame :: CoyoteCookie -> Effect Unit -> Effect Unit
joinGame c f = do
  runAff_ (const f) do
    getGame c.id >>= case _ of
      Nothing -> Console.error "Can't find that game!"
      Just game -> tryJoinGame game
  where
    tryJoinGame game = do
      let newGame = game
            { state= addPlayer game.state
            , playerMap= Map.insert c.userId (Map.size (game.state.players)) game.playerMap
            }
      updateGame c.id newGame tryJoinGame
      H.liftEffect $ setCookie cookieName (writeJSON c) Nothing

runHalogen :: Effect Unit
runHalogen = do
  subscription <- R.new Nothing
  HA.runHalogenAff do
    subscriptionLock <- newLock
    b <- HA.awaitBody
    
    baseUrl <- H.liftEffect $ window >>= location >>= href
    cookie <- H.liftEffect do
      cookieE <- getCookie cookieName 
      case map readJSON cookieE of
        Nothing -> pure Nothing
        (Just (Left err)) -> do
          Console.error $ "Can't parse cookie: "<> show err
          pure Nothing
        (Just (Right cookie)) -> pure $ Just cookie
    H.liftEffect $ logShow cookie
    io <- runUI M.ui {cookie,baseUrl} b

    let 
      killSub = H.liftEffect (R.read subscription) >>= case _ of
        Nothing -> pure unit
        Just fiber -> killFiber (error "Unsubscribing to game") fiber
      subscribeToGame id = withLock subscriptionLock $ do
        killSub
        fiber <- H.liftEffect $ launchAff $ CR.runProcess (firebaseProducer id CR.$$ firebaseConsumer io.query)
        H.liftEffect $ R.write (Just fiber) subscription

    io.subscribe $ CR.consumer $ case _ of
      M.UnsubscribeFromGame -> do
        H.liftEffect $ deleteCookie cookieName
        withLock subscriptionLock killSub
        io.query $ H.action $ M.HandleInput {cookie: Nothing, baseUrl}
        pure Nothing
      M.SubscribeToGame c -> do
        subscribeToGame c.id
        pure Nothing
      M.CreateNewGame -> do
        c <- H.liftEffect $ (\i1 i2 -> {id:show i1,userId: show i2}) <$> genUUID <*> genUUID
        gs <- H.liftEffect $ initialGame
        stateHash <- H.liftEffect $ show <$> genUUID
        H.liftEffect $ newGame c.id 
          { state: gs
          , playerMap: mempty
          , stateHash
          }
        subscribeToGame c.id
        H.liftEffect $ joinGame c (launchAff_ $ io.query $ H.action $ M.HandleInput {cookie: Just c, baseUrl})
        pure Nothing

    case cookie of
      Nothing -> pure unit
      Just c -> subscribeToGame c.id
    
main :: Effect Unit
main = do
  getHash >>= match myRoute >>> case _ of
    Left err -> Console.error err
    Right (Join gId) -> do
      setHash ""
      getCookie cookieName >>= case _ of
        Nothing -> do
          userId <- show <$> genUUID
          joinGame {id: gId, userId} runHalogen
        Just cookieE -> do
          case readJSON cookieE of
            Left err -> Console.error $ "Can't parse cookie: "<> show err
            Right (cookie :: CoyoteCookie) -> 
              if (cookie.id /= gId) 
              then do
                userId <- show <$> genUUID
                joinGame {id: gId, userId} runHalogen
              else runHalogen
    _ -> runHalogen