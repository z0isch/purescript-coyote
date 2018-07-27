module Main where

import Prelude

import Components.Simple as SimpleComponent
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.State (execState, execStateT, runStateT)
import Coyote.Simple as Simple
import Coyote.Web.Simple as SimpleWeb
import Coyote.Web.Types (CoyoteCookie, GameId, WebGame)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, error, killFiber, launchAff, launchAff_, runAff_)
import Effect.Aff.Lock (Lock, newLock, withLock)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as R
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routes (Routes(..), myRoute)
import Routing (match)
import Routing.Hash (getHash, setHash)
import Simple.JSON (readJSON, writeJSON)
import Web.Cookies (deleteCookie, getCookie, setCookie)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Firebase (getGame, newGame, subscribeToGame, updateGame)
import Web.HTML (window)
import Web.HTML.Location (href)
import Web.HTML.Window (location)

foreign import _fullScreen :: Effect Unit

type Sub = 
  { fiber :: Ref (Maybe (Fiber Unit))
  , lock :: Lock
  }

newSub :: Aff Sub
newSub = do
  fiber <- H.liftEffect (R.new Nothing)
  lock <- newLock
  pure {fiber, lock}

cookieName :: String
cookieName = "coyote-game"

subToGame :: Sub -> (SimpleComponent.Query ~> Aff) -> String -> Aff Unit
subToGame sub query id = withLock sub.lock $ do
  killSub sub.fiber
  fiber <- H.liftEffect $ launchAff $ CR.runProcess (firebaseProducer id CR.$$ firebaseConsumer query)
  H.liftEffect $ R.write (Just fiber) sub.fiber

killSub :: Ref (Maybe (Fiber Unit)) -> Aff Unit
killSub fiber = H.liftEffect (R.read fiber) >>= case _ of
  Nothing -> pure unit
  Just f -> killFiber (error "Unsubscribing to game") f

firebaseProducer :: GameId -> CR.Producer (WebGame Simple.GameState) Aff Unit
firebaseProducer gId = CRA.produce $ subscribeToGame gId SimpleWeb.toWebGame <<< CRA.emit

firebaseConsumer :: (SimpleComponent.Query ~> Aff) -> CR.Consumer (WebGame Simple.GameState) Aff Unit
firebaseConsumer query = CR.consumer \game -> do
  query $ H.action $ SimpleComponent.GameUpdate game
  pure Nothing

processMsgs :: forall a. Sub -> String ->  (SimpleComponent.Query ~> Aff) -> SimpleComponent.Message -> Aff (Maybe a)
processMsgs sub baseUrl query = case _ of
  SimpleComponent.UnsubscribeFromGame -> do
    H.liftEffect $ deleteCookie cookieName
    withLock sub.lock (killSub sub.fiber)
    query $ H.action $ SimpleComponent.HandleInput {cookie: Nothing, baseUrl}
    pure Nothing
  SimpleComponent.DrawCard c -> do
    H.liftEffect $ drawCard c
    pure Nothing
  SimpleComponent.CoyoteCall c -> do
    H.liftEffect $ callCoyote c
    pure Nothing
  SimpleComponent.CreateNewGame -> do
    c <- H.liftEffect $ (\i1 i2 -> {id:show i1,userId: show i2}) <$> genUUID <*> genUUID
    state <- H.liftEffect $ Simple.initialGame
    stateHash <- H.liftEffect $ show <$> genUUID
    H.liftEffect $ newGame c.id
      { state
      , playerMap: mempty
      , stateHash
      } SimpleWeb.fromWebGame
    H.liftEffect $ joinGame c do
      H.liftEffect $ setCookie cookieName (writeJSON c) Nothing
      launchAff_ do
        query $ H.action $ SimpleComponent.HandleInput {cookie: Just c, baseUrl}
        subToGame sub query c.id
    pure Nothing

tryMakeMove :: CoyoteCookie -> Simple.Move -> WebGame (Simple.GameState) -> Aff Unit
tryMakeMove c mv game = do
  newState <- H.liftEffect $ execStateT (Simple.makeMove mv) game.state
  updateGame c.id game{state= newState} SimpleWeb.fromWebGame SimpleWeb.toWebGame (tryMakeMove c mv)

callCoyote :: CoyoteCookie -> Effect Unit
callCoyote c = launchAff_ $ getGame c.id SimpleWeb.toWebGame >>= case _ of
  Nothing -> Console.error "Can't find that game!"
  Just game -> tryMakeMove c (Simple.Coyote) game

drawCard :: CoyoteCookie -> Effect Unit
drawCard c = launchAff_ $ getGame c.id SimpleWeb.toWebGame >>= case _ of
  Nothing -> Console.error "Can't find that game!"
  Just game -> do
    case (Map.lookup c.userId game.playerMap) of
      Nothing -> Console.error "Player not in game!"
      Just pl -> tryMakeMove c (Simple.DrawCard pl) game
  
joinGame :: CoyoteCookie -> Effect Unit -> Effect Unit
joinGame c f = runAff_ (const f) $ getGame c.id SimpleWeb.toWebGame >>= case _ of
  Nothing -> Console.error "Can't find that game!"
  Just game -> tryJoinGame game
  where
    tryJoinGame game = updateGame c.id newGame SimpleWeb.fromWebGame SimpleWeb.toWebGame tryJoinGame
      where 
        newGame = game
            { state= Simple.addPlayer game.state
            , playerMap= Map.insert c.userId (Map.size (game.state.players)) game.playerMap
            }

runHalogen :: Effect Unit
runHalogen = HA.runHalogenAff do
    sub <- newSub

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

main :: Effect Unit
main = do
  getHash >>= match myRoute >>> case _ of
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
  _fullScreen

joinAndSetCookie :: String -> Effect Unit
joinAndSetCookie gId = do
  userId <- show <$> genUUID
  let c = {id: gId, userId}
  joinGame c do
    setCookie cookieName (writeJSON c) Nothing
    runHalogen