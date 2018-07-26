module Main where

import Prelude

import Components.Main as M
import Components.Zingtouch as Z
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Coyote.Types (addPlayer, initialGame)
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
import Simple.JSON (read, readJSON, writeJSON)
import Web.Cookies (deleteCookie, getCookie, setCookie)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Firebase (getGame, newGame, subscribeToGame, updateGame)
import Web.HTML (window)
import Web.HTML.Location (href)
import Web.HTML.Window (location)
import Web.Zingtouch (bindPan, unbind)

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

subToGame :: Sub -> (M.Query ~> Aff) -> String -> Aff Unit
subToGame sub query id = withLock sub.lock $ do
  killSub sub.fiber
  fiber <- H.liftEffect $ launchAff $ CR.runProcess (firebaseProducer id CR.$$ firebaseConsumer query)
  H.liftEffect $ R.write (Just fiber) sub.fiber

killSub :: Ref (Maybe (Fiber Unit)) -> Aff Unit
killSub fiber = H.liftEffect (R.read fiber) >>= case _ of
  Nothing -> pure unit
  Just f -> killFiber (error "Unsubscribing to game") f

firebaseProducer :: GameId -> CR.Producer WebGame Aff Unit
firebaseProducer gId = CRA.produce $ subscribeToGame gId <<< CRA.emit

firebaseConsumer :: (M.Query ~> Aff) -> CR.Consumer WebGame Aff Unit
firebaseConsumer query = CR.consumer \game -> do
  query $ H.action $ M.GameUpdate game
  pure Nothing

processMsgs :: forall a. Sub -> String ->  (M.Query ~> Aff) -> M.Message -> Aff (Maybe a)
processMsgs sub baseUrl query = case _ of
  M.UnsubscribeFromGame -> do
    H.liftEffect $ deleteCookie cookieName
    withLock sub.lock (killSub sub.fiber)
    query $ H.action $ M.HandleInput {cookie: Nothing, baseUrl}
    pure Nothing

  M.CreateNewGame -> do
    c <- H.liftEffect $ (\i1 i2 -> {id:show i1,userId: show i2}) <$> genUUID <*> genUUID
    state <- H.liftEffect $ initialGame
    stateHash <- H.liftEffect $ show <$> genUUID
    H.liftEffect $ newGame c.id 
      { state
      , playerMap: mempty
      , stateHash
      }
    H.liftEffect $ joinGame c do
      H.liftEffect $ setCookie cookieName (writeJSON c) Nothing
      launchAff_ do
        query $ H.action $ M.HandleInput {cookie: Just c, baseUrl}
        subToGame sub query c.id
    pure Nothing

  M.ZingtouchMessage e -> case e of
    Z.Bind el -> do
      H.liftEffect $ bindPan el \f -> case (read f) of
        Left err -> Console.logShow err *> pure unit
        Right (evt :: {detail :: {data :: Array {distance :: Number}}}) -> Console.logShow evt
      pure Nothing
    Z.Unbind el -> H.liftEffect (unbind el) *> pure Nothing

joinGame :: CoyoteCookie -> Effect Unit -> Effect Unit
joinGame c f = runAff_ (const f) $ getGame c.id >>= case _ of
  Nothing -> Console.error "Can't find that game!"
  Just game -> tryJoinGame game
  where
    tryJoinGame game = updateGame c.id newGame tryJoinGame
      where 
        newGame = game
            { state= addPlayer game.state
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
        io <- runUI M.ui {cookie,baseUrl} el
    
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

joinAndSetCookie :: String -> Effect Unit
joinAndSetCookie gId = do
  userId <- show <$> genUUID
  let c = {id: gId, userId}
  joinGame c do
    setCookie cookieName (writeJSON c) Nothing
    runHalogen