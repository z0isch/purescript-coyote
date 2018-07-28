module Web.Firebase where

import Prelude

import Control.Monad.State (execStateT)
import Control.Promise (Promise, toAff)
import Coyote.Simple as Simple
import Coyote.Web.Simple as SimpleWeb
import Coyote.Web.Types (CoyoteCookie, WebGame)
import Data.Array as A
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn4, runFn2, runFn4)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log, logShow)
import Foreign (Foreign)
import Simple.JSON (read, write)

foreign import _new :: Fn2 String Foreign (Effect Unit)
foreign import _subscribe :: Fn2 String (Foreign -> Effect Unit) (Effect Unit)
foreign import _update :: forall a b. Fn4 String (Foreign -> Effect Foreign) (a -> Either a b) (b -> Either a b) (Effect (Promise (Either Unit Unit)))
foreign import _get :: String -> (Effect (Promise Foreign))

--subscribeToGame :: forall s. GameId -> (WebGame s -> Effect Unit) -> Effect Unit
subscribeToGame gId toWebGame f = runFn2 _subscribe gId \msg -> do
  case read msg of
    Left err -> error $ "Can't decode json webgame with: " <> show err
    Right game -> f (toWebGame game)

--newGame :: forall s. GameId -> WebGame s -> Effect Unit
newGame gId s fromWebGame = runFn2 _new gId (write (fromWebGame s))

-- | Send an update to firebase. If the game is out of date it will run the `Aff` that you provide with the current state.
--updateGame :: forall s. GameId -> WebGame s -> (WebGame s -> Aff Unit) -> Aff Unit
updateGame gId f fromWebgame toWebGame = do
  promise <- liftEffect $ runFn4 _update gId f Left Right
  toAff promise

getGame id toWebGame = do
  json <- liftEffect (_get id) >>= toAff
  case read json of
    Left err -> do
      error $ "Can't decode json webgame with: " <> show err
      pure Nothing
    Right game -> pure $ Just $ toWebGame game

tryMakeMove :: CoyoteCookie -> Simple.Move -> Aff Unit
tryMakeMove c mv = updateGame c.id foo SimpleWeb.fromWebGame SimpleWeb.toWebGame >>= case _ of
  Left _ -> tryMakeMove c mv
  Right _ -> pure unit
  where
    foo json = case read json of
      Left err -> do
        error $ "Can't decode with: "<> show err
        pure json
      Right g -> do
        let game = SimpleWeb.toWebGame g
        newState <- liftEffect $ execStateT (Simple.makeMove mv) game.state
        let nextGame = game{state= newState}
        pure $ write (SimpleWeb.fromWebGame nextGame)

callCoyote :: CoyoteCookie -> Aff Unit
callCoyote c = tryMakeMove c (Simple.Coyote)

drawCard :: CoyoteCookie -> Aff Unit
drawCard c = getGame c.id SimpleWeb.toWebGame >>= case _ of
  Nothing -> error "Can't find that game!"
  Just game -> do
    case (Map.lookup c.userId game.playerMap) of
      Nothing -> error "Player not in game!"
      Just pl -> tryMakeMove c (Simple.DrawCard pl)
  
joinGame :: CoyoteCookie -> Aff Unit
joinGame c = updateGame c.id foo SimpleWeb.fromWebGame SimpleWeb.toWebGame >>= case _ of
  Left _ -> joinGame c
  Right _ -> pure unit   
  where
    foo json = case read json of
      Left err -> do
        error $ "Can't decode with: "<> show err
        pure json
      Right g -> do
        let game = SimpleWeb.toWebGame g
            nextGame = game
              { state= Simple.addPlayer game.state
              , playerMap= Map.insert c.userId (Map.size (game.state.players)) game.playerMap
              }
        pure $ write (SimpleWeb.fromWebGame nextGame)

foreign import test1 :: Foreign