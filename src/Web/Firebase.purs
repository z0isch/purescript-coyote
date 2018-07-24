module Web.Firebase where

import Prelude

import Control.Promise (Promise, toAff)
import Coyote.Web.Types (GameId, WebGame, fromWebGame, toWebGame)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Foreign (Foreign)
import Simple.JSON (read, write)

foreign import _new :: Fn2 String Foreign (Effect Unit)
foreign import _subscribe :: Fn2 String (Foreign -> Effect Unit) (Effect Unit)
foreign import _update :: Fn2 String Foreign (Effect (Promise Unit))
foreign import _get :: String -> (Effect (Promise Foreign))

subscribeToGame :: GameId -> (WebGame -> Effect Unit) -> Effect Unit
subscribeToGame gId f = runFn2 _subscribe gId \msg -> do
  case read msg of
    Left err -> error $ "Can't decode json webgame with: " <> show err
    Right game -> f (toWebGame game)

newGame :: GameId -> WebGame -> Effect Unit
newGame gId s = runFn2 _new gId (write (fromWebGame s))

updateGame :: GameId -> WebGame -> Aff Unit -> Aff Unit
updateGame gId s f = do
  promise <- liftEffect (runFn2 _update gId (write (fromWebGame s))) 
  attempt (toAff promise) >>= case _ of
    Left _ -> f
    Right _ -> pure unit

getGame :: GameId -> Aff (Maybe WebGame)
getGame id = do
  json <- liftEffect (_get id) >>= toAff
  case read json of
    Left err -> do
      error $ "Can't decode json webgame with: " <> show err
      pure Nothing
    Right game -> pure $ Just $ toWebGame game