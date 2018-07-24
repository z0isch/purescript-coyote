module Web.Firebase where

import Prelude

import Control.Promise (Promise, toAff)
import Coyote.Web.Types (GameId, WebGame, fromWebGame, toWebGame)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (error)
import Foreign (Foreign)
import Simple.JSON (read, write)

foreign import _subscribe :: Fn2 String (Foreign -> Effect Unit) (Effect Unit)
foreign import _write :: Fn2 String Foreign (Effect Unit)
foreign import _get :: String -> Promise Foreign

subscribeToGame :: GameId -> (WebGame -> Effect Unit) -> Effect Unit
subscribeToGame gId f = runFn2 _subscribe gId \msg -> do
  case read msg of
    Left err -> error $ "Can't decode json webgame with: " <> show err
    Right game -> f (toWebGame game)

writeGame :: GameId -> WebGame -> Effect Unit
writeGame gId s = runFn2 _write gId (write (fromWebGame s))

getGame :: GameId -> Aff (Maybe WebGame)
getGame id = do
  json <- toAff (_get id) 
  case read json of
    Left err -> pure Nothing
    Right game -> pure $ Just $ toWebGame game