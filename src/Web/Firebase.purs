module Web.Firebase where

import Prelude

import Coyote.Web.Types (GameId, WebGame, fromWebGame, toWebGame)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Class.Console (error)
import Foreign (Foreign)
import Simple.JSON (read, write)

foreign import _subscribe :: Fn2 String (Foreign -> Effect Unit) (Effect Unit)
foreign import _write :: Fn2 String Foreign (Effect Unit)

subscribeToGame :: GameId -> (WebGame -> Effect Unit) -> Effect Unit
subscribeToGame gId f = runFn2 _subscribe gId \msg -> do
  case read msg of
    Left err -> error $ "Can't decode json webgame with: " <> show err
    Right game -> f (toWebGame game)

writeGame :: GameId -> WebGame -> Effect Unit
writeGame gId s = runFn2 _write gId (write (fromWebGame s))