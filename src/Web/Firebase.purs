module Web.Firebase where

import Prelude

import Control.Promise (Promise, toAff)
import Coyote.Web.Types (GameId, WebGame)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn4, runFn2, runFn4)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Foreign (Foreign)
import Simple.JSON (read, write)

foreign import _new :: Fn2 String Foreign (Effect Unit)
foreign import _subscribe :: Fn2 String (Foreign -> Effect Unit) (Effect Unit)
foreign import _update :: forall a b. Fn4 String Foreign (a -> Either a b) (b -> Either a b) (Effect (Promise (Either Foreign Unit)))
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
updateGame gId s fromWebgame toWebGame f = do
  promise <- liftEffect $ runFn4 _update gId (write (fromWebgame s)) Left Right
  toAff promise >>= case _ of
    Left json -> case read json of
      Left err -> error $ "Can't decode json webgame with: " <> show err
      Right game -> f $ toWebGame game
    Right _ -> pure unit

getGame id toWebGame = do
  json <- liftEffect (_get id) >>= toAff
  case read json of
    Left err -> do
      error $ "Can't decode json webgame with: " <> show err
      pure Nothing
    Right game -> pure $ Just $ toWebGame game