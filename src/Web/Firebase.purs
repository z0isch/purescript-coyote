module Web.Firebase where

import Prelude

import Control.Monad.State (execStateT)
import Coyote.Simple as Simple
import Coyote.Web.Simple as SimpleWeb
import Coyote.Web.Types (CoyoteCookie, GameId, WebGame, WebGameDTO, StateHash)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn3, runFn1, runFn3)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, catchError, message)
import Effect.Aff.Compat (EffectFn2, EffectFnAff, fromEffectFnAff, runEffectFn2)
import Effect.Class.Console (error)
import Foreign (Foreign)
import Simple.JSON (class ReadForeign, class WriteForeign, E, read, readJSON, write)

foreign import _new :: EffectFn2 String Foreign Unit
foreign import _subscribe :: EffectFn2 String (Foreign -> Effect Unit) Unit
foreign import _update :: Fn3 String Int (Foreign -> Effect Foreign) (EffectFnAff Foreign)
foreign import _get :: Fn1 String (EffectFnAff Foreign)

subscribeToGame :: forall s. ReadForeign s => GameId -> (WebGameDTO s -> Effect Unit) -> Effect Unit
subscribeToGame gId f = runEffectFn2 _subscribe gId \msg -> do
  case read msg of
    Left err -> error $ "Can't decode json webgame with: " <> show err
    Right game -> f game

newGame :: forall s. WriteForeign s => GameId -> WebGameDTO s -> Effect Unit
newGame gId s = runEffectFn2 _new gId (write s)

updateGame :: forall s. ReadForeign s => WriteForeign s => GameId -> StateHash -> (WebGameDTO s -> Effect (WebGameDTO s)) -> Aff (E (WebGameDTO s))
updateGame gId stateHash f = read <$> fromEffectFnAff (runFn3 _update gId stateHash gameUpdate)
  where
    gameUpdate json = case read json of
      Left err -> do
        error $ "Can't decode with: "<> show err
        pure json
      Right g -> write <$> f g 

getGame :: forall s. ReadForeign s => GameId -> Aff (Maybe (WebGameDTO s))
getGame id = do
  json <- fromEffectFnAff $ runFn1 _get id
  case read json of
    Left err -> do
      error $ "Can't decode json webgame with: " <> show err
      pure Nothing
    Right game -> pure $ Just game

tryMakeMove :: CoyoteCookie -> WebGame (Simple.GameState) -> Simple.Move -> Aff Unit
tryMakeMove c g@{stateHash} mv = catchError (updateGame c.id stateHash foo >>= bar) retryWithCurr
  where
    retryWithCurr json = case readJSON (message json) of
      Left err -> error $ "Can't decode json webgame with: " <> show err
      Right dto -> tryMakeMove c (SimpleWeb.toWebGame dto) mv
    bar = case _ of
      Left err -> error $ "Can't decode json webgame with: " <> show err
      Right _ -> pure unit
    foo dto = do
      let game = SimpleWeb.toWebGame dto
      newState <- execStateT (Simple.makeMove mv) game.state
      let nextGame = game{state= newState}
      pure $ SimpleWeb.fromWebGame nextGame

callCoyote :: CoyoteCookie -> WebGame (Simple.GameState) -> Aff Unit
callCoyote c g = tryMakeMove c g (Simple.Coyote)

drawCard :: CoyoteCookie -> WebGame (Simple.GameState) -> Aff Unit
drawCard c g@{stateHash,playerMap}= case (Map.lookup c.userId playerMap) of
  Nothing -> error "Player not in game!"
  Just pl -> tryMakeMove c g (Simple.DrawCard pl)
  
joinGame :: CoyoteCookie -> WebGame (Simple.GameState) -> Aff Unit
joinGame c {stateHash} = catchError (updateGame c.id stateHash foo >>= bar) retryWithCurr
  where
    retryWithCurr json = case readJSON (message json) of
      Left err -> error $ "Can't decode json webgame with: " <> show err
      Right dto -> joinGame c (SimpleWeb.toWebGame dto)
    bar = case _ of
      Left err -> error $ "Can't decode json webgame with: " <> show err
      Right _ -> pure unit
    foo dto = do
      let game = SimpleWeb.toWebGame dto
          nextGame = game
            { state= Simple.addPlayer game.state
            , playerMap= Map.insert c.userId (Map.size (game.state.players)) game.playerMap
            }
      pure $ SimpleWeb.fromWebGame nextGame