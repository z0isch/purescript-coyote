module Web.Firebase where

import Prelude

import Control.Monad.State (execStateT)
import Coyote.Simple as Simple
import Coyote.Web.Simple as SimpleWeb
import Coyote.Web.Types (CoyoteCookie, GameId, WebGame, WebGameDTO, StateHash)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn5, runFn1, runFn5)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn2, EffectFnAff, fromEffectFnAff, runEffectFn2)
import Effect.Class.Console (error)
import Foreign (Foreign)
import Simple.JSON (class ReadForeign, class WriteForeign, E, read, write)

foreign import _new :: EffectFn2 String Foreign Unit
foreign import _subscribe :: EffectFn2 String (Foreign -> Effect Unit) Unit
foreign import _update :: ∀ a b. Fn5 (a -> Either a b) (b -> Either a b) String Int (Foreign -> Effect Foreign) (EffectFnAff (Either Foreign Foreign))
foreign import _get :: Fn1 String (EffectFnAff Foreign)

subscribeToGame :: ∀ s. ReadForeign s => GameId -> (WebGameDTO s -> Effect Unit) -> Effect Unit
subscribeToGame gId f = runEffectFn2 _subscribe gId \msg -> do
  case read msg of
    Left err -> error $ "Can't decode json webgame with: " <> show err
    Right game -> f game

newGame :: ∀ s. WriteForeign s => GameId -> WebGameDTO s -> Effect Unit
newGame gId s = runEffectFn2 _new gId (write s)

updateGame :: ∀ s. ReadForeign s => WriteForeign s => GameId -> StateHash -> (WebGameDTO s -> Effect (WebGameDTO s)) -> Aff (Either (E (WebGameDTO s)) (E (WebGameDTO s)))
updateGame gId stateHash f = bimap read read <$> fromEffectFnAff (runFn5 _update Left Right gId stateHash gameUpdate)
  where
    gameUpdate json = case read json of
      Left err -> do
        error $ "Can't decode with: "<> show err
        pure json
      Right g -> write <$> f g 

getGame :: ∀ s. ReadForeign s => GameId -> Aff (Maybe (WebGameDTO s))
getGame id = do
  json <- fromEffectFnAff $ runFn1 _get id
  case read json of
    Left err -> do
      error $ "Can't decode json webgame with: " <> show err
      pure Nothing
    Right game -> pure $ Just game

updateWithRetry :: CoyoteCookie -> StateHash -> (WebGame (Simple.GameState) -> Effect (WebGame (Simple.GameState))) -> Aff Unit
updateWithRetry c stateHash f = updateGame c.id stateHash g >>= retryWithCurr
  where
    g = map SimpleWeb.fromWebGame <<< f <<< SimpleWeb.toWebGame
    retryWithCurr = case _ of
      Left (Left err) -> error $ "Can't decode json webgame with: " <> show err
      Right (Left err) -> error $ "Can't decode json webgame with: " <> show err
      Left (Right {stateHash:stateHash'}) -> updateWithRetry c stateHash' f
      Right _ -> pure unit

makeMoveWithRetry :: CoyoteCookie -> WebGame (Simple.GameState) -> Simple.Move -> Aff Unit
makeMoveWithRetry c {stateHash} mv = updateWithRetry c stateHash mkMove
  where
    mkMove game = do
      newState <- execStateT (Simple.makeMove mv) game.state
      pure game{state= newState}

callCoyote :: CoyoteCookie -> WebGame (Simple.GameState) -> Aff Unit
callCoyote c g = makeMoveWithRetry c g Simple.Coyote

drawCard :: CoyoteCookie -> WebGame (Simple.GameState) -> Aff Unit
drawCard c g@{stateHash,playerMap} = case (Map.lookup c.userId playerMap) of
  Nothing -> error "Player not in game!"
  Just pl -> makeMoveWithRetry c g (Simple.DrawCard pl)
  
joinGame :: CoyoteCookie -> WebGame (Simple.GameState) -> Aff Unit
joinGame c {stateHash} = updateWithRetry c stateHash joinIt
  where
    joinIt game = pure game
      { state= Simple.addPlayer game.state
      , playerMap= Map.insert c.userId (Map.size (game.state.players)) game.playerMap
      }
      