module Test.Main where

import Prelude

import Control.Parallel (parallel, sequential)
import Coyote.Simple as Simple
import Coyote.Web.Simple as SimpleWeb
import Data.Array as A
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Web.Firebase as Firebase

main :: Effect Unit
main = do
  runTest do
    suite "Race Condition" do
      test "Add to game then draw card" do
        id <- liftEffect $ show <$> genUUID
        userId <- liftEffect $ show <$> genUUID
        userId2 <- liftEffect $ show <$> genUUID
        userId3 <- liftEffect $ show <$> genUUID

        let cookie = {id,userId}
            cookie2 = {id,userId:userId2}
            cookie3 = {id,userId:userId3}
        state <- liftEffect $ Simple.initialGame
        let webGame = {state,playerMap:mempty,stateHash:0}
        liftEffect $ Firebase.newGame cookie.id (SimpleWeb.fromWebGame webGame)
        sequential $ parallel (Firebase.joinGame cookie webGame) *> parallel (Firebase.joinGame cookie2 webGame) *> parallel (Firebase.joinGame cookie3 webGame)
        Firebase.getGame cookie.id >>= case _ of
          Nothing -> pure unit
          Just dto -> do
            let webGame' = SimpleWeb.toWebGame dto
            sequential $ parallel (Firebase.drawCard cookie2 webGame') *> parallel (Firebase.drawCard cookie webGame') *> parallel (Firebase.drawCard cookie3 webGame')
        g <- map SimpleWeb.toWebGame <$> Firebase.getGame cookie.id    
        Assert.assert "Should be three in the game" $ 
          (map (M.size <<< _.playerMap) g) == Just 3
        Assert.assert "Should all have cards" $
          (map (M.size <<< M.filter (\{hand} -> not $ A.null hand) <<< _.state.players) g) == Just 3