module Components.Main where

import Prelude

import Coyote.Types (addPlayer, initialGame)
import Coyote.Web.Types (WebGame, CoyoteCookie)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect.Aff (Aff)
import Effect.Class.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Simple.JSON (readJSON, writeJSON)
import Web.Cookies (getCookie, setCookie)

type State = 
  { cookie :: Maybe CoyoteCookie
  , game :: Maybe WebGame
  }
  
data Query a 
  = Initialize a
  | NewGameClick a
  | GameUpdate WebGame a

type Input = Unit

data Message 
  = SubscribeToGame CoyoteCookie
  | PushGameUpdate CoyoteCookie WebGame

type Slot = Void

ui :: H.Component HH.HTML Query Input Message Aff
ui = H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where
    initialState :: Input -> State
    initialState _ = 
      { cookie: Nothing
      , game: Nothing
      }

    render :: State -> H.ParentHTML Query Identity Slot Aff
    render s = HH.div_ 
      [ HH.p_ 
        [ HH.text $ show s
        ]
      , HH.p_
        [ HH.button 
          [ HE.onClick $ HE.input_ NewGameClick] 
          [ HH.text "Start new game"]
        ]
      ]
    
    eval :: Query ~> H.ParentDSL State Query Identity Slot Message Aff
    eval = case _ of
      Initialize next -> do
        H.liftEffect (getCookie "coyote-game") >>= case _ of
          Nothing -> pure unit
          Just cookieE -> do
            case readJSON cookieE of
              Left err -> do
                H.liftEffect $ error $ "Can't parse cookie: "<> show err
              Right cookie -> do
                H.modify_ _{cookie= Just cookie}
                H.raise $ SubscribeToGame cookie
        pure next

      NewGameClick next -> do
        cookie <- H.liftEffect $ (\i1 i2 -> {id:show i1,userId: show i2}) <$> genUUID <*> genUUID
        H.liftEffect $ setCookie "coyote-game" (writeJSON cookie) Nothing
        H.modify_ _{cookie= Just cookie, game= Nothing}
        H.raise $ SubscribeToGame cookie
        gs <- addPlayer <$> H.liftEffect initialGame
        H.raise $ PushGameUpdate cookie
          { id: cookie.id
          , state: gs
          , playerMap: M.singleton cookie.userId 0
          }
        pure next

      GameUpdate g next -> do
        H.modify_ _{game= Just g}
        pure next