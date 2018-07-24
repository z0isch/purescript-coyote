module Components.Main where

import Prelude

import Components.QRCode as QRCode
import Coyote.Types (addPlayer, initialGame)
import Coyote.Web.Types (WebGame, CoyoteCookie)
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID (genUUID)
import Effect.Aff (Aff)
import Effect.Class.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON (readJSON, writeJSON)
import Web.Cookies (deleteCookie, getCookie, setCookie)
import Web.HTML (window)
import Web.HTML.Location (href)
import Web.HTML.Window (location)

cookieName :: String
cookieName = "coyote-game"

type State = 
  { cookie :: Maybe CoyoteCookie
  , game :: Maybe WebGame
  , showingHand :: Boolean
  , baseUrl :: String
  , updatedOldGameState :: Boolean
  }
  
data Query a 
  = Initialize a
  | NewGameClick a
  | GameUpdate WebGame a
  | ExitGame a
  | ToggleHand a
  | UpdatedOldGameState a
  | DismissUpdatedOldGameState a

type Input = Unit

data Message 
  = SubscribeToGame CoyoteCookie
  | PushNewGame CoyoteCookie WebGame
  | UnsubscribeFromGame CoyoteCookie

type Slot = Unit

ui :: H.Component HH.HTML Query Input Message Aff
ui = H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where
    initialState :: State
    initialState = 
      { cookie: Nothing
      , game: Nothing
      , showingHand: false
      , baseUrl: ""
      , updatedOldGameState: false
      }

    render :: State -> H.ParentHTML Query (QRCode.Query) Slot Aff
    render s = HH.div 
      [HP.classes [H.ClassName "container-fluid"]] $
      [ HH.p_ 
        [ HH.pre_ [HH.text $ show s]
        ]
      ] <> 
      [ fromMaybe notInGame (inGame s.showingHand s.baseUrl <$> s.cookie <*> s.game) ]

    notInGame = HH.p_
      [ HH.button 
        [ HE.onClick $ HE.input_ NewGameClick] 
        [ HH.text "Start new game"]
      ]
      
    inGame showingHand baseUrl {userId,id} {playerMap,state} = case M.lookup userId playerMap of 
      Nothing -> HH.p_ []
      Just player -> HH.p_ $
        [ HH.button 
            [ HE.onClick $ HE.input_ ToggleHand] 
            [ HH.text "Toggle hand"]
          , HH.button 
            [ HE.onClick $ HE.input_ ExitGame] 
            [ HH.text "Exit the game"]
        ] <> if showingHand 
          then 
            [ HH.text $ show state
            ]
          else 
            [ HH.a
              [ HP.href $ baseUrl <> "#join/" <> id]
              [ HH.slot unit QRCode.ui (baseUrl <> "#join/" <> id) absurd ]
            ]
          
          
    eval :: Query ~> H.ParentDSL State Query (QRCode.Query) Slot Message Aff
    eval = case _ of
      Initialize next -> do
        loc <- H.liftEffect $ window >>= location >>= href
        H.modify_ _{baseUrl= loc}
        H.liftEffect (getCookie cookieName) >>= case _ of
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
        H.liftEffect $ setCookie cookieName (writeJSON cookie) Nothing
        H.modify_ _{cookie= Just cookie, game= Nothing}
        H.raise $ SubscribeToGame cookie
        gs <- addPlayer <$> H.liftEffect initialGame
        stateHash <- H.liftEffect $ show <$> genUUID
        H.raise $ PushNewGame cookie
          { state: gs
          , playerMap: M.singleton cookie.userId 0
          , stateHash
          }
        pure next
        
      ExitGame next -> do
        {cookie} <- H.get
        case cookie of
          Nothing -> pure unit
          Just c -> do
            H.liftEffect $ deleteCookie cookieName
            H.raise $ UnsubscribeFromGame c
            H.modify_ _
              { cookie= Nothing
              , game= Nothing
              , showingHand= false
              }
        pure next
      
      ToggleHand next -> do
        H.modify_ \s -> s{showingHand= not s.showingHand}
        pure next

      GameUpdate g next -> do
        H.modify_ _{game= Just g}
        pure next

      UpdatedOldGameState next -> do
        H.modify_ _{updatedOldGameState= true}
        pure next
        
      DismissUpdatedOldGameState next -> do
        H.modify_ _{updatedOldGameState= false}
        pure next