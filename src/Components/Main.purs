module Components.Main where

import Prelude

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
import Components.QRCode as QRCode

cookieName :: String
cookieName = "coyote-game"

type State = 
  { cookie :: Maybe CoyoteCookie
  , game :: Maybe WebGame
  , showingHand :: Boolean
  }
  
data Query a 
  = Initialize a
  | NewGameClick a
  | GameUpdate WebGame a
  | ExitGame a
  | ToggleHand a

type Input = Unit

data Message 
  = SubscribeToGame CoyoteCookie
  | PushGameUpdate CoyoteCookie WebGame
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
      }

    render :: State -> H.ParentHTML Query (QRCode.Query) Slot Aff
    render s = HH.div 
      [HP.classes [H.ClassName "container-fluid"]] $
      [ HH.p_ 
        [ HH.pre_ [HH.text $ show s]
        ]
      ] <> 
      [ fromMaybe notInGame (inGame s.showingHand <$> s.cookie <*> s.game) ]

    notInGame = HH.p_
      [ HH.button 
        [ HE.onClick $ HE.input_ NewGameClick] 
        [ HH.text "Start new game"]
      ]
      
    inGame showingHand {userId,id} {playerMap,state} = case M.lookup userId playerMap of 
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
            , HH.a
              [ HP.href $ "#join/" <> id]
              [ HH.slot unit QRCode.ui {text: "#join/" <> id} absurd ]
            ]
          else []
          
          
    eval :: Query ~> H.ParentDSL State Query (QRCode.Query) Slot Message Aff
    eval = case _ of
      Initialize next -> do
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
        H.raise $ PushGameUpdate cookie
          { id: cookie.id
          , state: gs
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
            H.modify_ $ const initialState
        pure next
      
      ToggleHand next -> do
        H.modify_ \s -> s{showingHand= not s.showingHand}
        pure next

      GameUpdate g next -> do
        H.modify_ _{game= Just g}
        pure next