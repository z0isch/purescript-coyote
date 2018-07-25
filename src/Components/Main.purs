module Components.Main where

import Prelude

import Components.QRCode as QRCode
import Coyote.Web.Types (WebGame, CoyoteCookie)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

cookieName :: String
cookieName = "coyote-game"

type State = 
  { input :: Input
  , game :: Maybe WebGame
  , showingHand :: Boolean
  , updatedOldGameState :: Boolean
  }
  
data Query a 
  = NewGameClick a
  | GameUpdate WebGame a
  | ExitGame a
  | ToggleHand a
  | UpdatedOldGameState a
  | DismissUpdatedOldGameState a
  | HandleInput Input a

type Input = 
  { cookie :: Maybe CoyoteCookie
  , baseUrl :: String
  }

data Message 
  = SubscribeToGame CoyoteCookie
  | UnsubscribeFromGame
  | CreateNewGame

type Slot = Unit

ui :: H.Component HH.HTML Query Input Message Aff
ui = H.parentComponent
  { initialState
  , render
  , eval
  , receiver: HE.input HandleInput
  }
  where
    initialState :: Input -> State
    initialState input = 
      { input
      , game: Nothing
      , showingHand: false
      , updatedOldGameState: false
      }

    render :: State -> H.ParentHTML Query (QRCode.Query) Slot Aff
    render s = HH.div 
      [HP.classes [H.ClassName "container-fluid"]] $
      [ HH.p_ 
        [ HH.pre_ [HH.text $ show s]
        ]
      ] <> 
      [ fromMaybe notInGame (inGame s.showingHand s.input.baseUrl <$> s.input.cookie <*> s.game) ]

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

      HandleInput i next -> do
        H.modify_ _{input= i}
        pure next

      NewGameClick next -> do
        H.raise CreateNewGame
        pure next
        
      ExitGame next -> do
        H.raise UnsubscribeFromGame
        H.modify_ _{game= Nothing}
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