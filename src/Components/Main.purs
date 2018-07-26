module Components.Main where

import Prelude

import Components.QRCode as QRCode
import Components.Zingtouch as Zingtouch
import Coyote.Web.Types (WebGame, CoyoteCookie)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (endsWith)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (liftAff)
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.NoSleep as NoSleep

type State = 
  { input :: Input
  , game :: Maybe WebGame
  , showingHand :: Boolean
  , updatedOldGameState :: Boolean
  , countdownToShowHand :: Maybe Int
  }
  
data Query a 
  = NewGameClick a
  | GameUpdate WebGame a
  | ExitGame a
  | ToggleHand a
  | UpdatedOldGameState a
  | DismissUpdatedOldGameState a
  | HandleInput Input a
  | HandleZingtouch Zingtouch.Message a

type Input = 
  { cookie :: Maybe CoyoteCookie
  , baseUrl :: String
  }

data Message 
  = UnsubscribeFromGame
  | CreateNewGame
  | ZingtouchMessage Zingtouch.Message

type ChildQuery = Coproduct2 QRCode.Query Zingtouch.Query
type ChildSlot = Either2 Unit Unit

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
      , countdownToShowHand: Nothing
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
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
              [ HP.href url ]
              [ HH.slot' cp1 unit QRCode.ui url absurd ]
            , HH.slot' cp2 unit Zingtouch.ui unit (HE.input HandleZingtouch)
            ]
      where 
        url = let prefix = if baseUrl `endsWith` "#" then "" else "#"
              in prefix <> "join/" <> id

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
    eval = case _ of

      HandleInput i next -> do
        H.modify_ _{input= i}
        pure next

      NewGameClick next -> do
        H.raise CreateNewGame
        H.liftEffect NoSleep._enable
        pure next
        
      ExitGame next -> do
        H.raise UnsubscribeFromGame
        H.modify_ _{game= Nothing, showingHand= false}
        pure next
      
      ToggleHand next -> do
        _ <- H.fork do
          H.modify_ _{countdownToShowHand= Just 3}
          liftAff $ delay $ Milliseconds 1000.0 
          H.modify_ _{countdownToShowHand= Just 2}
          liftAff $ delay $ Milliseconds 1000.0 
          H.modify_ _{countdownToShowHand= Just 1}
          liftAff $ delay $ Milliseconds 1000.0 
          H.modify_ \s -> s{showingHand= not s.showingHand, countdownToShowHand= Nothing}
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

      HandleZingtouch msg next -> do
        H.raise $ ZingtouchMessage msg
        pure next