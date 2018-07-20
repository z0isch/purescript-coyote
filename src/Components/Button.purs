module Components.Button where

import Prelude

import Data.DateTime (diff)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, Milliseconds)
import Effect.Now (now)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.TouchEvent.TouchEvent as TE

type State = 
  { status :: Boolean
  , clickStart :: Maybe Instant
  , clickEnd :: Maybe Instant
  }

data Query a
  = Toggle a
  | IsOn (Boolean -> a)
  | MDown a
  | MUp a
  | PreventDefault Event (Query a) 

data Message = Toggled Boolean

myButton :: H.Component HH.HTML Query Unit Message Aff
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = {status: false, clickStart: Nothing, clickEnd: Nothing}

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state.status then "On" else "Off"
    in
      HH.div_ 
      [ HH.text $ maybe "Nope" show ((diff <$> map toDateTime state.clickEnd <*> map toDateTime state.clickStart) :: Maybe Milliseconds)
      , HH.p_
        [ HH.button
          [ HP.title label
          , HE.onClick (HE.input_ Toggle)
          , HE.onMouseDown (HE.input_ MDown)
          , HE.onMouseLeave (HE.input_ MUp)
          , HE.onMouseUp (HE.input_ MUp)
          , HE.onTouchStart \e -> Just $ PreventDefault (TE.toEvent e) $ H.action MDown
          , HE.onTouchCancel \e -> Just $ PreventDefault (TE.toEvent e) $ H.action MUp
          , HE.onTouchLeave \e -> Just $ PreventDefault (TE.toEvent e) $ H.action MUp
          , HE.onTouchEnd \e -> Just $ PreventDefault (TE.toEvent e) $ H.action MUp
          ]
          [ HH.text label ]
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval = case _ of
    PreventDefault ev q -> do
      liftEffect $ preventDefault ev 
      eval q
    Toggle next -> do
      s <- H.get
      let nextState = s{status= not s.status}
      H.put nextState
      H.raise $ Toggled nextState.status
      pure next
    MDown next -> do
      n <- liftEffect now
      H.modify_ _{clickStart= Just n, clickEnd= Nothing}
      pure next
    MUp next -> do
      n <- liftEffect now
      H.modify_ _{clickEnd= Just n}
      pure next
    IsOn reply -> do
      {status} <- H.get
      pure (reply status)