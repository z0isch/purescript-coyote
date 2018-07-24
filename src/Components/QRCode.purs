module Components.QRCode where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.QRCode (insertQRCode)

type Input = 
  { text :: String
  }

type State = 
  { input :: Input
  }

data Query a 
  = Initialize a
  | Finalize a
  | HandleInput Input a

type Message = Void

ui :: H.Component HH.HTML Query Input Message Aff
ui =  H.lifecycleComponent
    { initialState: \input -> {input}
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: HE.input HandleInput
    }
  where
    render :: State -> H.ComponentHTML Query
    render s = HH.a 
      [ HP.ref (H.RefLabel "qr-code") ]
      []

    eval :: Query ~> H.ComponentDSL State Query Message Aff
    eval = case _ of
      Initialize next -> do
        setHtml
        pure next
      Finalize next -> pure next
      HandleInput i next -> do
        H.modify_ _{ input = i }
        setHtml
        pure next
      where 
        setHtml = H.getHTMLElementRef (H.RefLabel "qr-code") >>= case _ of
          Nothing -> pure unit
          Just el -> H.liftEffect $ insertQRCode el "yeah" 
