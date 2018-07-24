module Components.QRCode where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.QRCode (insertQRCode)

type Input = String

type State = Input

data Query a 
  = Initialize a
  | HandleInput Input a

type Message = Void

ui :: H.Component HH.HTML Query Input Message Aff
ui =  H.lifecycleComponent
    { initialState: identity
    , render
    , eval
    , receiver: HE.input HandleInput
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
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
      HandleInput i next -> do
        H.put i
        setHtml
        pure next
        where 
          setHtml = do
            t <- H.get
            H.getHTMLElementRef (H.RefLabel "qr-code") >>= case _ of
              Nothing -> pure unit
              Just el -> H.liftEffect $ insertQRCode el t
