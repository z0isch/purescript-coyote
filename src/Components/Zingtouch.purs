module Components.Zingtouch where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

type Input = Unit

type State = Input

data Query a 
  = Initialize a
  | Finalize a

data Message 
  = Bind HTMLElement
  | Unbind HTMLElement

ui :: forall m. H.Component HH.HTML Query Input Message m
ui =  H.lifecycleComponent
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Just $ H.action Finalize
    }
  where
    render :: State -> H.ComponentHTML Query
    render s = HH.div
      [ HP.class_ $ H.ClassName "zingtouch-region"
      , HP.ref (H.RefLabel "zingtouch")
      ]
      []

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Initialize next -> do
        raiseOnEl Bind
        pure next
      Finalize next -> do
        raiseOnEl Unbind
        pure next
        where 
          raiseOnEl f = do
            H.getHTMLElementRef (H.RefLabel "zingtouch") >>= case _ of
              Nothing -> pure unit
              Just el -> H.raise $ f el