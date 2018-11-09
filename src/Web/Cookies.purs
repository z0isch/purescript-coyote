module Web.Cookies (
           setCookie
         , getCookie
         , deleteCookie
         , CookiesOptions
         , path
         , domain
         , expires
         , secure
         ) where

import Data.JSDate (JSDate)
import Data.Options (Option, Options, opt)
import Prelude (Unit, bind, pure, unit, ($))
import Effect (Effect)
import Data.Maybe (Maybe(..))

foreign import data CookiesOptions :: Type

-- Following are the available cookies options
--

path :: Option CookiesOptions String
path = opt "path"

domain :: Option CookiesOptions String
domain = opt "domain"

expires :: Option CookiesOptions JSDate
expires = opt "expires"

secure :: Option CookiesOptions Boolean
secure = opt "secure"
       
       
foreign import _setCookie :: ∀ value opts. String -> value -> opts -> Effect Unit

foreign import _getCookie :: ∀ value. String -> Effect (Array value)

-- |  Get cookie with specified name.
getCookie :: String -> Effect (Maybe String)
getCookie key = do
    cook <- _getCookie key
    prepare cook
    where prepare [value] = pure $ Just value
          prepare _ = pure Nothing

-- |  Delete cookie with specified name.
foreign import deleteCookie :: String -> Effect Unit

-- | Set cookie with specified name and value. Last argument (opts) is a map of optional arguments such as expiration time.
setCookie :: ∀ value. String -> value -> Maybe (Options CookiesOptions) -> Effect Unit
setCookie name value Nothing = _setCookie name value unit
setCookie name value (Just opts) = _setCookie name value opts

-- | Set cookie with specified name and value. No options to the cookie are specified.
setSimpleCookie :: ∀ value. String -> value -> Effect Unit
setSimpleCookie name value = _setCookie name value unit