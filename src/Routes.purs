module Routes where

import Prelude

import Control.Alt ((<|>))
import Coyote.Web.Types (GameId)
import Routing.Match (Match, lit, str)

data Routes 
  = Home
  | Join GameId

myRoute :: Match Routes
myRoute 
  =   Join <$> (lit "join" *> str)
  <|> pure Home