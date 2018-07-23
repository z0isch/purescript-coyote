module Coyote.UntaggedSumRep where

import Prelude

import Control.Alt ((<|>))

import Data.Generic.Rep (Argument(..), Constructor(..), Sum(..))
import Foreign (F, Foreign)
import Simple.JSON (class ReadForeign, readImpl)

class UntaggedSumRep rep where
  untaggedSumRep :: Foreign -> F rep

instance untaggedSumRepSum ::
  ( UntaggedSumRep a
  , UntaggedSumRep b
  ) => UntaggedSumRep (Sum a b) where
  untaggedSumRep f
      = Inl <$> untaggedSumRep f
    <|> Inr <$> untaggedSumRep f

instance untaggedSumRepArgument ::
  ( ReadForeign a
  ) => UntaggedSumRep (Argument a) where
  untaggedSumRep f = Argument <$> readImpl f

instance untaggedSumRepConstructor ::
  ( UntaggedSumRep (Argument a)
  ) => UntaggedSumRep (Constructor name (Argument a)) where
  untaggedSumRep f = Constructor <$> untaggedSumRep f