module Language.PureScript.Label
  ( Label(..)
  , runLabel
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Language.PureScript.PSString (PSString, mkString)

-- | Labels are used as record keys and row entry names.
-- | Labels are PSStrings because records are indexed by PureScript strings at runtime.
newtype Label = Label PSString

runLabel :: Label -> PSString
runLabel (Label ps) = ps

derive instance eqLabel :: Eq Label
derive instance ordLabel :: Ord Label

instance showLabel :: Show Label where
  show (Label ps) = "(Label " <> show ps <> ")"

instance encodeJsonLabel :: EncodeJson Label where
  encodeJson (Label ps) = encodeJson ps

instance decodeJsonLabel :: DecodeJson Label where
  decodeJson json = Label <$> decodeJson json
