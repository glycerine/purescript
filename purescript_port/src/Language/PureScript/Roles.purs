module Language.PureScript.Roles
  ( Role(..)
  , displayRole
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))

data Role
  = Nominal
  | Representational
  | Phantom

derive instance eqRole :: Eq Role
derive instance ordRole :: Ord Role

instance showRole :: Show Role where
  show Nominal           = "Nominal"
  show Representational  = "Representational"
  show Phantom           = "Phantom"

instance encodeJsonRole :: EncodeJson Role where
  encodeJson Nominal           = encodeJson "Nominal"
  encodeJson Representational  = encodeJson "Representational"
  encodeJson Phantom           = encodeJson "Phantom"

instance decodeJsonRole :: DecodeJson Role where
  decodeJson json = do
    s <- decodeJson json
    case s of
      "Nominal"           -> Right Nominal
      "Representational"  -> Right Representational
      "Phantom"           -> Right Phantom
      _                   -> Left $ TypeMismatch ("Role: " <> s)

displayRole :: Role -> String
displayRole Nominal           = "nominal"
displayRole Representational  = "representational"
displayRole Phantom           = "phantom"
