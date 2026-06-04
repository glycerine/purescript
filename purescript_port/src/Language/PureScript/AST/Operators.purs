module Language.PureScript.AST.Operators
  ( Precedence
  , Associativity(..)
  , Fixity(..)
  , showAssoc
  , readAssoc
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))

type Precedence = Int

data Associativity = Infixl | Infixr | Infix

derive instance eqAssociativity :: Eq Associativity
derive instance ordAssociativity :: Ord Associativity

instance showAssociativity :: Show Associativity where
  show Infixl = "Infixl"
  show Infixr = "Infixr"
  show Infix  = "Infix"

showAssoc :: Associativity -> String
showAssoc Infixl = "infixl"
showAssoc Infixr = "infixr"
showAssoc Infix  = "infix"

readAssoc :: String -> Associativity
readAssoc "infixl" = Infixl
readAssoc "infixr" = Infixr
readAssoc _        = Infix

instance encodeJsonAssociativity :: EncodeJson Associativity where
  encodeJson = encodeJson <<< showAssoc

instance decodeJsonAssociativity :: DecodeJson Associativity where
  decodeJson json = readAssoc <$> decodeJson json

data Fixity = Fixity Associativity Precedence

derive instance eqFixity :: Eq Fixity
derive instance ordFixity :: Ord Fixity

instance showFixity :: Show Fixity where
  show (Fixity assoc prec) = "(Fixity " <> show assoc <> " " <> show prec <> ")"

instance encodeJsonFixity :: EncodeJson Fixity where
  encodeJson (Fixity assoc prec) =
    "associativity" := assoc ~> "precedence" := prec ~> jsonEmptyObject

instance decodeJsonFixity :: DecodeJson Fixity where
  decodeJson json = do
    obj <- decodeJson json
    assoc <- obj .: "associativity"
    prec <- obj .: "precedence"
    pure (Fixity assoc prec)
