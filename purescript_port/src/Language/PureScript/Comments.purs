module Language.PureScript.Comments
  ( Comment(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

data Comment
  = LineComment String
  | BlockComment String

derive instance eqComment :: Eq Comment
derive instance ordComment :: Ord Comment

instance showComment :: Show Comment where
  show (LineComment t) = "(LineComment " <> show t <> ")"
  show (BlockComment t) = "(BlockComment " <> show t <> ")"

instance encodeJsonComment :: EncodeJson Comment where
  encodeJson (LineComment t) =
    encodeJson $ Object.fromFoldable [ Tuple "LineComment" (encodeJson t) ]
  encodeJson (BlockComment t) =
    encodeJson $ Object.fromFoldable [ Tuple "BlockComment" (encodeJson t) ]

instance decodeJsonComment :: DecodeJson Comment where
  decodeJson json = do
    obj <- decodeJson json
    let keys = Object.keys obj
    case keys of
      [ "LineComment" ] -> LineComment <$> (obj .: "LineComment")
      [ "BlockComment" ] -> BlockComment <$> (obj .: "BlockComment")
      _ -> Left $ TypeMismatch "Comment"
