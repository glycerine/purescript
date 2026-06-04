module Language.PureScript.AST.SourcePos
  ( SourcePos(..)
  , SourceSpan(..)
  , SourceAnn
  , displaySourcePos
  , displaySourcePosShort
  , displaySourceSpan
  , displayStartEndPos
  , displayStartEndPosShort
  , internalModuleSourceSpan
  , nullSourceSpan
  , nullSourceAnn
  , nonEmptySpan
  , widenSourceSpan
  , widenSourceAnn
  , spanStart
  , isNullSourceSpan
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Language.PureScript.Comments (Comment)

type SourceAnn = Tuple SourceSpan (Array Comment)

newtype SourcePos = SourcePos { line :: Int, column :: Int }

derive instance eqSourcePos :: Eq SourcePos
derive instance ordSourcePos :: Ord SourcePos

instance showSourcePos :: Show SourcePos where
  show (SourcePos { line, column }) =
    "(SourcePos { line: " <> show line <> ", column: " <> show column <> " })"

instance encodeJsonSourcePos :: EncodeJson SourcePos where
  encodeJson (SourcePos { line, column }) = encodeJson [ line, column ]

instance decodeJsonSourcePos :: DecodeJson SourcePos where
  decodeJson json = do
    arr <- decodeJson json
    case arr of
      [ l, c ] -> Right $ SourcePos { line: l, column: c }
      _ -> Left $ TypeMismatch "SourcePos [line, col]"

displaySourcePos :: SourcePos -> String
displaySourcePos (SourcePos { line, column }) =
  "line " <> show line <> ", column " <> show column

displaySourcePosShort :: SourcePos -> String
displaySourcePosShort (SourcePos { line, column }) =
  show line <> ":" <> show column

newtype SourceSpan = SourceSpan { name :: String, start :: SourcePos, end :: SourcePos }

derive instance eqSourceSpan :: Eq SourceSpan
derive instance ordSourceSpan :: Ord SourceSpan

instance showSourceSpan :: Show SourceSpan where
  show (SourceSpan { name, start, end }) =
    "(SourceSpan { name: " <> show name <> ", start: " <> show start <> ", end: " <> show end <> " })"

instance encodeJsonSourceSpan :: EncodeJson SourceSpan where
  encodeJson (SourceSpan { name, start, end }) =
    "name" := name ~> "start" := start ~> "end" := end ~> jsonEmptyObject

instance decodeJsonSourceSpan :: DecodeJson SourceSpan where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .: "name"
    start <- obj .: "start"
    end <- obj .: "end"
    Right $ SourceSpan { name, start, end }

displayStartEndPos :: SourceSpan -> String
displayStartEndPos ss@(SourceSpan { start, end }) =
  "(" <> displaySourcePos start <> " - " <> displaySourcePos end <> ")"

displayStartEndPosShort :: SourceSpan -> String
displayStartEndPosShort (SourceSpan { start, end }) =
  displaySourcePosShort start <> " - " <> displaySourcePosShort end

displaySourceSpan :: String -> SourceSpan -> String
displaySourceSpan relPath ss@(SourceSpan { name }) =
  makeRelative relPath name <> ":" <> displayStartEndPosShort ss <> " " <> displayStartEndPos ss
  where
  makeRelative base path =
    let basePfx = base <> "/"
    in if base /= "" && String.take (String.length basePfx) path == basePfx
       then String.drop (String.length basePfx) path
       else path

internalModuleSourceSpan :: String -> SourceSpan
internalModuleSourceSpan name =
  SourceSpan { name, start: SourcePos { line: 0, column: 0 }, end: SourcePos { line: 0, column: 0 } }

nullSourceSpan :: SourceSpan
nullSourceSpan = internalModuleSourceSpan ""

nullSourceAnn :: SourceAnn
nullSourceAnn = Tuple nullSourceSpan []

spanStart :: SourceSpan -> SourcePos
spanStart (SourceSpan ss) = ss.start

isNullSourceSpan :: SourceSpan -> Boolean
isNullSourceSpan (SourceSpan { name: "", start: SourcePos { line: 0, column: 0 }, end: SourcePos { line: 0, column: 0 } }) = true
isNullSourceSpan _ = false

nonEmptySpan :: SourceAnn -> Maybe SourceSpan
nonEmptySpan (Tuple ss _)
  | isNullSourceSpan ss = Nothing
  | otherwise = Just ss

widenSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
widenSourceSpan a b
  | isNullSourceSpan a = b
  | isNullSourceSpan b = a
  | otherwise =
      let SourceSpan { name: n1, start: s1, end: e1 } = a
          SourceSpan { name: n2, start: s2, end: e2 } = b
          n = if n1 == "" then n2 else n1
      in SourceSpan { name: n, start: min s1 s2, end: max e1 e2 }

widenSourceAnn :: SourceAnn -> SourceAnn -> SourceAnn
widenSourceAnn (Tuple s1 _) (Tuple s2 _) = Tuple (widenSourceSpan s1 s2) []
