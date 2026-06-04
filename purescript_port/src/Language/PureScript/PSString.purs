-- | PureScript strings are sequences of UTF-16 code units (0–65535).
-- | Lone surrogates are permitted.
module Language.PureScript.PSString
  ( PSString
  , toUTF16CodeUnits
  , decodeString
  , decodeStringEither
  , decodeStringWithReplacement
  , prettyPrintString
  , prettyPrintStringJS
  , mkString
  ) where

import Prelude

import Data.Array as Array
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Enum (toEnum, fromEnum)
import Data.Int (toStringAs, hexadecimal)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

-- | A PureScript string as an array of UTF-16 code units (Ints 0–65535).
newtype PSString = PSString (Array Int)

instance eqPSString :: Eq PSString where
  eq (PSString a) (PSString b) = a == b

instance ordPSString :: Ord PSString where
  compare (PSString a) (PSString b) = compare a b

instance semigroupPSString :: Semigroup PSString where
  append (PSString a) (PSString b) = PSString (a <> b)

instance monoidPSString :: Monoid PSString where
  mempty = PSString []

instance showPSString :: Show PSString where
  show s = show (decodeStringWithReplacement s)

toUTF16CodeUnits :: PSString -> Array Int
toUTF16CodeUnits (PSString units) = units

isLead :: Int -> Boolean
isLead h = h >= 0xD800 && h <= 0xDBFF

isTrail :: Int -> Boolean
isTrail l = l >= 0xDC00 && l <= 0xDFFF

isSurrogate :: Int -> Boolean
isSurrogate c = isLead c || isTrail c

decodeSurrogatePair :: Int -> Int -> Int
decodeSurrogatePair h l = (h - 0xD800) * 0x400 + (l - 0xDC00) + 0x10000

-- | Decode: lone surrogates are Left (code unit), normal chars are Right (code point int).
decodeStringEither :: PSString -> Array (Either Int Int)
decodeStringEither (PSString units) = unfoldr step units
  where
  step :: Array Int -> Maybe (Tuple (Either Int Int) (Array Int))
  step arr = case Array.uncons arr of
    Nothing -> Nothing
    Just { head: h, tail: rest1 } ->
      if isLead h then
        case Array.uncons rest1 of
          Just { head: l, tail: rest2 } | isTrail l ->
            Just (Tuple (Right (decodeSurrogatePair h l)) rest2)
          _ -> Just (Tuple (Left h) rest1)
      else if isSurrogate h then
        Just (Tuple (Left h) rest1)
      else
        Just (Tuple (Right h) rest1)

codePointToStr :: Int -> String
codePointToStr cp = fromMaybe "\xFFFD" do
  codePoint <- toEnum cp :: Maybe CodePoint
  pure (CP.singleton codePoint)

-- | Decode, replacing lone surrogates with U+FFFD.
decodeStringWithReplacement :: PSString -> String
decodeStringWithReplacement s =
  Array.foldMap encodeEither (decodeStringEither s)
  where
  encodeEither (Left _)   = "\xFFFD"
  encodeEither (Right cp) = codePointToStr cp

-- | Decode to String, returning Nothing if any lone surrogates exist.
decodeString :: PSString -> Maybe String
decodeString s =
  let decoded = decodeStringEither s
      hasLone = Array.any isLeftEither decoded
  in if hasLone then Nothing
     else Just $ Array.foldMap encodeRight decoded
  where
  isLeftEither (Left _) = true
  isLeftEither _        = false
  encodeRight (Right cp) = codePointToStr cp
  encodeRight (Left _)   = ""

-- | Create a PSString from a PureScript String.
mkString :: String -> PSString
mkString str = PSString $ Array.concatMap encodeCodePoint (CP.toCodePointArray str)
  where
  encodeCodePoint :: CodePoint -> Array Int
  encodeCodePoint cp =
    let n = fromEnum cp
    in if n > 0xFFFF
       then
         let h = (n - 0x10000) / 0x400 + 0xD800
             l = (n - 0x10000) `mod` 0x400 + 0xDC00
         in [ h, l ]
       else [ n ]

instance encodeJsonPSString :: EncodeJson PSString where
  encodeJson s = case decodeString s of
    Just str -> encodeJson str
    Nothing  -> encodeJson (toUTF16CodeUnits s)

instance decodeJsonPSString :: DecodeJson PSString where
  decodeJson json =
    let asStr = decodeJson json :: Either JsonDecodeError String
        asArr = decodeJson json :: Either JsonDecodeError (Array Int)
    in case asStr of
      Right str  -> Right (mkString str)
      Left _     -> case asArr of
        Right units -> Right (PSString units)
        Left _      -> Left (TypeMismatch "PSString: expected string or array of ints")

showHex' :: Int -> Int -> String
showHex' width n =
  let hs = toStringAs hexadecimal n
      padLen = max 0 (width - String.length hs)
      padding = CU.fromCharArray (Array.replicate padLen '0')
  in padding <> hs

-- | Pretty-print a PSString using PureScript escape sequences.
prettyPrintString :: PSString -> String
prettyPrintString s =
  "\"" <> Array.foldMap encodeChar (decodeStringEither s) <> "\""
  where
  encodeChar (Left c)  = "\\x" <> showHex' 6 c
  encodeChar (Right c)
    | c == 0x09 = "\\t"
    | c == 0x0D = "\\r"
    | c == 0x0A = "\\n"
    | c == 0x22 = "\\\""
    | c == 0x27 = "\\'"
    | c == 0x5C = "\\\\"
    | shouldPrint c = codePointToStr c
    | otherwise = "\\x" <> showHex' 6 c
  shouldPrint c = c == 0x20 || (c >= 0x21 && c <= 0x7E) || c > 0x9F

-- | Pretty-print using JavaScript escape sequences (for compiled JS output).
prettyPrintStringJS :: PSString -> String
prettyPrintStringJS s =
  "\"" <> Array.foldMap encodeChar (toUTF16CodeUnits s) <> "\""
  where
  encodeChar c
    | c > 0xFF              = "\\u" <> showHex' 4 c
    | c > 0x7E || c < 0x20 = "\\x" <> showHex' 2 c
    | c == 0x08             = "\\b"
    | c == 0x09             = "\\t"
    | c == 0x0A             = "\\n"
    | c == 0x0B             = "\\v"
    | c == 0x0C             = "\\f"
    | c == 0x0D             = "\\r"
    | c == 0x22             = "\\\""
    | c == 0x5C             = "\\\\"
    | otherwise             = fromMaybe "?" do
        ch <- toEnum c :: Maybe Char
        pure (CU.singleton ch)
