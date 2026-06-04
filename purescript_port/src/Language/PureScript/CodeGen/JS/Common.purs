-- | Common code generation utility functions.
module Language.PureScript.CodeGen.JS.Common
  ( moduleNameToJs
  , identToJs
  , properToJs
  , anyNameToJs
  ) where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isAlpha, isAlphaNum)
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits as SCU

import Language.PureScript.Names (Ident(..), ModuleName(..), ProperName(..))

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName mn) =
  let name = String.replaceAll (String.Pattern ".") (String.Replacement "_") mn
  in if nameIsJsBuiltIn name then "$$" <> name else name

nameIsJsBuiltIn :: String -> Boolean
nameIsJsBuiltIn name = Array.elem name
  [ "Infinity", "NaN", "undefined", "null", "true", "false", "eval"
  , "arguments", "Object", "Function", "Boolean", "Symbol", "Error"
  , "Number", "BigInt", "Math", "Date", "String", "RegExp", "Array"
  , "Map", "Set", "WeakMap", "WeakSet", "Promise", "JSON"
  ]

identToJs :: Ident -> String
identToJs (Ident name) = anyNameToJs name
identToJs (GenIdent name i) = anyNameToJs (fromMaybe "" name <> show i)
identToJs UnusedIdent = "$__unused"
identToJs (InternalIdent _) = "$__internal"

properToJs :: forall a. ProperName a -> String
properToJs (ProperName name) = name

anyNameToJs :: String -> String
anyNameToJs name
  | String.null name = name
  | isJsKeyword name = "$$" <> name
  | otherwise =
      let chars = SCU.toCharArray name
          encoded = map encodeChar chars
      in String.joinWith "" encoded
  where
  encodeChar :: Char -> String
  encodeChar c =
    let cp = codePointFromChar c
    in if isAlpha cp || isAlphaNum cp || c == '_' || c == '$'
       then SCU.singleton c
       else "$" <> String.replaceAll (String.Pattern " ") (String.Replacement "_") (show (fromEnum c))

isJsKeyword :: String -> Boolean
isJsKeyword name = Array.elem name
  [ "break", "case", "catch", "continue", "debugger", "default", "delete"
  , "do", "else", "finally", "for", "function", "if", "in", "instanceof"
  , "new", "return", "switch", "this", "throw", "try", "typeof", "var"
  , "void", "while", "with", "class", "const", "enum", "export", "extends"
  , "import", "super", "implements", "interface", "let", "package", "private"
  , "protected", "public", "static", "yield"
  ]
