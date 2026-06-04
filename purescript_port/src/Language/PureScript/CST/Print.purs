-- | Simple token printer for CST. Printing all tokens with this printer
-- reproduces the exact input that was given to the lexer.
module Language.PureScript.CST.Print
  ( printToken
  , printTokens
  , printLeadingComment
  , printTrailingComment
  ) where

import Prelude

import Data.Array (intercalate)
import Data.String (joinWith)
import Data.Void (Void, absurd)
import Language.PureScript.CST.Types
  ( Comment(..)
  , LineFeed(..)
  , SourceStyle(..)
  , SourceToken(..)
  , Token(..)
  , TokenAnn(..)
  )

printToken :: Token -> String
printToken = printToken' true

printToken' :: Boolean -> Token -> String
printToken' showLayout tok = case tok of
  TokLeftParen             -> "("
  TokRightParen            -> ")"
  TokLeftBrace             -> "{"
  TokRightBrace            -> "}"
  TokLeftSquare            -> "["
  TokRightSquare           -> "]"
  TokLeftArrow ASCII       -> "<-"
  TokLeftArrow Unicode     -> "←"
  TokRightArrow ASCII      -> "->"
  TokRightArrow Unicode    -> "→"
  TokRightFatArrow ASCII   -> "=>"
  TokRightFatArrow Unicode -> "⇒"
  TokDoubleColon ASCII     -> "::"
  TokDoubleColon Unicode   -> "∷"
  TokForall ASCII          -> "forall"
  TokForall Unicode        -> "∀"
  TokEquals                -> "="
  TokPipe                  -> "|"
  TokTick                  -> "`"
  TokDot                   -> "."
  TokComma                 -> ","
  TokUnderscore            -> "_"
  TokBackslash             -> "\\"
  TokLowerName qual name   -> printQual qual <> name
  TokUpperName qual name   -> printQual qual <> name
  TokOperator qual sym     -> printQual qual <> sym
  TokSymbolName qual sym   -> printQual qual <> "(" <> sym <> ")"
  TokSymbolArr Unicode     -> "(→)"
  TokSymbolArr ASCII       -> "(->)"
  TokHole hole             -> "?" <> hole
  TokChar raw _            -> "'" <> raw <> "'"
  TokString raw _          -> "\"" <> raw <> "\""
  TokRawString raw         -> "\"\"\"" <> raw <> "\"\"\""
  TokInt raw _             -> raw
  TokNumber raw _          -> raw
  TokLayoutStart           -> if showLayout then "{" else ""
  TokLayoutSep             -> if showLayout then ";" else ""
  TokLayoutEnd             -> if showLayout then "}" else ""
  TokEof                   -> if showLayout then "<eof>" else ""

printQual :: Array String -> String
printQual quals = joinWith "" (map (\s -> s <> ".") quals)

printTokens :: Array SourceToken -> String
printTokens = printTokens' true

printTokens' :: Boolean -> Array SourceToken -> String
printTokens' showLayout toks = joinWith "" (map pp toks)
  where
  pp (SourceToken { tokAnn: TokenAnn { tokLeadingComments: leading, tokTrailingComments: trailing }, tokValue: tok }) =
    joinWith "" (map printLeadingComment leading)
      <> printToken' showLayout tok
      <> joinWith "" (map printTrailingComment trailing)

printLeadingComment :: Comment LineFeed -> String
printLeadingComment c = case c of
  Comment raw -> raw
  Space n     -> repeatStr n " "
  Line LF     -> "\n"
  Line CRLF   -> "\r\n"

printTrailingComment :: Comment Void -> String
printTrailingComment c = case c of
  Comment raw -> raw
  Space n     -> repeatStr n " "
  Line v      -> absurd v

repeatStr :: Int -> String -> String
repeatStr n s = joinWith "" (map (\_ -> s) (replicate n unit))
  where
  replicate :: forall a. Int -> a -> Array a
  replicate count x = go count []
    where
    go 0 acc = acc
    go k acc = go (k - 1) (acc <> [x])
