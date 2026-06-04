module Language.PureScript.CST.Errors
  ( ParserErrorInfo(..)
  , ParserErrorType(..)
  , ParserWarningType(..)
  , ParserError
  , ParserWarning
  , prettyPrintError
  , prettyPrintErrorMessage
  , prettyPrintWarningMessage
  ) where

import Prelude

import Data.Array (head, length) as Array
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.String (toUpper) as String
import Data.String.CodePoints (codePointFromChar, toCodePointArray) as SCP
import Data.String.CodeUnits (charAt, singleton) as SCU
import Language.PureScript.CST.Layout (LayoutStack)
import Language.PureScript.CST.Print (printToken)
import Language.PureScript.CST.Types
  ( CSTSourcePos(..)
  , SourceRange(..)
  , SourceToken(..)
  , Token(..)
  )

data ParserErrorType
  = ErrWildcardInType
  | ErrConstraintInKind
  | ErrHoleInType
  | ErrExprInBinder
  | ErrExprInDeclOrBinder
  | ErrExprInDecl
  | ErrBinderInDecl
  | ErrRecordUpdateInCtr
  | ErrRecordPunInUpdate
  | ErrRecordCtrInUpdate
  | ErrTypeInConstraint
  | ErrElseInDecl
  | ErrInstanceNameMismatch
  | ErrUnknownFundep
  | ErrImportInDecl
  | ErrGuardInLetBinder
  | ErrKeywordVar
  | ErrKeywordSymbol
  | ErrQuotedPun
  | ErrToken
  | ErrLineFeedInString
  | ErrAstralCodePointInChar
  | ErrCharEscape
  | ErrNumberOutOfRange
  | ErrLeadingZero
  | ErrExpectedFraction
  | ErrExpectedExponent
  | ErrExpectedHex
  | ErrReservedSymbol
  | ErrCharInGap Char
  | ErrModuleName
  | ErrQualifiedName
  | ErrEmptyDo
  | ErrLexeme (Maybe String) (Array String)
  | ErrConstraintInForeignImportSyntax
  | ErrEof
  | ErrCustom String

derive instance eqParserErrorType  :: Eq ParserErrorType
derive instance ordParserErrorType :: Ord ParserErrorType

instance showParserErrorType :: Show ParserErrorType where
  show _ = "<ParserErrorType>"

data ParserWarningType
  = WarnDeprecatedRowSyntax
  | WarnDeprecatedForeignKindSyntax
  | WarnDeprecatedKindImportSyntax
  | WarnDeprecatedKindExportSyntax
  | WarnDeprecatedCaseOfOffsideSyntax

derive instance eqParserWarningType  :: Eq ParserWarningType
derive instance ordParserWarningType :: Ord ParserWarningType

instance showParserWarningType :: Show ParserWarningType where
  show _ = "<ParserWarningType>"

data ParserErrorInfo a = ParserErrorInfo
  { errRange :: SourceRange
  , errToks  :: Array SourceToken
  , errStack :: LayoutStack
  , errType  :: a
  }

derive instance eqParserErrorInfo  :: Eq a  => Eq (ParserErrorInfo a)
derive instance ordParserErrorInfo :: Ord a => Ord (ParserErrorInfo a)

instance showParserErrorInfo :: Show a => Show (ParserErrorInfo a) where
  show _ = "<ParserErrorInfo>"

type ParserError   = ParserErrorInfo ParserErrorType
type ParserWarning = ParserErrorInfo ParserWarningType

prettyPrintError :: ParserError -> String
prettyPrintError pe@(ParserErrorInfo { errRange }) =
  prettyPrintErrorMessage pe <> " at " <> errPos
  where
  errPos = case errRange of
    SourceRange { srcStart: CSTSourcePos { srcLine: line, srcColumn: col } } ->
      "line " <> show line <> ", column " <> show col

prettyPrintErrorMessage :: ParserError -> String
prettyPrintErrorMessage (ParserErrorInfo { errType, errToks }) = case errType of
  ErrWildcardInType ->
    "Unexpected wildcard in type; type wildcards are only allowed in value annotations"
  ErrConstraintInKind ->
    "Unsupported constraint in kind; constraints are only allowed in value annotations"
  ErrHoleInType ->
    "Unexpected hole in type; type holes are only allowed in value annotations"
  ErrExprInBinder ->
    "Expected pattern, saw expression"
  ErrExprInDeclOrBinder ->
    "Expected declaration or pattern, saw expression"
  ErrExprInDecl ->
    "Expected declaration, saw expression"
  ErrBinderInDecl ->
    "Expected declaration, saw pattern"
  ErrRecordUpdateInCtr ->
    "Expected ':', saw '='"
  ErrRecordPunInUpdate ->
    "Expected record update, saw pun"
  ErrRecordCtrInUpdate ->
    "Expected '=', saw ':'"
  ErrTypeInConstraint ->
    "Expected constraint, saw type"
  ErrElseInDecl ->
    "Expected declaration, saw 'else'"
  ErrInstanceNameMismatch ->
    "All instances in a chain must implement the same type class"
  ErrUnknownFundep ->
    "Unknown type variable in functional dependency"
  ErrImportInDecl ->
    "Expected declaration, saw 'import'"
  ErrGuardInLetBinder ->
    "Unexpected guard in let pattern"
  ErrKeywordVar ->
    "Expected variable, saw keyword"
  ErrKeywordSymbol ->
    "Expected symbol, saw reserved symbol"
  ErrQuotedPun ->
    "Unexpected quoted label in record pun, perhaps due to a missing ':'"
  ErrEof ->
    "Unexpected end of input"
  ErrLexeme (Just a) _
    | isSpaceChar (SCU.charAt 0 a) ->
        "Illegal whitespace character " <> displayCodePoint (SCU.charAt 0 a)
    | otherwise ->
        "Unexpected " <> a
  ErrLineFeedInString ->
    "Unexpected line feed in string literal"
  ErrAstralCodePointInChar ->
    "Illegal astral code point in character literal"
  ErrCharEscape ->
    "Illegal character escape code"
  ErrNumberOutOfRange ->
    "Number literal is out of range"
  ErrLeadingZero ->
    "Unexpected leading zeros"
  ErrExpectedFraction ->
    "Expected fraction"
  ErrExpectedExponent ->
    "Expected exponent"
  ErrExpectedHex ->
    "Expected hex digit"
  ErrReservedSymbol ->
    "Unexpected reserved symbol"
  ErrCharInGap ch ->
    "Unexpected character '" <> SCU.singleton ch <> "' in gap"
  ErrModuleName ->
    "Invalid module name; underscores and primes are not allowed in module names"
  ErrQualifiedName ->
    "Unexpected qualified name"
  ErrEmptyDo ->
    "Expected do statement"
  ErrLexeme _ _ ->
    basicError errToks
  ErrConstraintInForeignImportSyntax ->
    "Constraints are not allowed in foreign imports. Omit the constraint instead and update the foreign module accordingly."
  ErrToken ->
    case Array.head errToks of
      Just (SourceToken { tokValue: TokLeftArrow _ }) ->
        "Unexpected \"<-\" in expression, perhaps due to a missing 'do' or 'ado' keyword"
      _ -> basicError errToks
  ErrCustom err ->
    err
  where
  isSpaceChar :: Maybe Char -> Boolean
  isSpaceChar (Just c) = c == ' ' || c == '\t' || c == '\n' || c == '\r'
  isSpaceChar Nothing  = false

basicError :: Array SourceToken -> String
basicError toks = case Array.head toks of
  Just (SourceToken { tokValue: tok }) -> basicTokError tok
  Nothing -> "Unexpected input"
  where
  basicTokError t = case t of
    TokLayoutStart -> "Unexpected or mismatched indentation"
    TokLayoutSep   -> "Unexpected or mismatched indentation"
    TokLayoutEnd   -> "Unexpected or mismatched indentation"
    TokEof         -> "Unexpected end of input"
    _              -> "Unexpected token '" <> printToken t <> "'"

displayCodePoint :: Maybe Char -> String
displayCodePoint Nothing  = "U+0000"
displayCodePoint (Just c) =
  "U+" <> toUpperHex (fromEnum (SCP.codePointFromChar c))

toUpperHex :: Int -> String
toUpperHex n = padLeft4 (intToHex n)
  where
  intToHex :: Int -> String
  intToHex x
    | x <= 0    = ""
    | otherwise =
        let digit = x `mod` 16
            rest  = x `div` 16
            ch    = if digit < 10
                    then SCU.singleton (toEnum' (digit + 48))
                    else SCU.singleton (toEnum' (digit - 10 + 65))
        in intToHex rest <> ch

  padLeft4 :: String -> String
  padLeft4 s
    | stringLen s >= 4 = s
    | otherwise = padLeft4 ("0" <> s)

  stringLen :: String -> Int
  stringLen str = Array.length (SCP.toCodePointArray str)

toEnum' :: Int -> Char
toEnum' = toEnum

foreign import toEnum :: Int -> Char

prettyPrintWarningMessage :: ParserWarning -> String
prettyPrintWarningMessage (ParserErrorInfo { errType }) = case errType of
  WarnDeprecatedRowSyntax ->
    "Unary '#' syntax for row kinds is deprecated and will be removed in a future release. Use the 'Row' kind instead."
  WarnDeprecatedForeignKindSyntax ->
    "Foreign kind imports are deprecated and will be removed in a future release. Use empty 'data' instead."
  WarnDeprecatedKindImportSyntax ->
    "Kind imports are deprecated and will be removed in a future release. Omit the 'kind' keyword instead."
  WarnDeprecatedKindExportSyntax ->
    "Kind exports are deprecated and will be removed in a future release. Omit the 'kind' keyword instead."
  WarnDeprecatedCaseOfOffsideSyntax ->
    "Dedented expressions in case branches are deprecated and will be removed in a future release. Indent the branch's expression past it's binder instead."
