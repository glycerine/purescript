-- | Pretty printer for the JavaScript CoreImp AST.
-- | Translates CoreImp Module and AST nodes to JavaScript source text.
module Language.PureScript.CodeGen.JS.Printer
  ( prettyPrintJS
  , prettyPrintJSWithSourceMaps
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))

import Language.PureScript.CodeGen.JS.Common
  ( anyNameToJs
  )
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.CoreImp.AST
  ( AST(..)
  , BinaryOperator(..)
  , CIComments(..)
  , UnaryOperator(..)
  )
import Language.PureScript.CoreImp.Module (Export(..), Import(..), Module(..))
import Language.PureScript.Names (runModuleName)
import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

-- ---------------------------------------------------------------------------
-- Printer state
-- ---------------------------------------------------------------------------

type Indent = Int

indentStr :: Indent -> String
indentStr n = String.fromCodePointArray
  (Array.replicate (n * 2) (String.codePointFromChar ' '))

-- ---------------------------------------------------------------------------
-- Top-level entry points
-- ---------------------------------------------------------------------------

-- | Pretty-print a CoreImp module to JavaScript source text.
prettyPrintJS :: Module -> String
prettyPrintJS m = printModule m

-- | Pretty-print with (trivially empty) source maps.
prettyPrintJSWithSourceMaps :: Module -> Tuple String (Array (Tuple Int (Array Int)))
prettyPrintJSWithSourceMaps m = Tuple (printModule m) []

-- ---------------------------------------------------------------------------
-- Module printer
-- ---------------------------------------------------------------------------

printModule :: Module -> String
printModule (Module { modHeader, modImports, modBody, modExports }) =
  let headerLines  = map printComment modHeader
      importLines  = map printImport modImports
      bodyLines    = map (printStatement 0) modBody
      exportLines  = map printExport modExports
      sections     = Array.filter (not <<< String.null)
        [ joinLines headerLines
        , joinLines importLines
        , joinLines bodyLines
        , joinLines exportLines
        ]
  in String.joinWith "\n" sections <> "\n"

joinLines :: Array String -> String
joinLines = String.joinWith "\n" <<< Array.filter (not <<< String.null)

-- ---------------------------------------------------------------------------
-- Comments
-- ---------------------------------------------------------------------------

printComment :: Comment -> String
printComment (LineComment t)  = "//" <> t
printComment (BlockComment t) =
  "/**\n" <>
  String.joinWith "\n" (map (\line -> " * " <> line) (String.split (String.Pattern "\n") t)) <>
  "\n */"

-- ---------------------------------------------------------------------------
-- Imports
-- ---------------------------------------------------------------------------

printImport :: Import -> String
printImport (Import ident from) =
  "var " <> ident <> " = require(" <> prettyPrintStringJS from <> ");"

-- ---------------------------------------------------------------------------
-- Exports
-- ---------------------------------------------------------------------------

printExport :: Export -> String
printExport (Export idents from) =
  let identList = NEL.toUnfoldable idents :: Array String
      exportedIdent ident =
        let jsIdent = exportIdentToJs ident
        in case from of
             Nothing -> "exports[\"" <> ident <> "\"] = " <> jsIdent <> ";"
             Just f  -> "exports[\"" <> ident <> "\"] = require(" <> prettyPrintStringJS f <> ")[\"" <> ident <> "\"];"
  in String.joinWith "\n" (map exportedIdent identList)

exportIdentToJs :: String -> String
exportIdentToJs ident
  | nameIsJsBuiltIn ident = "$$" <> ident
  | otherwise = anyNameToJs ident

-- ---------------------------------------------------------------------------
-- Statements
-- ---------------------------------------------------------------------------

printStatement :: Indent -> AST -> String
printStatement ind ast = indentStr ind <> printExpr ind ast <> ";"

printStatements :: Indent -> Array AST -> String
printStatements ind stmts =
  String.joinWith "\n" (map (printStatement ind) stmts)

-- ---------------------------------------------------------------------------
-- Expressions (with operator precedence handled via wrapping)
-- ---------------------------------------------------------------------------

-- Precedence levels (higher = tighter binding)
data Prec = Prec Int

precOf :: AST -> Int
precOf (Binary _ op _ _) = binOpPrec op
precOf (Unary _ _ _)     = 14
precOf (App _ _ _)       = 17
precOf (Indexer _ _ _)   = 17
precOf (InstanceOf _ _ _) = 10
precOf (Function _ _ _ _) = 1
precOf _ = 20

binOpPrec :: BinaryOperator -> Int
binOpPrec Multiply             = 13
binOpPrec Divide               = 13
binOpPrec Modulus              = 13
binOpPrec Add                  = 12
binOpPrec Subtract             = 12
binOpPrec ShiftLeft            = 11
binOpPrec ShiftRight           = 11
binOpPrec ZeroFillShiftRight   = 11
binOpPrec LessThan             = 10
binOpPrec LessThanOrEqualTo    = 10
binOpPrec GreaterThan          = 10
binOpPrec GreaterThanOrEqualTo = 10
binOpPrec EqualTo              = 9
binOpPrec NotEqualTo           = 9
binOpPrec BitwiseAnd           = 8
binOpPrec BitwiseXor           = 7
binOpPrec BitwiseOr            = 6
binOpPrec And                  = 5
binOpPrec Or                   = 4

-- Print an expression, wrapping in parens if its precedence is lower than ctx
printExprPrec :: Indent -> Int -> AST -> String
printExprPrec ind ctxPrec ast =
  let s = printExpr ind ast
      p = precOf ast
  in if p < ctxPrec then "(" <> s <> ")" else s

printExpr :: Indent -> AST -> String
printExpr ind ast = case ast of
  NumericLiteral _ (Left n)  -> show n
  NumericLiteral _ (Right n) -> show n

  StringLiteral _ s -> prettyPrintStringJS s

  BooleanLiteral _ true  -> "true"
  BooleanLiteral _ false -> "false"

  ArrayLiteral _ xs ->
    "[ " <> String.joinWith ", " (map (printExpr ind) xs) <> " ]"

  ObjectLiteral _ [] -> "{}"
  ObjectLiteral _ ps ->
    let ind' = ind + 1
        printProp (Tuple k v) = indentStr ind' <> objectKey k <> ": " <> printExpr ind' v
        inner = String.joinWith ",\n" (map printProp ps)
    in "{\n" <> inner <> "\n" <> indentStr ind <> "}"

  Var _ ident -> ident

  ModuleAccessor _ mn prop ->
    let modJs = runModuleName mn
        propStr = fromMaybe (prettyPrintStringJS prop) (decodeString prop)
    in modJs <> "." <> propStr

  Function _ name args body ->
    let nameStr = fromMaybe "" name
        argsStr = String.joinWith ", " args
    in "function " <> nameStr <> "(" <> argsStr <> ") " <> printBlock ind body

  App _ fn args ->
    let fnStr = printExprPrec ind (precOf fn `max` 17) fn
        argsStr = String.joinWith ", " (map (printExpr ind) args)
    in fnStr <> "(" <> argsStr <> ")"

  Indexer _ index val ->
    case index of
      StringLiteral _ prop ->
        case decodeString prop of
          Just s | isValidJsIdentifier s ->
            printExprPrec ind 17 val <> "." <> s
          _ ->
            printExprPrec ind 17 val <> "[" <> prettyPrintStringJS prop <> "]"
      _ ->
        printExprPrec ind 17 val <> "[" <> printExpr ind index <> "]"

  InstanceOf _ val ty ->
    printExprPrec ind 10 val <> " instanceof " <> printExprPrec ind 10 ty

  Unary _ op val ->
    unaryOpStr op <> printExprPrec ind 14 val

  Binary _ op l r ->
    let p   = binOpPrec op
        lhs = printExprPrec ind (p + 1) l
        rhs = printExprPrec ind (p + 1) r
        opStr = " " <> binaryOpStr op <> " "
    in lhs <> opStr <> rhs

  -- Blocks are only valid as function bodies; emit as block
  Block _ stmts ->
    printBlock ind (Block Nothing stmts)

  VariableIntroduction _ ident Nothing ->
    "var " <> ident

  VariableIntroduction _ ident (Just (Tuple _ val)) ->
    "var " <> ident <> " = " <> printExpr ind val

  Assignment _ target val ->
    printExpr ind target <> " = " <> printExpr ind val

  While _ cond body ->
    "while (" <> printExpr ind cond <> ") " <> printBlock ind body

  For _ ident start end body ->
    "for (var " <> ident <> " = " <> printExpr ind start <>
    "; " <> ident <> " < " <> printExpr ind end <>
    "; " <> ident <> "++) " <> printBlock ind body

  ForIn _ ident obj body ->
    "for (var " <> ident <> " in " <> printExpr ind obj <> ") " <> printBlock ind body

  IfElse _ cond thenBranch elseBranch ->
    "if (" <> printExpr ind cond <> ") " <>
    printBlock ind thenBranch <>
    case elseBranch of
      Nothing -> ""
      Just e  -> " else " <> printBlock ind e

  Return _ val ->
    "return " <> printExpr ind val

  ReturnNoResult _ -> "return"

  Throw _ val ->
    "throw " <> printExpr ind val

  Comment (SourceComments comments) inner ->
    String.joinWith "" (map printComment comments) <> "\n" <>
    indentStr ind <> printExpr ind inner

  Comment PureAnnotation inner ->
    "/* #__PURE__ */ " <> printExpr ind inner

-- ---------------------------------------------------------------------------
-- Block printing
-- ---------------------------------------------------------------------------

printBlock :: Indent -> AST -> String
printBlock ind (Block _ stmts) =
  let ind'  = ind + 1
      inner = printStatements ind' stmts
  in "{\n" <> inner <> "\n" <> indentStr ind <> "}"
printBlock ind other =
  "{ " <> printStatement ind other <> " }"

-- ---------------------------------------------------------------------------
-- Operator strings
-- ---------------------------------------------------------------------------

unaryOpStr :: UnaryOperator -> String
unaryOpStr Negate    = "-"
unaryOpStr Not       = "!"
unaryOpStr BitwiseNot = "~"
unaryOpStr Positive  = "+"
unaryOpStr New       = "new "

binaryOpStr :: BinaryOperator -> String
binaryOpStr Add                  = "+"
binaryOpStr Subtract             = "-"
binaryOpStr Multiply             = "*"
binaryOpStr Divide               = "/"
binaryOpStr Modulus              = "%"
binaryOpStr EqualTo              = "==="
binaryOpStr NotEqualTo           = "!=="
binaryOpStr LessThan             = "<"
binaryOpStr LessThanOrEqualTo    = "<="
binaryOpStr GreaterThan          = ">"
binaryOpStr GreaterThanOrEqualTo = ">="
binaryOpStr And                  = "&&"
binaryOpStr Or                   = "||"
binaryOpStr BitwiseAnd           = "&"
binaryOpStr BitwiseOr            = "|"
binaryOpStr BitwiseXor           = "^"
binaryOpStr ShiftLeft            = "<<"
binaryOpStr ShiftRight           = ">>"
binaryOpStr ZeroFillShiftRight   = ">>>"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

objectKey :: PSString -> String
objectKey s = case decodeString s of
  Just k | isValidJsIdentifier k -> k
  _ -> prettyPrintStringJS s

isValidJsIdentifier :: String -> Boolean
isValidJsIdentifier s =
  not (String.null s) &&
  not (isJsKeyword s) &&
  not (nameIsJsBuiltIn s) &&
  isValidFirst (String.take 1 s) &&
  Array.all isValidRest (map String.singleton (String.toCodePointArray (String.drop 1 s)))
  where
  isValidFirst c = String.length c == 1 &&
    let cp = fromEnum (unsafeCodePointAt0 c)
    in (cp >= 65 && cp <= 90) || (cp >= 97 && cp <= 122) || cp == 95 || cp == 36
  isValidRest c = String.length c == 1 &&
    let cp = fromEnum (unsafeCodePointAt0 c)
    in (cp >= 65 && cp <= 90) || (cp >= 97 && cp <= 122) ||
       (cp >= 48 && cp <= 57) || cp == 95 || cp == 36

unsafeCodePointAt0 :: String -> String.CodePoint
unsafeCodePointAt0 s = fromMaybe (String.codePointFromChar ' ') (Array.head (String.toCodePointArray s))

isJsKeyword :: String -> Boolean
isJsKeyword name = Array.elem name
  [ "break", "case", "catch", "continue", "debugger", "default", "delete"
  , "do", "else", "finally", "for", "function", "if", "in", "instanceof"
  , "new", "return", "switch", "this", "throw", "try", "typeof", "var"
  , "void", "while", "with", "class", "const", "enum", "export", "extends"
  , "import", "super", "implements", "interface", "let", "package", "private"
  , "protected", "public", "static", "yield"
  ]

nameIsJsBuiltIn :: String -> Boolean
nameIsJsBuiltIn name = Array.elem name
  [ "Infinity", "NaN", "undefined", "null", "true", "false", "eval"
  , "arguments", "Object", "Function", "Boolean", "Symbol", "Error"
  , "Number", "BigInt", "Math", "Date", "String", "RegExp", "Array"
  , "Map", "Set", "WeakMap", "WeakSet", "Promise", "JSON"
  ]
