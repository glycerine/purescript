-- | Serialise the CoreFn IR to the JSON format emitted by the Haskell compiler.
-- | The output must be byte-for-byte compatible with `purs compile --codegen corefn`.
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  ) where

import Prelude

import Data.Argonaut.Core
  ( Json
  , fromArray
  , fromBoolean
  , fromNumber
  , fromObject
  , fromString
  , jsonEmptyObject
  , jsonNull
  )
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array as Array
import Data.Either (Either(..), isLeft)
import Data.Enum (fromEnum)
import Data.Int (toNumber) as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan(..))
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))
import Language.PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.Names
  ( Ident
  , ModuleName(..)
  , ProperName
  , Qualified(..)
  , QualifiedBy(..)
  , runIdent
  , runModuleName
  , runProperName
  )
import Language.PureScript.PSString (PSString, decodeString, toUTF16CodeUnits)

-- ---------------------------------------------------------------------------
-- Helpers

obj :: Array (Tuple String Json) -> Json
obj pairs = fromObject (Object.fromFoldable pairs)

-- ---------------------------------------------------------------------------
-- SourcePos / SourceSpan

sourcePosToJSON :: SourcePos -> Json
sourcePosToJSON (SourcePos { line, column }) =
  fromArray [ fromNumber (Int.toNumber line), fromNumber (Int.toNumber column) ]

-- Note: Haskell ToJSON omits the `name` (file path) field from the inner
-- sourceSpan — it only emits `{start, end}`.
sourceSpanToJSON :: SourceSpan -> Json
sourceSpanToJSON (SourceSpan { start, end }) =
  obj
    [ Tuple "start" (sourcePosToJSON start)
    , Tuple "end"   (sourcePosToJSON end)
    ]

-- ---------------------------------------------------------------------------
-- Comments

commentToJSON :: Comment -> Json
commentToJSON (LineComment t)  = obj [ Tuple "LineComment"  (fromString t) ]
commentToJSON (BlockComment t) = obj [ Tuple "BlockComment" (fromString t) ]

-- ---------------------------------------------------------------------------
-- Meta / Ann

constructorTypeToJSON :: ConstructorType -> Json
constructorTypeToJSON ProductType = fromString "ProductType"
constructorTypeToJSON SumType     = fromString "SumType"

metaToJSON :: Meta -> Json
metaToJSON (IsConstructor t is) =
  obj
    [ Tuple "metaType"        (fromString "IsConstructor")
    , Tuple "constructorType" (constructorTypeToJSON t)
    , Tuple "identifiers"     (fromArray (map identToJSON is))
    ]
metaToJSON IsNewtype              = obj [ Tuple "metaType" (fromString "IsNewtype") ]
metaToJSON IsTypeClassConstructor = obj [ Tuple "metaType" (fromString "IsTypeClassConstructor") ]
metaToJSON IsForeign              = obj [ Tuple "metaType" (fromString "IsForeign") ]
metaToJSON IsWhere                = obj [ Tuple "metaType" (fromString "IsWhere") ]
-- The Haskell compiler calls this IsSyntheticApp in JSON; our port uses IsSynthetic
metaToJSON IsSynthetic            = obj [ Tuple "metaType" (fromString "IsSyntheticApp") ]

annToJSON :: Ann -> Json
annToJSON (Tuple ss (Tuple _ meta)) =
  obj
    [ Tuple "sourceSpan" (sourceSpanToJSON ss)
    , Tuple "meta"       (case meta of
        Nothing -> jsonNull
        Just m  -> metaToJSON m)
    ]

-- ---------------------------------------------------------------------------
-- Names

identToJSON :: Ident -> Json
identToJSON = fromString <<< runIdent

properNameToJSON :: forall a. ProperName a -> Json
properNameToJSON = fromString <<< runProperName

moduleNameToJSON :: ModuleName -> Json
moduleNameToJSON (ModuleName n) =
  fromArray (map fromString (String.split (String.Pattern ".") n))

qualifiedToJSON :: forall a. (a -> String) -> Qualified a -> Json
qualifiedToJSON f (Qualified qb a) =
  case qb of
    ByModuleName mn ->
      obj
        [ Tuple "moduleName" (moduleNameToJSON mn)
        , Tuple "identifier" (fromString (f a))
        ]
    BySourcePos ss ->
      obj
        [ Tuple "sourcePos"  (sourcePosToJSON ss)
        , Tuple "identifier" (fromString (f a))
        ]

-- ---------------------------------------------------------------------------
-- PSString -> JSON (Haskell serialises it as a plain string when possible,
-- otherwise as an array of UTF-16 code units — same as our EncodeJson instance)

psStringToJSON :: PSString -> Json
psStringToJSON s = case decodeString s of
  Just str -> fromString str
  Nothing  -> fromArray (map (fromNumber <<< Int.toNumber) (toUTF16CodeUnits s))

-- ---------------------------------------------------------------------------
-- Literals

literalToJSON :: forall a. (a -> Json) -> Literal a -> Json
literalToJSON _ (NumericLiteral (Left n)) =
  obj
    [ Tuple "literalType" (fromString "IntLiteral")
    , Tuple "value"       (fromNumber (Int.toNumber n))
    ]
literalToJSON _ (NumericLiteral (Right n)) =
  obj
    [ Tuple "literalType" (fromString "NumberLiteral")
    , Tuple "value"       (fromNumber n)
    ]
literalToJSON _ (StringLiteral s) =
  obj
    [ Tuple "literalType" (fromString "StringLiteral")
    , Tuple "value"       (psStringToJSON s)
    ]
literalToJSON _ (CharLiteral c) =
  obj
    [ Tuple "literalType" (fromString "CharLiteral")
    , Tuple "value"       (fromString (CU.singleton c))
    ]
literalToJSON _ (BooleanLiteral b) =
  obj
    [ Tuple "literalType" (fromString "BooleanLiteral")
    , Tuple "value"       (fromBoolean b)
    ]
literalToJSON f (ArrayLiteral xs) =
  obj
    [ Tuple "literalType" (fromString "ArrayLiteral")
    , Tuple "value"       (fromArray (map f xs))
    ]
literalToJSON f (ObjectLiteral kvs) =
  obj
    [ Tuple "literalType" (fromString "ObjectLiteral")
    , Tuple "value"       (recordToJSON f kvs)
    ]

-- | Serialise a list of (PSString, a) record fields the same way the Haskell
-- | compiler does: `toJSON . map (toJSON *** f)` → array of [key, value] pairs.
recordToJSON :: forall a. (a -> Json) -> Array (Tuple PSString a) -> Json
recordToJSON f pairs =
  fromArray (map (\(Tuple k v) -> fromArray [ psStringToJSON k, f v ]) pairs)

-- ---------------------------------------------------------------------------
-- Expressions

exprToJSON :: Expr Ann -> Json
exprToJSON (Var ann i) =
  obj
    [ Tuple "type"       (fromString "Var")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "value"      (qualifiedToJSON runIdent i)
    ]
exprToJSON (Literal ann l) =
  obj
    [ Tuple "type"       (fromString "Literal")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "value"      (literalToJSON exprToJSON l)
    ]
exprToJSON (Constructor ann d c is) =
  obj
    [ Tuple "type"            (fromString "Constructor")
    , Tuple "annotation"      (annToJSON ann)
    , Tuple "typeName"        (properNameToJSON d)
    , Tuple "constructorName" (properNameToJSON c)
    , Tuple "fieldNames"      (fromArray (map identToJSON is))
    ]
exprToJSON (Accessor ann f r) =
  obj
    [ Tuple "type"       (fromString "Accessor")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "fieldName"  (psStringToJSON f)
    , Tuple "expression" (exprToJSON r)
    ]
exprToJSON (ObjectUpdate ann r copy fs) =
  obj
    [ Tuple "type"       (fromString "ObjectUpdate")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "expression" (exprToJSON r)
    , Tuple "copy"       (case copy of
        Nothing    -> jsonNull
        Just names -> fromArray (map psStringToJSON names))
    , Tuple "updates"    (recordToJSON exprToJSON fs)
    ]
exprToJSON (Abs ann p b) =
  obj
    [ Tuple "type"       (fromString "Abs")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "argument"   (identToJSON p)
    , Tuple "body"       (exprToJSON b)
    ]
exprToJSON (App ann f x) =
  obj
    [ Tuple "type"        (fromString "App")
    , Tuple "annotation"  (annToJSON ann)
    , Tuple "abstraction" (exprToJSON f)
    , Tuple "argument"    (exprToJSON x)
    ]
exprToJSON (Case ann ss cs) =
  obj
    [ Tuple "type"              (fromString "Case")
    , Tuple "annotation"        (annToJSON ann)
    , Tuple "caseExpressions"   (fromArray (map exprToJSON ss))
    , Tuple "caseAlternatives"  (fromArray (map caseAlternativeToJSON cs))
    ]
exprToJSON (Let ann bs e) =
  obj
    [ Tuple "type"       (fromString "Let")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "binds"      (fromArray (map bindToJSON bs))
    , Tuple "expression" (exprToJSON e)
    ]

-- ---------------------------------------------------------------------------
-- Case alternatives

caseAlternativeToJSON :: CaseAlternative Ann -> Json
caseAlternativeToJSON (CaseAlternative { caseAlternativeBinders: bs, caseAlternativeResult: r' }) =
  let isGuarded = isLeft r'
      exprKey   = if isGuarded then "expressions" else "expression"
      exprVal   = case r' of
        Left guards ->
          fromArray (map (\(Tuple g e) ->
            obj [ Tuple "guard" (exprToJSON g), Tuple "expression" (exprToJSON e) ]) guards)
        Right e -> exprToJSON e
  in obj
    [ Tuple "binders"   (fromArray (map binderToJSON bs))
    , Tuple "isGuarded" (fromBoolean isGuarded)
    , Tuple exprKey     exprVal
    ]

-- ---------------------------------------------------------------------------
-- Binders

binderToJSON :: Binder Ann -> Json
binderToJSON (NullBinder ann) =
  obj
    [ Tuple "binderType" (fromString "NullBinder")
    , Tuple "annotation" (annToJSON ann)
    ]
binderToJSON (LiteralBinder ann l) =
  obj
    [ Tuple "binderType" (fromString "LiteralBinder")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "literal"    (literalToJSON binderToJSON l)
    ]
binderToJSON (VarBinder ann v) =
  obj
    [ Tuple "binderType" (fromString "VarBinder")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "identifier" (identToJSON v)
    ]
binderToJSON (ConstructorBinder ann d c bs) =
  obj
    [ Tuple "binderType"      (fromString "ConstructorBinder")
    , Tuple "annotation"      (annToJSON ann)
    , Tuple "typeName"        (qualifiedToJSON runProperName d)
    , Tuple "constructorName" (qualifiedToJSON runProperName c)
    , Tuple "binders"         (fromArray (map binderToJSON bs))
    ]
binderToJSON (NamedBinder ann n b) =
  obj
    [ Tuple "binderType" (fromString "NamedBinder")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "identifier" (identToJSON n)
    , Tuple "binder"     (binderToJSON b)
    ]

-- ---------------------------------------------------------------------------
-- Binds

bindToJSON :: Bind Ann -> Json
bindToJSON (NonRec ann n e) =
  obj
    [ Tuple "bindType"   (fromString "NonRec")
    , Tuple "annotation" (annToJSON ann)
    , Tuple "identifier" (identToJSON n)
    , Tuple "expression" (exprToJSON e)
    ]
bindToJSON (Rec bs) =
  obj
    [ Tuple "bindType" (fromString "Rec")
    , Tuple "binds"    (fromArray (map encodeRecBind bs))
    ]
  where
  encodeRecBind (Tuple (Tuple ann n) e) =
    obj
      [ Tuple "identifier"  (identToJSON n)
      , Tuple "annotation"  (annToJSON ann)
      , Tuple "expression"  (exprToJSON e)
      ]

-- ---------------------------------------------------------------------------
-- reExports: Map ModuleName (Array Ident) -> JSON object
-- Haskell: toJSON . M.map (map runIdent)
-- The keys are the module name as a dotted string, e.g. "Data.Map"

reExportsToJSON :: Map.Map ModuleName (Array Ident) -> Json
reExportsToJSON m =
  let pairs = Map.toUnfoldable m :: Array (Tuple ModuleName (Array Ident))
  in fromObject $ Object.fromFoldable $
       map (\(Tuple mn idents) ->
         Tuple (runModuleName mn) (fromArray (map identToJSON idents))) pairs

-- ---------------------------------------------------------------------------
-- Module

-- | Serialise a CoreFn module to JSON.
-- | The `version` argument is the compiler version string (e.g. "0.15.16").
moduleToJSON :: String -> Module Ann -> Json
moduleToJSON version (Module m) =
  obj
    [ Tuple "sourceSpan" (sourceSpanToJSON m.moduleSourceSpan)
    , Tuple "moduleName" (moduleNameToJSON m.moduleName)
    , Tuple "modulePath" (fromString m.modulePath)
    , Tuple "imports"    (fromArray (map importToJSON m.moduleImports))
    , Tuple "exports"    (fromArray (map identToJSON m.moduleExports))
    , Tuple "reExports"  (reExportsToJSON m.moduleReExports)
    , Tuple "foreign"    (fromArray (map identToJSON m.moduleForeign))
    , Tuple "decls"      (fromArray (map bindToJSON m.moduleDecls))
    , Tuple "builtWith"  (fromString version)
    , Tuple "comments"   (fromArray (map commentToJSON m.moduleComments))
    ]
  where
  importToJSON (Tuple ann mn) =
    obj
      [ Tuple "annotation" (annToJSON ann)
      , Tuple "moduleName" (moduleNameToJSON mn)
      ]
