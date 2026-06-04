module Language.PureScript.Environment
  ( Environment(..)
  , TypeClassData(..)
  , FunctionalDependency(..)
  , NameVisibility(..)
  , NameKind(..)
  , TypeKind(..)
  , DataDeclType(..)
  , initEnvironment
  , makeTypeClassData
  , showDataDeclType
  , tyRecord
  , tyFunction
  , function
  , kindRow
  , dictTypeName
  , primTypes
  , primClasses
  , primBooleanTypes
  , primCoerceTypes
  , primCoerceClasses
  , primOrderingTypes
  , primRowTypes
  , primRowClasses
  , primRowListTypes
  , primRowListClasses
  , primSymbolTypes
  , primSymbolClasses
  , primIntTypes
  , primIntClasses
  , primTypeErrorTypes
  , primTypeErrorClasses
  , allPrimTypes
  , allPrimClasses
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Language.PureScript.Names (ClassName, ConstructorName, Ident, ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..), TypeName, coerceProperName)
import Language.PureScript.Constants.Prim as C
import Language.PureScript.Roles (Role(..))
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope)
import Language.PureScript.AST.SourcePos (nullSourceAnn)
import Language.PureScript.Types (SourceConstraint, SourceType, Type(..), srcTypeConstructor)

data NameVisibility = Undefined | Defined

derive instance eqNameVisibility :: Eq NameVisibility

instance showNameVisibility :: Show NameVisibility where
  show Undefined = "Undefined"
  show Defined   = "Defined"

data NameKind = Private | Public | External

derive instance eqNameKind :: Eq NameKind

instance showNameKind :: Show NameKind where
  show Private  = "Private"
  show Public   = "Public"
  show External = "External"

data DataDeclType = Data | Newtype

derive instance eqDataDeclType :: Eq DataDeclType
derive instance ordDataDeclType :: Ord DataDeclType

instance showDataDeclType' :: Show DataDeclType where
  show Data    = "Data"
  show Newtype = "Newtype"

showDataDeclType :: DataDeclType -> String
showDataDeclType Data    = "data"
showDataDeclType Newtype = "newtype"

instance encodeJsonDataDeclType :: EncodeJson DataDeclType where
  encodeJson = encodeJson <<< showDataDeclType

instance decodeJsonDataDeclType :: DecodeJson DataDeclType where
  decodeJson json = do
    s <- decodeJson json
    case s of
      "data"    -> Right Data
      "newtype" -> Right Newtype
      other     -> Left (TypeMismatch ("DataDeclType: " <> other))

data TypeKind
  = DataType DataDeclType (Array (Tuple (Tuple String (Maybe SourceType)) Role))
      (Array (Tuple (ProperName ConstructorName) (Array SourceType)))
  | TypeSynonym
  | ExternData (Array Role)
  | LocalTypeVariable
  | ScopedTypeVar

derive instance eqTypeKind :: Eq TypeKind

instance showTypeKind :: Show TypeKind where
  show (DataType dt _ _) = "(DataType " <> show dt <> " ...)"
  show TypeSynonym        = "TypeSynonym"
  show (ExternData roles) = "(ExternData " <> show roles <> ")"
  show LocalTypeVariable  = "LocalTypeVariable"
  show ScopedTypeVar      = "ScopedTypeVar"

data FunctionalDependency = FunctionalDependency
  { fdDeterminers :: Array Int
  , fdDetermined  :: Array Int
  }

derive instance eqFunctionalDependency :: Eq FunctionalDependency

instance showFunctionalDependency :: Show FunctionalDependency where
  show (FunctionalDependency { fdDeterminers, fdDetermined }) =
    "(FunctionalDependency { determiners: " <> show fdDeterminers <> ", determined: " <> show fdDetermined <> " })"

instance encodeJsonFunctionalDependency :: EncodeJson FunctionalDependency where
  encodeJson (FunctionalDependency { fdDeterminers, fdDetermined }) =
    "determiners" := fdDeterminers ~> "determined" := fdDetermined ~> jsonEmptyObject

instance decodeJsonFunctionalDependency :: DecodeJson FunctionalDependency where
  decodeJson json = do
    obj <- decodeJson json
    fdDeterminers <- obj .: "determiners"
    fdDetermined  <- obj .: "determined"
    pure $ FunctionalDependency { fdDeterminers, fdDetermined }

data TypeClassData = TypeClassData
  { typeClassArguments         :: Array (Tuple String (Maybe SourceType))
  , typeClassMembers           :: Array (Tuple (Tuple Ident SourceType) (Maybe (Set (Array Int))))
  , typeClassSuperclasses      :: Array SourceConstraint
  , typeClassDependencies      :: Array FunctionalDependency
  , typeClassDeterminedArguments :: Set Int
  , typeClassCoveringSets      :: Set (Set Int)
  , typeClassIsEmpty           :: Boolean
  }

instance showTypeClassData :: Show TypeClassData where
  show (TypeClassData d) = "(TypeClassData { args: " <> show d.typeClassArguments <> " })"

makeTypeClassData
  :: Array (Tuple String (Maybe SourceType))
  -> Array (Tuple Ident SourceType)
  -> Array SourceConstraint
  -> Array FunctionalDependency
  -> Boolean
  -> TypeClassData
makeTypeClassData args members superclasses deps isEmpty =
  TypeClassData
    { typeClassArguments: args
    , typeClassMembers: map (\(Tuple i t) -> Tuple (Tuple i t) Nothing) members
    , typeClassSuperclasses: superclasses
    , typeClassDependencies: deps
    , typeClassDeterminedArguments: Set.empty
    , typeClassCoveringSets: Set.empty
    , typeClassIsEmpty: isEmpty
    }

data Environment = Environment
  { names :: Map (Qualified Ident) (Tuple (Tuple SourceType NameKind) NameVisibility)
  , types :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
  , dataConstructors :: Map (Qualified (ProperName ConstructorName))
      (Tuple (Tuple (Tuple DataDeclType (ProperName TypeName)) SourceType) (Array Ident))
  , typeSynonyms :: Map (Qualified (ProperName TypeName))
      (Tuple (Array (Tuple String (Maybe SourceType))) SourceType)
  , typeClassDictionaries :: Map QualifiedBy
      (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
  , typeClasses :: Map (Qualified (ProperName ClassName)) TypeClassData
  }

instance showEnvironment :: Show Environment where
  show _ = "<Environment>"

initEnvironment :: Environment
initEnvironment = Environment
  { names: Map.empty
  , types: allPrimTypes
  , dataConstructors: Map.empty
  , typeSynonyms: Map.empty
  , typeClassDictionaries: Map.empty
  , typeClasses: allPrimClasses
  }

allPrimTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
allPrimTypes = Map.unions
  [ primTypes
  , primBooleanTypes
  , primCoerceTypes
  , primOrderingTypes
  , primRowTypes
  , primRowListTypes
  , primSymbolTypes
  , primIntTypes
  , primTypeErrorTypes
  ]

allPrimClasses :: Map (Qualified (ProperName ClassName)) TypeClassData
allPrimClasses = Map.unions
  [ primClasses
  , primCoerceClasses
  , primRowClasses
  , primRowListClasses
  , primSymbolClasses
  , primIntClasses
  , primTypeErrorClasses
  ]

primName :: String -> Qualified (ProperName TypeName)
primName n = Qualified (ByModuleName (ModuleName "Prim")) (ProperName n)

tyFunction :: SourceType
tyFunction = srcTypeConstructor (primName "Function")

tyRecord :: SourceType
tyRecord = srcTypeConstructor (primName "Record")

function :: SourceType -> SourceType -> SourceType
function a b = TypeApp nullSourceAnn (TypeApp nullSourceAnn tyFunction a) b

kindRow :: SourceType -> SourceType
kindRow = TypeApp nullSourceAnn (srcTypeConstructor (primName "Row"))

dictTypeName :: forall a b. ProperName a -> ProperName b
dictTypeName (ProperName s) = ProperName (s <> "$Dict")

-- Kind-level helpers (must not import Kinds.purs to avoid circular dependency)
kindType :: SourceType
kindType = srcTypeConstructor (primName "Type")

kindConstraint :: SourceType
kindConstraint = srcTypeConstructor (primName "Constraint")

-- Dummy entry for prim types where the kind doesn't matter for name resolution
dummyTypeEntry :: Tuple SourceType TypeKind
dummyTypeEntry = Tuple kindType (ExternData [])

dummyClassEntry :: TypeClassData
dummyClassEntry = makeTypeClassData [] [] [] [] false

-- Coerce a qualified class name to a qualified type name
coerceQual :: Qualified (ProperName ClassName) -> Qualified (ProperName TypeName)
coerceQual (Qualified qb pn) = Qualified qb (coerceProperName pn)

primQualTy :: ModuleName -> String -> Qualified (ProperName TypeName)
primQualTy mn n = Qualified (ByModuleName mn) (ProperName n)

primQualCls :: ModuleName -> String -> Qualified (ProperName ClassName)
primQualCls mn n = Qualified (ByModuleName mn) (ProperName n)

primTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
primTypes = Map.fromFoldable
  [ Tuple (primQualTy mPrim "Type")       (Tuple kindType                                             (ExternData []))
  , Tuple (primQualTy mPrim "Constraint") (Tuple kindType                                             (ExternData []))
  , Tuple (primQualTy mPrim "Symbol")     (Tuple kindType                                             (ExternData []))
  , Tuple (primQualTy mPrim "Row")        (Tuple (function kindType kindType)                         (ExternData [Phantom]))
  , Tuple (primQualTy mPrim "Function")   (Tuple (function kindType (function kindType kindType))     (ExternData [Representational, Representational]))
  , Tuple (primQualTy mPrim "Array")      (Tuple (function kindType kindType)                         (ExternData [Representational]))
  , Tuple (primQualTy mPrim "Record")     (Tuple (function (kindRow kindType) kindType)               (ExternData [Representational]))
  , Tuple (primQualTy mPrim "String")     (Tuple kindType                                             (ExternData []))
  , Tuple (primQualTy mPrim "Char")       (Tuple kindType                                             (ExternData []))
  , Tuple (primQualTy mPrim "Number")     (Tuple kindType                                             (ExternData []))
  , Tuple (primQualTy mPrim "Int")        (Tuple kindType                                             (ExternData []))
  , Tuple (primQualTy mPrim "Boolean")    (Tuple kindType                                             (ExternData []))
  , Tuple (coerceQual C.tyPartial)        (Tuple kindConstraint                                       (ExternData []))
  ]
  where mPrim = ModuleName "Prim"

primClasses :: Map (Qualified (ProperName ClassName)) TypeClassData
primClasses = Map.fromFoldable
  [ Tuple (primQualCls (ModuleName "Prim") "Partial") (makeTypeClassData [] [] [] [] true)
  ]

primBooleanTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
primBooleanTypes = Map.fromFoldable
  [ Tuple C.tyTrue  dummyTypeEntry
  , Tuple C.tyFalse dummyTypeEntry
  ]

primCoerceTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
primCoerceTypes = Map.fromFoldable
  [ Tuple (coerceQual C.tyCoercible)           dummyTypeEntry
  , Tuple (map dictTypeName (coerceQual C.tyCoercible)) dummyTypeEntry
  ]

primCoerceClasses :: Map (Qualified (ProperName ClassName)) TypeClassData
primCoerceClasses = Map.fromFoldable
  [ Tuple C.tyCoercible dummyClassEntry
  ]

primOrderingTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
primOrderingTypes = Map.fromFoldable
  [ Tuple C.tyTypeOrdering dummyTypeEntry
  , Tuple C.tyLT           dummyTypeEntry
  , Tuple C.tyEQ           dummyTypeEntry
  , Tuple C.tyGT           dummyTypeEntry
  ]

primRowTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
primRowTypes = Map.fromFoldable $ Array.concatMap primClassTyEntries
  [ C.clsRowUnion
  , C.clsRowNub
  , C.clsRowLacks
  , C.clsRowCons
  ]

primRowClasses :: Map (Qualified (ProperName ClassName)) TypeClassData
primRowClasses = Map.fromFoldable
  [ Tuple C.clsRowUnion dummyClassEntry
  , Tuple C.clsRowNub   dummyClassEntry
  , Tuple C.clsRowLacks dummyClassEntry
  , Tuple C.clsRowCons  dummyClassEntry
  ]

primRowListTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
primRowListTypes = Map.fromFoldable $
  [ Tuple C.tyRowList     dummyTypeEntry
  , Tuple C.tyRowListCons dummyTypeEntry
  , Tuple C.tyRowListNil  dummyTypeEntry
  ] <> Array.concatMap primClassTyEntries [ C.clsRowToList ]

primRowListClasses :: Map (Qualified (ProperName ClassName)) TypeClassData
primRowListClasses = Map.fromFoldable
  [ Tuple C.clsRowToList dummyClassEntry
  ]

primSymbolTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
primSymbolTypes = Map.fromFoldable $ Array.concatMap primClassTyEntries
  [ C.clsSymbolAppend
  , C.clsSymbolCompare
  , C.clsSymbolCons
  ]

primSymbolClasses :: Map (Qualified (ProperName ClassName)) TypeClassData
primSymbolClasses = Map.fromFoldable
  [ Tuple C.clsSymbolAppend  dummyClassEntry
  , Tuple C.clsSymbolCompare dummyClassEntry
  , Tuple C.clsSymbolCons    dummyClassEntry
  ]

primIntTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
primIntTypes = Map.fromFoldable $ Array.concatMap primClassTyEntries
  [ C.clsIntAdd
  , C.clsIntCompare
  , C.clsIntMul
  , C.clsIntToString
  ]

primIntClasses :: Map (Qualified (ProperName ClassName)) TypeClassData
primIntClasses = Map.fromFoldable
  [ Tuple C.clsIntAdd      dummyClassEntry
  , Tuple C.clsIntCompare  dummyClassEntry
  , Tuple C.clsIntMul      dummyClassEntry
  , Tuple C.clsIntToString dummyClassEntry
  ]

primTypeErrorTypes :: Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
primTypeErrorTypes = Map.fromFoldable $
  [ Tuple C.tyDoc        dummyTypeEntry
  , Tuple C.tyText       dummyTypeEntry
  , Tuple C.tyQuote      dummyTypeEntry
  , Tuple C.tyQuoteLabel dummyTypeEntry
  , Tuple C.tyBeside     dummyTypeEntry
  , Tuple C.tyAbove      dummyTypeEntry
  ] <> Array.concatMap primClassTyEntries [ C.clsFail, C.clsWarn ]

primTypeErrorClasses :: Map (Qualified (ProperName ClassName)) TypeClassData
primTypeErrorClasses = Map.fromFoldable
  [ Tuple C.clsFail dummyClassEntry
  , Tuple C.clsWarn dummyClassEntry
  ]

-- Given a class qualified name, produce the two type map entries (class-as-type and dict-type)
primClassTyEntries
  :: Qualified (ProperName ClassName)
  -> Array (Tuple (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind))
primClassTyEntries cls =
  let tyName = coerceQual cls
  in [ Tuple tyName dummyTypeEntry
     , Tuple (map dictTypeName tyName) dummyTypeEntry
     ]
