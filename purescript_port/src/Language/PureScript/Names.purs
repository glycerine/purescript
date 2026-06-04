module Language.PureScript.Names
  ( Name(..)
  , InternalIdentData(..)
  , Ident(..)
  , OpName(..)
  , OpNameType
  , ValueOpName
  , TypeOpName
  , AnyOpName
  , ProperName(..)
  , ProperNameType
  , TypeName
  , ConstructorName
  , ClassName
  , Namespace
  , ModuleName(..)
  , QualifiedBy(..)
  , Qualified(..)
  , getIdentName
  , getValOpName
  , getTypeName
  , getTypeOpName
  , getDctorName
  , getClassName
  , unusedIdent
  , runIdent
  , showIdent
  , isPlainIdent
  , runOpName
  , showOp
  , eraseOpName
  , coerceOpName
  , runProperName
  , coerceProperName
  , runModuleName
  , moduleNameFromString
  , isBuiltinModuleName
  , isBySourcePos
  , byMaybeModuleName
  , toMaybeModuleName
  , showQualified
  , getQual
  , qualify
  , mkQualified
  , disqualify
  , disqualifyFor
  , isQualified
  , isUnqualified
  , isQualifiedWith
  , byNullSourcePos
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Language.PureScript.AST.SourcePos (SourcePos(..))

-- | Phantom types for ProperName tags
foreign import data TypeName :: Type
foreign import data ConstructorName :: Type
foreign import data ClassName :: Type
foreign import data Namespace :: Type

-- | The closed set of proper name types (used as phantom params).
type ProperNameType = Type

-- | Phantom types for OpName tags
foreign import data ValueOpName :: Type
foreign import data TypeOpName :: Type
foreign import data AnyOpName :: Type

-- Phantom types are never instantiated as values; these instances are trivially safe.
instance eqValueOpName :: Eq ValueOpName where eq _ _ = true
instance ordValueOpName :: Ord ValueOpName where compare _ _ = EQ
instance eqTypeOpName :: Eq TypeOpName where eq _ _ = true
instance ordTypeOpName :: Ord TypeOpName where compare _ _ = EQ
instance eqAnyOpName :: Eq AnyOpName where eq _ _ = true
instance ordAnyOpName :: Ord AnyOpName where compare _ _ = EQ

type OpNameType = Type

-- | A sum of all name types, for error messages.
data Name
  = IdentName Ident
  | ValOpName (OpName ValueOpName)
  | TyName (ProperName TypeName)
  | TyOpName (OpName TypeOpName)
  | DctorName (ProperName ConstructorName)
  | TyClassName (ProperName ClassName)
  | ModName ModuleName

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

instance showName :: Show Name where
  show (IdentName i)    = "(IdentName " <> show i <> ")"
  show (ValOpName o)    = "(ValOpName " <> show o <> ")"
  show (TyName n)       = "(TyName " <> show n <> ")"
  show (TyOpName o)     = "(TyOpName " <> show o <> ")"
  show (DctorName n)    = "(DctorName " <> show n <> ")"
  show (TyClassName n)  = "(TyClassName " <> show n <> ")"
  show (ModName m)      = "(ModName " <> show m <> ")"

getIdentName :: Name -> Maybe Ident
getIdentName (IdentName n) = Just n
getIdentName _ = Nothing

getValOpName :: Name -> Maybe (OpName ValueOpName)
getValOpName (ValOpName n) = Just n
getValOpName _ = Nothing

getTypeName :: Name -> Maybe (ProperName TypeName)
getTypeName (TyName n) = Just n
getTypeName _ = Nothing

getTypeOpName :: Name -> Maybe (OpName TypeOpName)
getTypeOpName (TyOpName n) = Just n
getTypeOpName _ = Nothing

getDctorName :: Name -> Maybe (ProperName ConstructorName)
getDctorName (DctorName n) = Just n
getDctorName _ = Nothing

getClassName :: Name -> Maybe (ProperName ClassName)
getClassName (TyClassName n) = Just n
getClassName _ = Nothing

-- | Internal identifier payloads (for compiler-generated names).
data InternalIdentData
  = RuntimeLazyFactory
  | Lazy String

derive instance eqInternalIdentData :: Eq InternalIdentData
derive instance ordInternalIdentData :: Ord InternalIdentData

instance showInternalIdentData :: Show InternalIdentData where
  show RuntimeLazyFactory = "RuntimeLazyFactory"
  show (Lazy t)           = "(Lazy " <> show t <> ")"

instance encodeJsonInternalIdentData :: EncodeJson InternalIdentData where
  encodeJson RuntimeLazyFactory = encodeJson { "RuntimeLazyFactory": {} }
  encodeJson (Lazy t)           = encodeJson { "Lazy": t }

instance decodeJsonInternalIdentData :: DecodeJson InternalIdentData where
  decodeJson json = do
    obj <- decodeJson json
    let tryRLF = do
          _ <- obj .: "RuntimeLazyFactory" :: Either JsonDecodeError {}
          pure RuntimeLazyFactory
        tryLazy = Lazy <$> (obj .: "Lazy")
    case tryRLF of
      Right r -> Right r
      Left _  -> tryLazy

-- | Value identifier names.
data Ident
  = Ident String
  | GenIdent (Maybe String) Int
  | UnusedIdent
  | InternalIdent InternalIdentData

derive instance eqIdent :: Eq Ident
derive instance ordIdent :: Ord Ident

instance showIdentInst :: Show Ident where
  show (Ident s)             = "(Ident " <> show s <> ")"
  show (GenIdent mn n)       = "(GenIdent " <> show mn <> " " <> show n <> ")"
  show UnusedIdent           = "UnusedIdent"
  show (InternalIdent d)     = "(InternalIdent " <> show d <> ")"

unusedIdent :: String
unusedIdent = "$__unused"

runIdent :: Ident -> String
runIdent (Ident i)             = i
runIdent (GenIdent Nothing n)  = "$" <> show n
runIdent (GenIdent (Just nm) n) = "$" <> nm <> show n
runIdent UnusedIdent           = unusedIdent
runIdent (InternalIdent _)     = "<internal>"

showIdent :: Ident -> String
showIdent = runIdent

isPlainIdent :: Ident -> Boolean
isPlainIdent (Ident _) = true
isPlainIdent _         = false

instance encodeJsonIdent :: EncodeJson Ident where
  encodeJson (Ident s)              = encodeJson { "Ident": s }
  encodeJson (GenIdent mn n)        = encodeJson { "GenIdent": encodeJson (Tuple mn n) }
  encodeJson UnusedIdent            = encodeJson { "UnusedIdent": {} }
  encodeJson (InternalIdent d)      = encodeJson { "InternalIdent": d }

instance decodeJsonIdent :: DecodeJson Ident where
  decodeJson json = do
    obj <- decodeJson json
    let tryIdent    = Ident <$> (obj .: "Ident")
        tryGen      = do
          pair <- obj .: "GenIdent"
          Tuple mn n <- decodeJson pair
          pure $ GenIdent mn n
        tryUnused   = do
          _ <- obj .: "UnusedIdent" :: Either JsonDecodeError {}
          pure UnusedIdent
        tryInternal = InternalIdent <$> (obj .: "InternalIdent")
    case tryIdent of
      Right r -> Right r
      Left _  -> case tryGen of
        Right r -> Right r
        Left _  -> case tryUnused of
          Right r -> Right r
          Left _  -> tryInternal

-- | Operator alias names (phantom-tagged by OpNameType).
newtype OpName :: Type -> Type
newtype OpName a = OpName String

runOpName :: forall a. OpName a -> String
runOpName (OpName s) = s

showOp :: forall a. OpName a -> String
showOp op = "(" <> runOpName op <> ")"

eraseOpName :: forall a. OpName a -> OpName AnyOpName
eraseOpName (OpName s) = OpName s

coerceOpName :: forall a b. OpName a -> OpName b
coerceOpName (OpName s) = OpName s

derive instance eqOpName :: Eq (OpName a)
derive instance ordOpName :: Ord (OpName a)

instance showOpName :: Show (OpName a) where
  show (OpName s) = "(OpName " <> show s <> ")"

instance encodeJsonOpName :: EncodeJson (OpName a) where
  encodeJson = encodeJson <<< runOpName

instance decodeJsonOpName :: DecodeJson (OpName a) where
  decodeJson = map OpName <<< decodeJson

-- | Proper names (capitalized, phantom-tagged by ProperNameType).
newtype ProperName :: Type -> Type
newtype ProperName a = ProperName String

runProperName :: forall a. ProperName a -> String
runProperName (ProperName s) = s

coerceProperName :: forall a b. ProperName a -> ProperName b
coerceProperName (ProperName s) = ProperName s

derive instance eqProperName :: Eq (ProperName a)
derive instance ordProperName :: Ord (ProperName a)

instance showProperName :: Show (ProperName a) where
  show (ProperName s) = "(ProperName " <> show s <> ")"

instance encodeJsonProperName :: EncodeJson (ProperName a) where
  encodeJson = encodeJson <<< runProperName

instance decodeJsonProperName :: DecodeJson (ProperName a) where
  decodeJson = map ProperName <<< decodeJson

-- | Module names (dot-separated proper names, e.g. "Data.Map.Internal").
newtype ModuleName = ModuleName String

runModuleName :: ModuleName -> String
runModuleName (ModuleName n) = n

moduleNameFromString :: String -> ModuleName
moduleNameFromString = ModuleName

isBuiltinModuleName :: ModuleName -> Boolean
isBuiltinModuleName (ModuleName mn) =
  mn == "Prim" || String.take 5 mn == "Prim."

derive instance eqModuleName :: Eq ModuleName
derive instance ordModuleName :: Ord ModuleName

instance showModuleName :: Show ModuleName where
  show (ModuleName n) = "(ModuleName " <> show n <> ")"

-- | JSON encoding for ModuleName: array of parts, e.g. ["Data","Map","Internal"]
instance encodeJsonModuleName :: EncodeJson ModuleName where
  encodeJson (ModuleName n) = encodeJson (String.split (String.Pattern ".") n)

instance decodeJsonModuleName :: DecodeJson ModuleName where
  decodeJson json = do
    parts <- decodeJson json :: Either JsonDecodeError (Array String)
    pure $ ModuleName (String.joinWith "." parts)

-- | How a name is qualified: by source position (unqualified) or module name.
data QualifiedBy
  = BySourcePos SourcePos
  | ByModuleName ModuleName

derive instance eqQualifiedBy :: Eq QualifiedBy
derive instance ordQualifiedBy :: Ord QualifiedBy

instance showQualifiedBy :: Show QualifiedBy where
  show (BySourcePos sp) = "(BySourcePos " <> show sp <> ")"
  show (ByModuleName mn) = "(ByModuleName " <> show mn <> ")"

isBySourcePos :: QualifiedBy -> Boolean
isBySourcePos (BySourcePos _) = true
isBySourcePos _ = false

byMaybeModuleName :: Maybe ModuleName -> QualifiedBy
byMaybeModuleName (Just mn) = ByModuleName mn
byMaybeModuleName Nothing   = BySourcePos (SourcePos { line: 0, column: 0 })

toMaybeModuleName :: QualifiedBy -> Maybe ModuleName
toMaybeModuleName (ByModuleName mn) = Just mn
toMaybeModuleName (BySourcePos _)   = Nothing

-- | A name optionally qualified with a module name.
data Qualified a = Qualified QualifiedBy a

derive instance eqQualified :: Eq a => Eq (Qualified a)
derive instance ordQualified :: Ord a => Ord (Qualified a)
derive instance functorQualified :: Functor Qualified

instance showQualified' :: Show a => Show (Qualified a) where
  show (Qualified qb a) = "(Qualified " <> show qb <> " " <> show a <> ")"

showQualified :: forall a. (a -> String) -> Qualified a -> String
showQualified f (Qualified (BySourcePos _) a) = f a
showQualified f (Qualified (ByModuleName mn) a) = runModuleName mn <> "." <> f a

getQual :: forall a. Qualified a -> Maybe ModuleName
getQual (Qualified qb _) = toMaybeModuleName qb

qualify :: forall a. ModuleName -> Qualified a -> Tuple ModuleName a
qualify m (Qualified (BySourcePos _) a)  = Tuple m a
qualify _ (Qualified (ByModuleName m) a) = Tuple m a

mkQualified :: forall a. a -> ModuleName -> Qualified a
mkQualified name mn = Qualified (ByModuleName mn) name

disqualify :: forall a. Qualified a -> a
disqualify (Qualified _ a) = a

disqualifyFor :: forall a. Maybe ModuleName -> Qualified a -> Maybe a
disqualifyFor mn (Qualified qb a) | mn == toMaybeModuleName qb = Just a
disqualifyFor _ _ = Nothing

isQualified :: forall a. Qualified a -> Boolean
isQualified (Qualified (BySourcePos _) _) = false
isQualified _ = true

isUnqualified :: forall a. Qualified a -> Boolean
isUnqualified = not <<< isQualified

isQualifiedWith :: forall a. ModuleName -> Qualified a -> Boolean
isQualifiedWith mn (Qualified (ByModuleName mn') _) = mn == mn'
isQualifiedWith _ _ = false

instance encodeJsonQualified :: EncodeJson a => EncodeJson (Qualified a) where
  encodeJson (Qualified (ByModuleName mn) a) = encodeJson (Tuple mn a)
  encodeJson (Qualified (BySourcePos sp) a)  = encodeJson (Tuple sp a)

instance decodeJsonQualified :: DecodeJson a => DecodeJson (Qualified a) where
  decodeJson json = do
    Tuple first second <- decodeJson json
    -- Try parsing first as ModuleName (array of strings), then as SourcePos (array of ints)
    let byModule = do
          mn <- decodeJson first
          pure $ Qualified (ByModuleName mn) second
        bySourcePos = do
          sp <- decodeJson first
          pure $ Qualified (BySourcePos sp) second
        byMaybe = do
          mn <- decodeJson first
          pure $ Qualified (byMaybeModuleName mn) second
    case byModule of
      Right r -> Right r
      Left _  -> case bySourcePos of
        Right r -> Right r
        Left _  -> byMaybe

byNullSourcePos :: QualifiedBy
byNullSourcePos = BySourcePos (SourcePos { line: 0, column: 0 })
