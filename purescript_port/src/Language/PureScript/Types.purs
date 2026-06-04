module Language.PureScript.Types
  ( SourceType
  , SourceConstraint
  , SkolemScope(..)
  , WildcardData(..)
  , TypeVarVisibility(..)
  , Type(..)
  , Constraint(..)
  , ConstraintData(..)
  , typeVarVisibilityPrefix
  , srcTUnknown
  , srcTypeVar
  , srcTypeLevelString
  , srcTypeLevelInt
  , srcTypeWildcard
  , srcTypeConstructor
  , srcTypeApp
  , srcKindApp
  , srcForAll
  , srcConstrainedType
  , srcREmpty
  , srcRCons
  , srcKindedType
  , srcConstraint
  , isREmpty
  , mapConstraintArgs
  , overConstraintArgs
  , mapConstraintArgsAll
  , getAnnForType
  , setAnnForType
  , everythingOnTypes
  , everywhereOnTypesM
  , everywhereOnTypesTopDownM
  , overConstraintArgsAll
  , RowListItem(..)
  , srcRowListItem
  , rowFromList
  , rowToList
  , freeTypeVariables
  , replaceAllTypeVars
  , quantify
  , addVisibility
  , moveQuantifiersToFront
  , replaceTypeVars
  , alignRowsWith
  , rowToSortedList
  , isREmptyKinded
  , mkForAll
  , completeBinderList
  , everywhereOnTypes
  , srcInstanceType
  ) where

import Prelude

import Control.Monad ((>=>))
import Data.Traversable (traverse)

import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Language.PureScript.AST.SourcePos (SourceAnn, SourceSpan, nullSourceAnn)
import Language.PureScript.Label (Label)
import Language.PureScript.Names (OpName, TypeOpName, ProperName, TypeName, ClassName, Qualified, coerceProperName)
import Language.PureScript.PSString (PSString)

type SourceType = Type SourceAnn
type SourceConstraint = Constraint SourceAnn

newtype SkolemScope = SkolemScope Int

derive instance eqSkolemScope :: Eq SkolemScope
derive instance ordSkolemScope :: Ord SkolemScope

instance showSkolemScope :: Show SkolemScope where
  show (SkolemScope n) = "(SkolemScope " <> show n <> ")"

instance encodeJsonSkolemScope :: EncodeJson SkolemScope where
  encodeJson (SkolemScope n) = encodeJson n

instance decodeJsonSkolemScope :: DecodeJson SkolemScope where
  decodeJson = map SkolemScope <<< decodeJson

data WildcardData
  = HoleWildcard String
  | UnnamedWildcard
  | IgnoredWildcard

derive instance eqWildcardData :: Eq WildcardData
derive instance ordWildcardData :: Ord WildcardData

instance showWildcardData :: Show WildcardData where
  show (HoleWildcard s) = "(HoleWildcard " <> show s <> ")"
  show UnnamedWildcard  = "UnnamedWildcard"
  show IgnoredWildcard  = "IgnoredWildcard"

data TypeVarVisibility
  = TypeVarVisible
  | TypeVarInvisible

derive instance eqTypeVarVisibility :: Eq TypeVarVisibility
derive instance ordTypeVarVisibility :: Ord TypeVarVisibility

instance showTypeVarVisibility :: Show TypeVarVisibility where
  show TypeVarVisible   = "TypeVarVisible"
  show TypeVarInvisible = "TypeVarInvisible"

typeVarVisibilityPrefix :: TypeVarVisibility -> String
typeVarVisibilityPrefix TypeVarVisible   = "@"
typeVarVisibilityPrefix TypeVarInvisible = ""

-- | The PureScript type language.
data Type a
  = TUnknown a Int
  | TypeVar a String
  | TypeLevelString a PSString
  | TypeLevelInt a Int
  | TypeWildcard a WildcardData
  | TypeConstructor a (Qualified (ProperName TypeName))
  | TypeOp a (Qualified (OpName TypeOpName))
  | TypeApp a (Type a) (Type a)
  | KindApp a (Type a) (Type a)
  | ForAll a TypeVarVisibility String (Maybe (Type a)) (Type a) (Maybe SkolemScope)
  | ConstrainedType a (Constraint a) (Type a)
  | Skolem a String (Maybe (Type a)) Int SkolemScope
  | REmpty a
  | RCons a Label (Type a) (Type a)
  | KindedType a (Type a) (Type a)
  | BinaryNoParensType a (Type a) (Type a) (Type a)
  | ParensInType a (Type a)

derive instance eqType :: Eq a => Eq (Type a)
derive instance ordType :: Ord a => Ord (Type a)
derive instance functorType :: Functor Type

instance showType :: Show a => Show (Type a) where
  show (TUnknown a n)            = "(TUnknown " <> show a <> " " <> show n <> ")"
  show (TypeVar a s)             = "(TypeVar " <> show a <> " " <> show s <> ")"
  show (TypeLevelString a ps)    = "(TypeLevelString " <> show a <> " " <> show ps <> ")"
  show (TypeLevelInt a n)        = "(TypeLevelInt " <> show a <> " " <> show n <> ")"
  show (TypeWildcard a wd)       = "(TypeWildcard " <> show a <> " " <> show wd <> ")"
  show (TypeConstructor a q)     = "(TypeConstructor " <> show a <> " " <> show q <> ")"
  show (TypeOp a q)              = "(TypeOp " <> show a <> " " <> show q <> ")"
  show (TypeApp a t1 t2)         = "(TypeApp " <> show a <> " " <> show t1 <> " " <> show t2 <> ")"
  show (KindApp a t1 t2)         = "(KindApp " <> show a <> " " <> show t1 <> " " <> show t2 <> ")"
  show (ForAll a vis s mk t sk)  = "(ForAll " <> show a <> " " <> show s <> " " <> show t <> ")"
  show (ConstrainedType a c t)   = "(ConstrainedType " <> show a <> " " <> show c <> " " <> show t <> ")"
  show (Skolem a s mk n sc)      = "(Skolem " <> show a <> " " <> show s <> " " <> show n <> ")"
  show (REmpty a)                = "(REmpty " <> show a <> ")"
  show (RCons a l t1 t2)         = "(RCons " <> show a <> " " <> show l <> " " <> show t1 <> " " <> show t2 <> ")"
  show (KindedType a t1 t2)      = "(KindedType " <> show a <> " " <> show t1 <> " " <> show t2 <> ")"
  show (BinaryNoParensType a t1 t2 t3) = "(BinaryNoParensType " <> show a <> " " <> show t1 <> " " <> show t2 <> " " <> show t3 <> ")"
  show (ParensInType a t)        = "(ParensInType " <> show a <> " " <> show t <> ")"

isREmpty :: forall a. Type a -> Boolean
isREmpty (REmpty _) = true
isREmpty (KindApp _ (REmpty _) _) = true
isREmpty _ = false

-- | Additional data for Partial constraints.
data ConstraintData
  = PartialConstraintData (Array (Array String)) Boolean

derive instance eqConstraintData :: Eq ConstraintData
derive instance ordConstraintData :: Ord ConstraintData

instance showConstraintData :: Show ConstraintData where
  show (PartialConstraintData rows trunc) =
    "(PartialConstraintData " <> show rows <> " " <> show trunc <> ")"

-- | A type class constraint.
data Constraint a = Constraint
  { constraintAnn      :: a
  , constraintClass    :: Qualified (ProperName ClassName)
  , constraintKindArgs :: Array (Type a)
  , constraintArgs     :: Array (Type a)
  , constraintData     :: Maybe ConstraintData
  }

derive instance eqConstraint :: Eq a => Eq (Constraint a)
derive instance ordConstraint :: Ord a => Ord (Constraint a)
derive instance functorConstraint :: Functor Constraint

instance showConstraint :: Show a => Show (Constraint a) where
  show (Constraint c) =
    "(Constraint { constraintClass: " <> show c.constraintClass <>
    ", constraintKindArgs: " <> show c.constraintKindArgs <>
    ", constraintArgs: " <> show c.constraintArgs <>
    ", constraintData: " <> show c.constraintData <> " })"

srcConstraint
  :: Qualified (ProperName ClassName)
  -> Array SourceType
  -> Array SourceType
  -> Maybe ConstraintData
  -> SourceConstraint
srcConstraint cls kindArgs args dat =
  Constraint
    { constraintAnn: nullSourceAnn
    , constraintClass: cls
    , constraintKindArgs: kindArgs
    , constraintArgs: args
    , constraintData: dat
    }

mapConstraintArgs :: forall a. (Array (Type a) -> Array (Type a)) -> Constraint a -> Constraint a
mapConstraintArgs f (Constraint c) = Constraint c { constraintArgs = f c.constraintArgs }

overConstraintArgs :: forall f a. Functor f => (Array (Type a) -> f (Array (Type a))) -> Constraint a -> f (Constraint a)
overConstraintArgs f (Constraint c) = (\args -> Constraint c { constraintArgs = args }) <$> f c.constraintArgs

mapConstraintArgsAll :: forall a. (Array (Type a) -> Array (Type a)) -> Constraint a -> Constraint a
mapConstraintArgsAll f (Constraint c) = Constraint c
  { constraintKindArgs = f c.constraintKindArgs
  , constraintArgs = f c.constraintArgs
  }

-- Smart constructors using nullSourceAnn
srcTUnknown :: Int -> SourceType
srcTUnknown = TUnknown nullSourceAnn

srcTypeVar :: String -> SourceType
srcTypeVar = TypeVar nullSourceAnn

srcTypeLevelString :: PSString -> SourceType
srcTypeLevelString = TypeLevelString nullSourceAnn

srcTypeLevelInt :: Int -> SourceType
srcTypeLevelInt = TypeLevelInt nullSourceAnn

srcTypeWildcard :: SourceType
srcTypeWildcard = TypeWildcard nullSourceAnn UnnamedWildcard

srcTypeConstructor :: Qualified (ProperName TypeName) -> SourceType
srcTypeConstructor = TypeConstructor nullSourceAnn

srcTypeApp :: SourceType -> SourceType -> SourceType
srcTypeApp = TypeApp nullSourceAnn

srcKindApp :: SourceType -> SourceType -> SourceType
srcKindApp = KindApp nullSourceAnn

srcForAll :: TypeVarVisibility -> String -> Maybe SourceType -> SourceType -> Maybe SkolemScope -> SourceType
srcForAll = ForAll nullSourceAnn

srcConstrainedType :: SourceConstraint -> SourceType -> SourceType
srcConstrainedType = ConstrainedType nullSourceAnn

srcREmpty :: SourceType
srcREmpty = REmpty nullSourceAnn

srcRCons :: Label -> SourceType -> SourceType -> SourceType
srcRCons = RCons nullSourceAnn

srcKindedType :: SourceType -> SourceType -> SourceType
srcKindedType = KindedType nullSourceAnn

getAnnForType :: forall a. Type a -> a
getAnnForType = case _ of
  TUnknown a _         -> a
  TypeVar a _          -> a
  TypeLevelString a _  -> a
  TypeLevelInt a _     -> a
  TypeWildcard a _     -> a
  TypeConstructor a _  -> a
  TypeOp a _           -> a
  TypeApp a _ _        -> a
  KindApp a _ _        -> a
  ForAll a _ _ _ _ _   -> a
  ConstrainedType a _ _-> a
  Skolem a _ _ _ _     -> a
  REmpty a             -> a
  RCons a _ _ _        -> a
  KindedType a _ _     -> a
  BinaryNoParensType a _ _ _ -> a
  ParensInType a _     -> a

setAnnForType :: forall a. a -> Type a -> Type a
setAnnForType ann = case _ of
  TUnknown _ i          -> TUnknown ann i
  TypeVar _ s           -> TypeVar ann s
  TypeLevelString _ ps  -> TypeLevelString ann ps
  TypeLevelInt _ n      -> TypeLevelInt ann n
  TypeWildcard _ w      -> TypeWildcard ann w
  TypeConstructor _ qn  -> TypeConstructor ann qn
  TypeOp _ qn           -> TypeOp ann qn
  TypeApp _ t1 t2       -> TypeApp ann t1 t2
  KindApp _ t1 t2       -> KindApp ann t1 t2
  ForAll _ vis s mb t sc -> ForAll ann vis s mb t sc
  ConstrainedType _ c t -> ConstrainedType ann c t
  Skolem _ s mb i sc    -> Skolem ann s mb i sc
  REmpty _              -> REmpty ann
  RCons _ l t1 t2       -> RCons ann l t1 t2
  KindedType _ t k      -> KindedType ann t k
  BinaryNoParensType _ op t1 t2 -> BinaryNoParensType ann op t1 t2
  ParensInType _ t      -> ParensInType ann t

everythingOnTypes :: forall r a. (r -> r -> r) -> (Type a -> r) -> Type a -> r
everythingOnTypes combine f = go
  where
  go t@(TypeApp _ t1 t2) = combine (combine (f t) (go t1)) (go t2)
  go t@(KindApp _ t1 t2) = combine (combine (f t) (go t1)) (go t2)
  go t@(ForAll _ _ _ (Just k) ty _) = combine (combine (f t) (go k)) (go ty)
  go t@(ForAll _ _ _ _ ty _) = combine (f t) (go ty)
  go t@(ConstrainedType _ (Constraint c) ty) =
    let r0 = f t
        r1 = foldl combine r0 (map go c.constraintKindArgs)
        r2 = foldl combine r1 (map go c.constraintArgs)
    in combine r2 (go ty)
  go t@(Skolem _ _ (Just k) _ _) = combine (f t) (go k)
  go t@(RCons _ _ ty rest) = combine (combine (f t) (go ty)) (go rest)
  go t@(KindedType _ ty k) = combine (combine (f t) (go ty)) (go k)
  go t@(BinaryNoParensType _ t1 t2 t3) = combine (combine (combine (f t) (go t1)) (go t2)) (go t3)
  go t@(ParensInType _ t1) = combine (f t) (go t1)
  go other = f other

overConstraintArgsAll :: forall f a. Applicative f => (Array (Type a) -> f (Array (Type a))) -> Constraint a -> f (Constraint a)
overConstraintArgsAll f (Constraint c) =
  (\ka a -> Constraint c { constraintKindArgs = ka, constraintArgs = a })
    <$> f c.constraintKindArgs
    <*> f c.constraintArgs

everywhereOnTypesM :: forall m a. Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesM f = go
  where
  go (TypeApp ann t1 t2) = (TypeApp ann <$> go t1 <*> go t2) >>= f
  go (KindApp ann t1 t2) = (KindApp ann <$> go t1 <*> go t2) >>= f
  go (ForAll ann vis arg mbK ty sco) = (ForAll ann vis arg <$> traverse go mbK <*> go ty <*> pure sco) >>= f
  go (ConstrainedType ann c ty) = (ConstrainedType ann <$> overConstraintArgsAll (traverse go) c <*> go ty) >>= f
  go (Skolem ann name mbK i sc) = (Skolem ann name <$> traverse go mbK <*> pure i <*> pure sc) >>= f
  go (RCons ann name ty rest) = (RCons ann name <$> go ty <*> go rest) >>= f
  go (KindedType ann ty k) = (KindedType ann <$> go ty <*> go k) >>= f
  go (BinaryNoParensType ann t1 t2 t3) = (BinaryNoParensType ann <$> go t1 <*> go t2 <*> go t3) >>= f
  go (ParensInType ann t) = (ParensInType ann <$> go t) >>= f
  go other = f other

everywhereOnTypesTopDownM :: forall m a. Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesTopDownM f = (go <=< f)
  where
  go (TypeApp ann t1 t2) = TypeApp ann <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (KindApp ann t1 t2) = KindApp ann <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (ForAll ann vis arg mbK ty sco) = ForAll ann vis arg <$> traverse (f >=> go) mbK <*> (f ty >>= go) <*> pure sco
  go (ConstrainedType ann c ty) = ConstrainedType ann <$> overConstraintArgsAll (traverse (go <=< f)) c <*> (f ty >>= go)
  go (Skolem ann name mbK i sc) = Skolem ann name <$> traverse (f >=> go) mbK <*> pure i <*> pure sc
  go (RCons ann name ty rest) = RCons ann name <$> (f ty >>= go) <*> (f rest >>= go)
  go (KindedType ann ty k) = KindedType ann <$> (f ty >>= go) <*> (f k >>= go)
  go (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann <$> (f t1 >>= go) <*> (f t2 >>= go) <*> (f t3 >>= go)
  go (ParensInType ann t) = ParensInType ann <$> (f t >>= go)
  go other = pure other

data RowListItem a = RowListItem
  { rowListAnn  :: a
  , rowListLabel :: Label
  , rowListType  :: Type a
  }

srcRowListItem :: Label -> SourceType -> RowListItem SourceAnn
srcRowListItem lbl ty = RowListItem { rowListAnn: nullSourceAnn, rowListLabel: lbl, rowListType: ty }

rowFromList :: forall a. Tuple (Array (RowListItem a)) (Type a) -> Type a
rowFromList (Tuple xs r) = foldr (\(RowListItem item) acc -> RCons item.rowListAnn item.rowListLabel item.rowListType acc) r xs

rowToList :: forall a. Type a -> Tuple (Array (RowListItem a)) (Type a)
rowToList ty = go [] ty
  where
  go acc (RCons ann name ty' rest) = go (Array.snoc acc (RowListItem { rowListAnn: ann, rowListLabel: name, rowListType: ty' })) rest
  go acc t = Tuple acc t

freeTypeVariables :: forall a. Type a -> Array String
freeTypeVariables = Array.fromFoldable <<< go Set.empty
  where
  go :: Set String -> Type a -> Set String
  go bound (TypeVar _ v) | not (Set.member v bound) = Set.singleton v
  go bound (TypeVar _ _) = Set.empty
  go bound (TypeApp _ t1 t2) = Set.union (go bound t1) (go bound t2)
  go bound (KindApp _ t1 t2) = Set.union (go bound t1) (go bound t2)
  go bound (ForAll _ _ arg _ ty _) = go (Set.insert arg bound) ty
  go bound (ConstrainedType _ (Constraint c) ty) =
    Set.union (Array.foldl (\acc t -> Set.union acc (go bound t)) Set.empty (c.constraintKindArgs <> c.constraintArgs))
              (go bound ty)
  go bound (RCons _ _ ty rest) = Set.union (go bound ty) (go bound rest)
  go bound (KindedType _ ty k) = Set.union (go bound ty) (go bound k)
  go _ _ = Set.empty

replaceAllTypeVars :: forall a. Array (Tuple String (Type a)) -> Type a -> Type a
replaceAllTypeVars = go []
  where
  go :: Array String -> Array (Tuple String (Type a)) -> Type a -> Type a
  go bound replacements t@(TypeVar _ v) =
    case Array.find (\(Tuple k _) -> k == v && not (Array.elem v bound)) replacements of
      Just (Tuple _ r) -> r
      Nothing -> t
  go bound replacements (TypeApp ann t1 t2) =
    TypeApp ann (go bound replacements t1) (go bound replacements t2)
  go bound replacements (KindApp ann t1 t2) =
    KindApp ann (go bound replacements t1) (go bound replacements t2)
  go bound replacements (ForAll ann vis arg mbK ty sco) =
    let replacements' = Array.filter (\(Tuple k _) -> k /= arg) replacements
    in ForAll ann vis arg (map (go (Array.cons arg bound) replacements') mbK)
         (go (Array.cons arg bound) replacements' ty) sco
  go bound replacements (ConstrainedType ann (Constraint c) ty) =
    ConstrainedType ann
      (Constraint c
        { constraintKindArgs = map (go bound replacements) c.constraintKindArgs
        , constraintArgs     = map (go bound replacements) c.constraintArgs
        })
      (go bound replacements ty)
  go bound replacements (RCons ann name ty rest) =
    RCons ann name (go bound replacements ty) (go bound replacements rest)
  go bound replacements (KindedType ann ty k) =
    KindedType ann (go bound replacements ty) (go bound replacements k)
  go _ _ t = t

quantify :: SourceType -> SourceType
quantify ty =
  let fvs = freeTypeVariables ty
  in foldr (\arg t -> ForAll nullSourceAnn TypeVarInvisible arg Nothing t Nothing) ty fvs

addVisibility :: forall a. Array (Tuple String TypeVarVisibility) -> Type a -> Type a
addVisibility vis = go
  where
  go (ForAll ann _ arg mbK ty sco) =
    let vis' = case Array.find (\(Tuple k _) -> k == arg) vis of
                 Just (Tuple _ v) -> v
                 Nothing -> TypeVarInvisible
    in ForAll ann vis' arg mbK (go ty) sco
  go t = t

moveQuantifiersToFront :: forall a. a -> Type a -> Type a
moveQuantifiersToFront syntheticAnn = collectQuantifiers []
  where
  collectQuantifiers :: Array (Tuple (Tuple TypeVarVisibility String) (Maybe (Type a))) -> Type a -> Type a
  collectQuantifiers acc (ForAll _ vis arg mbK ty _) =
    collectQuantifiers (Array.snoc acc (Tuple (Tuple vis arg) mbK)) ty
  collectQuantifiers acc (ConstrainedType ann c ty) =
    ConstrainedType ann c (collectQuantifiers acc ty)
  collectQuantifiers acc ty =
    foldr (\(Tuple (Tuple vis arg) mbK) t -> ForAll syntheticAnn vis arg mbK t Nothing) ty acc

replaceTypeVars :: forall a. String -> Type a -> Type a -> Type a
replaceTypeVars v r = replaceAllTypeVars [Tuple v r]

everywhereOnTypes :: forall a. (Type a -> Type a) -> Type a -> Type a
everywhereOnTypes f = go
  where
  go (TypeApp ann t1 t2) = f (TypeApp ann (go t1) (go t2))
  go (KindApp ann t1 t2) = f (KindApp ann (go t1) (go t2))
  go (ForAll ann vis arg mbK ty sco) = f (ForAll ann vis arg (map go mbK) (go ty) sco)
  go (ConstrainedType ann (Constraint c) ty) =
    f (ConstrainedType ann
        (Constraint c
          { constraintKindArgs = map go c.constraintKindArgs
          , constraintArgs = map go c.constraintArgs
          })
        (go ty))
  go (Skolem ann name mbK i sc) = f (Skolem ann name (map go mbK) i sc)
  go (RCons ann name ty rest) = f (RCons ann name (go ty) (go rest))
  go (KindedType ann ty k) = f (KindedType ann (go ty) (go k))
  go (BinaryNoParensType ann t1 t2 t3) = f (BinaryNoParensType ann (go t1) (go t2) (go t3))
  go (ParensInType ann t) = f (ParensInType ann (go t))
  go other = f other

rowToSortedList :: forall a. Type a -> Tuple (Array (RowListItem a)) (Type a)
rowToSortedList ty =
  let Tuple items tail = rowToList ty
  in Tuple (Array.sortBy (\(RowListItem a) (RowListItem b) -> compare a.rowListLabel b.rowListLabel) items) tail

alignRowsWith
  :: forall a r
   . (Label -> Type a -> Type a -> r)
  -> Type a
  -> Type a
  -> Tuple (Array r) (Tuple (Tuple (Array (RowListItem a)) (Type a)) (Tuple (Array (RowListItem a)) (Type a)))
alignRowsWith f ty1 ty2 = go s1 s2
  where
  Tuple s1 tail1 = rowToSortedList ty1
  Tuple s2 tail2 = rowToSortedList ty2

  go :: Array (RowListItem a) -> Array (RowListItem a) -> Tuple (Array r) (Tuple (Tuple (Array (RowListItem a)) (Type a)) (Tuple (Array (RowListItem a)) (Type a)))
  go lhs rhs = case Tuple (Array.uncons lhs) (Array.uncons rhs) of
    Tuple Nothing _ -> Tuple [] (Tuple (Tuple [] tail1) (Tuple rhs tail2))
    Tuple _ Nothing -> Tuple [] (Tuple (Tuple lhs tail1) (Tuple [] tail2))
    Tuple (Just { head: RowListItem a1, tail: r1 }) (Just { head: RowListItem a2, tail: r2 }) ->
      case compare a1.rowListLabel a2.rowListLabel of
        LT ->
          let Tuple ms (Tuple (Tuple lft rl) rr) = go r1 rhs
          in Tuple ms (Tuple (Tuple (Array.cons (RowListItem a1) lft) rl) rr)
        GT ->
          let Tuple ms (Tuple lr (Tuple rft rr)) = go lhs r2
          in Tuple ms (Tuple lr (Tuple (Array.cons (RowListItem a2) rft) rr))
        EQ ->
          let Tuple ms rest = go r1 r2
          in Tuple (Array.cons (f a1.rowListLabel a1.rowListType a2.rowListType) ms) rest

isREmptyKinded :: forall a. Type a -> Boolean
isREmptyKinded (REmpty _) = true
isREmptyKinded (KindApp _ (REmpty _) _) = true
isREmptyKinded _ = false

mkForAll :: forall a. Array (Tuple a (Tuple String (Maybe (Type a)))) -> Type a -> Type a
mkForAll args ty = foldr (\(Tuple ann (Tuple arg mbK)) t -> ForAll ann TypeVarInvisible arg mbK t Nothing) ty args

-- | Construct the type of an instance declaration from its parts.
-- | Used in error messages describing unnamed instances.
srcInstanceType
  :: SourceSpan
  -> Array (Tuple String SourceType)
  -> Qualified (ProperName ClassName)
  -> Array SourceType
  -> SourceType
srcInstanceType ss vars className tys =
  setAnnForType (Tuple ss [])
    $ foldr (\(Tuple tv k) ty -> srcForAll TypeVarInvisible tv (Just k) ty Nothing) base vars
  where
  base :: SourceType
  base = foldl srcTypeApp (srcTypeConstructor (map coerceProperName className)) tys

completeBinderList :: forall a. Type a -> Maybe (Tuple (Array (Tuple a (Tuple String (Type a)))) (Type a))
completeBinderList = go []
  where
  go acc (ForAll _ _ _ Nothing _ _) = Nothing
  go acc (ForAll ann _ var (Just k) ty _) = go (Array.snoc acc (Tuple ann (Tuple var k))) ty
  go acc ty = Just (Tuple acc ty)
