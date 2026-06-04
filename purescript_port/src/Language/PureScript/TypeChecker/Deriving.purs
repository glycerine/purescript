-- | Derived type class instance generation.
-- | Port of Language.PureScript.TypeChecker.Deriving from Haskell.
module Language.PureScript.TypeChecker.Deriving
  ( deriveInstance
  ) where

import Prelude
import Control.Alt ((<|>))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

import Control.Monad.Error.Class (class MonadError, throwError, liftEither)
import Control.Monad.State.Class (class MonadState, gets)
import Control.Monad.Supply.Class (class MonadSupply, freshIdent)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT(..), runWriterT)
import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array as Array
import Data.Array (uncons, last, init, (!!))
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr, for_, any)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Traversable (traverse, for, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Void (Void, absurd)
import Unsafe.Coerce as Unsafe.Coerce

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , ErrorMessageHint(..)
  , Expr(..)
  , GuardedExpr(..)
  , Guard(..)
  , InstanceDerivationStrategy(..)
  )
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceSpan, nullSourceSpan, nullSourceAnn)
import Language.PureScript.Constants.Libs as Libs
import Language.PureScript.Constants.Prim as Prim
import Language.PureScript.Environment
  ( DataDeclType(..)
  , Environment(..)
  , FunctionalDependency(..)
  , TypeClassData(..)
  , TypeKind(..)
  )
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage
  , internalCompilerError
  )
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , Ident(..)
  , ModuleName(..)
  , Name(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , byNullSourcePos
  , coerceProperName
  , qualify
  )
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.Sugar.TypeClasses (superClassDictionaryNames)
import Language.PureScript.TypeChecker.Entailment (InstanceContext, findDicts)
import Language.PureScript.TypeChecker.Monad
  ( CheckState(..)
  , getEnv
  , getTypeClassDictionaries
  , unsafeCheckCurrentModule
  )
import Language.PureScript.TypeChecker.Synonyms (replaceAllTypeSynonyms)
import Language.PureScript.TypeClassDictionaries (TypeClassDictionaryInScope(..))
import Language.PureScript.Types
  ( Constraint(..)
  , SourceType
  , Type(..)
  , completeBinderList
  , everythingOnTypes
  , replaceAllTypeVars
  , srcTypeVar
  )

-- ---------------------------------------------------------------------------
-- Local These type (no purescript-these package available)
-- ---------------------------------------------------------------------------

data These a b
  = This a
  | That b
  | Both a b

both :: forall a b. (a -> b) -> These a a -> These b b
both f = case _ of
  This a -> This (f a)
  That b -> That (f b)
  Both a b -> Both (f a) (f b)

-- | Apply f to the left part, g to the right part
mapThese :: forall a b c d. (a -> c) -> (b -> d) -> These a b -> These c d
mapThese f g = case _ of
  This a -> This (f a)
  That b -> That (g b)
  Both a b -> Both (f a) (g b)

these :: forall a b c. (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these f _ _ (This a) = f a
these _ g _ (That b) = g b
these _ _ h (Both a b) = h a b

-- | align two Maybes into These
alignMaybes :: forall a b. Maybe a -> Maybe b -> Maybe (These a b)
alignMaybes Nothing Nothing = Nothing
alignMaybes (Just a) Nothing = Just (This a)
alignMaybes Nothing (Just b) = Just (That b)
alignMaybes (Just a) (Just b) = Just (Both a b)

-- | unalign: split These into two Maybe
unalignThese :: forall a b. These a b -> Tuple (Maybe a) (Maybe b)
unalignThese (This a) = Tuple (Just a) Nothing
unalignThese (That b) = Tuple Nothing (Just b)
unalignThese (Both a b) = Tuple (Just a) (Just b)

-- | Return, if possible, a These the contents of which each satisfy the predicate.
filterThese :: forall a. (a -> Boolean) -> These a a -> Maybe (These a a)
filterThese p t =
  let Tuple ma mb = unalignThese t
      ma' = Array.filter p (Array.fromFoldable ma)
      mb' = Array.filter p (Array.fromFoldable mb)
  in alignMaybes (Array.head ma') (Array.head mb')

-- | Traverse both sides of These with an applicative
traverseBothThese :: forall m a b. Applicative m => (a -> m b) -> These a a -> m (These b b)
traverseBothThese f = case _ of
  This a -> This <$> f a
  That b -> That <$> f b
  Both a b -> Both <$> f a <*> f b

-- ---------------------------------------------------------------------------
-- Const functor for accumulating things
-- ---------------------------------------------------------------------------

newtype Const r a = Const (Array r)

getConst :: forall r a. Const r a -> Array r
getConst (Const xs) = xs

instance functorConst :: Functor (Const r) where
  map _ (Const xs) = Const xs

instance applyConst :: Apply (Const r) where
  apply (Const f) (Const x) = Const (f <> x)

instance applicativeConst :: Applicative (Const r) where
  pure _ = Const []

toConst :: forall f a b. f a -> Const (f a) b
toConst x = Const [x]

consumeConst :: forall f a b c. Applicative f => (Array a -> b) -> Const (f a) c -> f b
consumeConst f (Const xs) = f <$> sequence xs

-- ---------------------------------------------------------------------------
-- Unwrapped type constructor
-- ---------------------------------------------------------------------------

data UnwrappedTypeConstructor = UnwrappedTypeConstructor
  { utcModuleName :: ModuleName
  , utcTyCon      :: ProperName TypeName
  , utcKindArgs   :: Array SourceType
  , utcArgs       :: Array SourceType
  }

utcQTyCon :: UnwrappedTypeConstructor -> Qualified (ProperName TypeName)
utcQTyCon (UnwrappedTypeConstructor utc) = Qualified (ByModuleName utc.utcModuleName) utc.utcTyCon

unwrapTypeConstructor :: SourceType -> Maybe UnwrappedTypeConstructor
unwrapTypeConstructor = go [] []
  where
  go kargs args = case _ of
    TypeConstructor _ (Qualified (ByModuleName mn) tyCon) ->
      Just (UnwrappedTypeConstructor { utcModuleName: mn, utcTyCon: tyCon, utcKindArgs: kargs, utcArgs: args })
    TypeApp _ ty arg -> go kargs (Array.cons arg args) ty
    KindApp _ ty karg -> go (Array.cons karg kargs) args ty
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- AST utilities
-- ---------------------------------------------------------------------------

mkRef :: Qualified Ident -> Expr
mkRef = Var nullSourceSpan

mkVar :: Ident -> Expr
mkVar = mkRef <<< Qualified byNullSourcePos

mkBinder :: Ident -> Binder
mkBinder = VarBinder nullSourceSpan

mkLit :: Literal Expr -> Expr
mkLit = Literal nullSourceSpan

mkCtor :: ModuleName -> ProperName ConstructorName -> Expr
mkCtor mn name = Constructor nullSourceSpan (Qualified (ByModuleName mn) name)

mkCtorBinder :: ModuleName -> ProperName ConstructorName -> Array Binder -> Binder
mkCtorBinder mn name = ConstructorBinder nullSourceSpan (Qualified (ByModuleName mn) name)

unguarded :: Expr -> Array GuardedExpr
unguarded e = [GuardedExpr [] e]

lam :: Ident -> Expr -> Expr
lam i = Abs (mkBinder i)

lamCase :: Ident -> Array CaseAlternative -> Expr
lamCase s alts = lam s (Case [mkVar s] alts)

lamCase2 :: Ident -> Ident -> Array CaseAlternative -> Expr
lamCase2 s t alts = lam s (lam t (Case [mkVar s, mkVar t] alts))

-- ---------------------------------------------------------------------------
-- Lib references (qualified identifiers for built-in library functions)
-- ---------------------------------------------------------------------------

-- Data.Eq
iEq :: Expr
iEq = mkRef (Qualified (ByModuleName (ModuleName "Data.Eq")) (Ident "eq"))

iEq1 :: Expr
iEq1 = mkRef (Qualified (ByModuleName (ModuleName "Data.Eq")) (Ident "eq1"))

sEq :: PSString
sEq = mkString "eq"

sEq1 :: PSString
sEq1 = mkString "eq1"

-- Data.Ord
iCompare :: Expr
iCompare = mkRef (Qualified (ByModuleName (ModuleName "Data.Ord")) (Ident "compare"))

iCompare1 :: Expr
iCompare1 = mkRef (Qualified (ByModuleName (ModuleName "Data.Ord")) (Ident "compare1"))

sCompare :: PSString
sCompare = mkString "compare"

sCompare1 :: PSString
sCompare1 = mkString "compare1"

-- Data.HeytingAlgebra
iConj :: Expr
iConj = mkRef (Qualified (ByModuleName (ModuleName "Data.HeytingAlgebra")) (Ident "conj"))

-- Data.Functor
iMap :: Expr
iMap = mkRef (Qualified (ByModuleName (ModuleName "Data.Functor")) (Ident "map"))

sMap :: PSString
sMap = mkString "map"

-- Data.Bifunctor
iBimap :: Expr
iBimap = mkRef (Qualified (ByModuleName (ModuleName "Data.Bifunctor")) (Ident "bimap"))

sBimap :: PSString
sBimap = mkString "bimap"

iLmap :: Expr
iLmap = mkRef (Qualified (ByModuleName (ModuleName "Data.Bifunctor")) (Ident "lmap"))

iRmap :: Expr
iRmap = mkRef (Qualified (ByModuleName (ModuleName "Data.Bifunctor")) (Ident "rmap"))

-- Data.Functor.Contravariant
iCmap :: Expr
iCmap = mkRef (Qualified (ByModuleName (ModuleName "Data.Functor.Contravariant")) (Ident "cmap"))

sCmap :: PSString
sCmap = mkString "cmap"

-- Data.Profunctor
iDimap :: Expr
iDimap = mkRef (Qualified (ByModuleName (ModuleName "Data.Profunctor")) (Ident "dimap"))

sDimap :: PSString
sDimap = mkString "dimap"

iLcmap :: Expr
iLcmap = mkRef (Qualified (ByModuleName (ModuleName "Data.Profunctor")) (Ident "lcmap"))

iProfunctorRmap :: Expr
iProfunctorRmap = mkRef (Qualified (ByModuleName (ModuleName "Data.Profunctor")) (Ident "rmap"))

-- Data.Foldable
iFoldl :: Expr
iFoldl = mkRef (Qualified (ByModuleName (ModuleName "Data.Foldable")) (Ident "foldl"))

iFoldr :: Expr
iFoldr = mkRef (Qualified (ByModuleName (ModuleName "Data.Foldable")) (Ident "foldr"))

iFoldMap :: Expr
iFoldMap = mkRef (Qualified (ByModuleName (ModuleName "Data.Foldable")) (Ident "foldMap"))

sFoldl :: PSString
sFoldl = mkString "foldl"

sFoldr :: PSString
sFoldr = mkString "foldr"

sFoldMap :: PSString
sFoldMap = mkString "foldMap"

-- Data.Bifoldable
iBifoldl :: Expr
iBifoldl = mkRef (Qualified (ByModuleName (ModuleName "Data.Bifoldable")) (Ident "bifoldl"))

iBifoldr :: Expr
iBifoldr = mkRef (Qualified (ByModuleName (ModuleName "Data.Bifoldable")) (Ident "bifoldr"))

iBifoldMap :: Expr
iBifoldMap = mkRef (Qualified (ByModuleName (ModuleName "Data.Bifoldable")) (Ident "bifoldMap"))

sBifoldl :: PSString
sBifoldl = mkString "bifoldl"

sBifoldr :: PSString
sBifoldr = mkString "bifoldr"

sBifoldMap :: PSString
sBifoldMap = mkString "bifoldMap"

-- Data.Traversable
iTraverse :: Expr
iTraverse = mkRef (Qualified (ByModuleName (ModuleName "Data.Traversable")) (Ident "traverse"))

iSequence :: Expr
iSequence = mkRef (Qualified (ByModuleName (ModuleName "Data.Traversable")) (Ident "sequence"))

sTraverse :: PSString
sTraverse = mkString "traverse"

sSequence :: PSString
sSequence = mkString "sequence"

-- Data.Bitraversable
iBitraverse :: Expr
iBitraverse = mkRef (Qualified (ByModuleName (ModuleName "Data.Bitraversable")) (Ident "bitraverse"))

iBisequence :: Expr
iBisequence = mkRef (Qualified (ByModuleName (ModuleName "Data.Bitraversable")) (Ident "bisequence"))

sBitraverse :: PSString
sBitraverse = mkString "bitraverse"

sBisequence :: PSString
sBisequence = mkString "bisequence"

iLtraverse :: Expr
iLtraverse = mkRef (Qualified (ByModuleName (ModuleName "Data.Bitraversable")) (Ident "ltraverse"))

iRtraverse :: Expr
iRtraverse = mkRef (Qualified (ByModuleName (ModuleName "Data.Bitraversable")) (Ident "rtraverse"))

-- Data.Monoid
iMempty :: Expr
iMempty = mkRef (Qualified (ByModuleName (ModuleName "Data.Monoid")) (Ident "mempty"))

-- Data.Semigroup
iAppend :: Expr
iAppend = mkRef (Qualified (ByModuleName (ModuleName "Data.Semigroup")) (Ident "append"))

-- Control.Applicative
iPure :: Expr
iPure = mkRef (Qualified (ByModuleName (ModuleName "Control.Applicative")) (Ident "pure"))

-- Control.Apply
iApply :: Expr
iApply = mkRef (Qualified (ByModuleName (ModuleName "Control.Apply")) (Ident "apply"))

-- Control.Category
iIdentity :: Expr
iIdentity = mkRef (Qualified (ByModuleName (ModuleName "Control.Category")) (Ident "identity"))

-- Data.Function
iConst :: Expr
iConst = mkRef (Qualified (ByModuleName (ModuleName "Data.Function")) (Ident "const"))

iFlip :: Expr
iFlip = mkRef (Qualified (ByModuleName (ModuleName "Data.Function")) (Ident "flip"))

-- Class Qualified names
clsEq :: Qualified (ProperName ClassName)
clsEq = Qualified (ByModuleName (ModuleName "Data.Eq")) (ProperName "Eq")

clsEq1 :: Qualified (ProperName ClassName)
clsEq1 = Qualified (ByModuleName (ModuleName "Data.Eq")) (ProperName "Eq1")

clsOrd :: Qualified (ProperName ClassName)
clsOrd = Qualified (ByModuleName (ModuleName "Data.Ord")) (ProperName "Ord")

clsOrd1 :: Qualified (ProperName ClassName)
clsOrd1 = Qualified (ByModuleName (ModuleName "Data.Ord")) (ProperName "Ord1")

clsFunctor :: Qualified (ProperName ClassName)
clsFunctor = Qualified (ByModuleName (ModuleName "Data.Functor")) (ProperName "Functor")

clsBifunctor :: Qualified (ProperName ClassName)
clsBifunctor = Qualified (ByModuleName (ModuleName "Data.Bifunctor")) (ProperName "Bifunctor")

clsContravariant :: Qualified (ProperName ClassName)
clsContravariant = Qualified (ByModuleName (ModuleName "Data.Functor.Contravariant")) (ProperName "Contravariant")

clsProfunctor :: Qualified (ProperName ClassName)
clsProfunctor = Qualified (ByModuleName (ModuleName "Data.Profunctor")) (ProperName "Profunctor")

clsFoldable :: Qualified (ProperName ClassName)
clsFoldable = Qualified (ByModuleName (ModuleName "Data.Foldable")) (ProperName "Foldable")

clsBifoldable :: Qualified (ProperName ClassName)
clsBifoldable = Qualified (ByModuleName (ModuleName "Data.Bifoldable")) (ProperName "Bifoldable")

clsTraversable :: Qualified (ProperName ClassName)
clsTraversable = Qualified (ByModuleName (ModuleName "Data.Traversable")) (ProperName "Traversable")

clsBitraversable :: Qualified (ProperName ClassName)
clsBitraversable = Qualified (ByModuleName (ModuleName "Data.Bitraversable")) (ProperName "Bitraversable")

-- ---------------------------------------------------------------------------
-- Type info
-- ---------------------------------------------------------------------------

data TypeInfo = TypeInfo
  { tiTypeParams :: Array String
  , tiCtors      :: Array (Tuple (ProperName ConstructorName) (Array SourceType))
  , tiArgSubst   :: Array (Tuple String SourceType)
  }

lookupTypeInfo
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => UnwrappedTypeConstructor
  -> m TypeInfo
lookupTypeInfo utc@(UnwrappedTypeConstructor utcRec) = do
  Tuple _ (Tuple kindParams (Tuple tiTypeParamPairs tiCtors)) <- lookupTypeDecl utcRec.utcModuleName utcRec.utcTyCon
  let tiTypeParams = map fst tiTypeParamPairs
      tiArgSubst = Array.zip tiTypeParams utcRec.utcArgs <> Array.zip kindParams utcRec.utcKindArgs
  pure (TypeInfo { tiTypeParams, tiCtors, tiArgSubst })

-- ---------------------------------------------------------------------------
-- lookupTypeDecl
-- ---------------------------------------------------------------------------

lookupTypeDecl
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => ModuleName
  -> ProperName TypeName
  -> m (Tuple (Maybe DataDeclType) (Tuple (Array String) (Tuple (Array (Tuple String (Maybe SourceType))) (Array (Tuple (ProperName ConstructorName) (Array SourceType))))))
lookupTypeDecl mn typeName = do
  env <- getEnv
  let Environment envRec = env
  case Map.lookup (Qualified (ByModuleName mn) typeName) envRec.types of
    Nothing -> throwError (errorMessage (CannotFindDerivingType typeName))
    Just (Tuple kind (DataType _ args dctors)) -> do
      let mkargs = completeBinderList kind
          kargs = case mkargs of
            Nothing -> []
            Just (Tuple ka _) -> map (\(Tuple _ (Tuple v _)) -> v) ka
          dtype = do
            Tuple ctorName _ <- Array.head dctors
            Tuple (Tuple (Tuple a _ ) _ ) _ <- Map.lookup (Qualified (ByModuleName mn) ctorName) envRec.dataConstructors
            pure a
          typeArgs = map (\(Tuple (Tuple v k) _) -> Tuple v k) args
      pure (Tuple dtype (Tuple kargs (Tuple typeArgs dctors)))
    Just _ -> throwError (errorMessage (CannotFindDerivingType typeName))

-- ---------------------------------------------------------------------------
-- Helper predicates
-- ---------------------------------------------------------------------------

isAppliedVar :: SourceType -> Boolean
isAppliedVar (TypeApp _ (TypeVar _ _) _) = true
isAppliedVar _ = false

objectType :: SourceType -> Maybe SourceType
objectType (TypeApp _ (TypeConstructor _ recQual) rec)
  | recQual == Prim.tyRecord = Just rec
objectType _ = Nothing

decomposeRec :: SourceType -> Maybe (Array (Tuple Label SourceType))
decomposeRec ty = map (Array.sortBy (\(Tuple a _) (Tuple b _) -> compare a b)) (go ty)
  where
  go (RCons _ str typ typs) = map (Array.cons (Tuple str typ)) (go typs)
  go (REmpty _) = Just []
  go (KindApp _ (REmpty _) _) = Just []
  go _ = Nothing

decomposeRec' :: SourceType -> Array (Tuple Label SourceType)
decomposeRec' = Array.sortBy (\(Tuple a _) (Tuple b _) -> compare a b) <<< go
  where
  go (RCons _ str typ typs) = Array.cons (Tuple str typ) (go typs)
  go _ = []

-- ---------------------------------------------------------------------------
-- usedTypeVariables
-- ---------------------------------------------------------------------------

usedTypeVariables :: SourceType -> Array String
usedTypeVariables = Array.nub <<< everythingOnTypes (<>) go
  where
  go (TypeVar _ v) = [v]
  go _ = []

-- ---------------------------------------------------------------------------
-- Extract newtype name
-- ---------------------------------------------------------------------------

extractNewtypeName :: ModuleName -> Array SourceType -> Maybe (Tuple ModuleName (ProperName TypeName))
extractNewtypeName mn tys = do
  ty <- last tys
  utc <- unwrapTypeConstructor ty
  let Tuple m a = qualify mn (utcQTyCon utc)
  pure (Tuple m a)

-- ---------------------------------------------------------------------------
-- deriveInstance — entry point
-- ---------------------------------------------------------------------------

deriveInstance
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => MonadWriter MultipleErrors m
  => SourceType
  -> Qualified (ProperName ClassName)
  -> InstanceDerivationStrategy
  -> m Expr
deriveInstance instType className strategy = do
  mn <- unsafeCheckCurrentModule
  env <- getEnv
  let Environment envRec = env
  instUtc <- case unwrapTypeConstructor instType of
    Nothing -> internalCompilerError "invalid instance type"
    Just utc -> pure utc
  let UnwrappedTypeConstructor utcRec = instUtc
      tys = utcRec.utcArgs
      ctorName = coerceProperName <$> utcQTyCon instUtc

  TypeClassData tcd <-
    case Map.lookup className envRec.typeClasses of
      Nothing -> throwError (errorMessage (UnknownName (map TyClassName className)))
      Just tcd -> pure tcd

  case strategy of
    KnownClassStrategy ->
      let
        unaryClass :: (UnwrappedTypeConstructor -> m (Array (Tuple PSString Expr))) -> m Expr
        unaryClass f = case tys of
          [ty] -> case unwrapTypeConstructor ty of
            Just utc | mn == (let UnwrappedTypeConstructor u = utc in u.utcModuleName) -> do
              let superclassesDicts = map (\(Constraint c) ->
                    let tyArgs = map (replaceAllTypeVars (Array.zip (map fst tcd.typeClassArguments) tys)) c.constraintArgs
                    in lam UnusedIdent (DeferredDictionary c.constraintClass tyArgs)
                    ) tcd.typeClassSuperclasses
                  superclasses = Array.zip (map mkString (superClassDictionaryNames tcd.typeClassSuperclasses)) superclassesDicts
              fields <- f utc
              pure (App (Constructor nullSourceSpan ctorName)
                        (mkLit (ObjectLiteral (map (\ (Tuple k v) -> Tuple k v) (fields <> superclasses)))))
            _ -> throwError (errorMessage (ExpectedTypeConstructor className tys ty))
          _ -> throwError (errorMessage (InvalidDerivedInstance className tys 1))

        unaryClass' :: (Qualified (ProperName ClassName) -> UnwrappedTypeConstructor -> m (Array (Tuple PSString Expr))) -> m Expr
        unaryClass' f = unaryClass (f className)

      in
        if className == clsEq then unaryClass deriveEq
        else if className == clsEq1 then unaryClass (\_ -> deriveEq1)
        else if className == clsOrd then unaryClass deriveOrd
        else if className == clsOrd1 then unaryClass (\_ -> deriveOrd1)
        else if className == clsFunctor then unaryClass' (deriveFunctor Nothing false sFunMap)
        else if className == clsBifunctor then unaryClass' (deriveFunctor (Just false) false sBimap)
        else if className == clsContravariant then unaryClass' (deriveFunctor Nothing true sCmap)
        else if className == clsProfunctor then unaryClass' (deriveFunctor (Just true) false sDimap)
        else if className == clsFoldable then unaryClass' (deriveFoldable false)
        else if className == clsBifoldable then unaryClass' (deriveFoldable true)
        else if className == clsTraversable then unaryClass' (deriveTraversable false)
        else if className == clsBitraversable then unaryClass' (deriveTraversable true)
        else throwError (errorMessage (CannotDerive className tys))

    NewtypeStrategy ->
      case Array.last tys of
        Just lastTy -> case unwrapTypeConstructor lastTy of
          Just utc | mn == (let UnwrappedTypeConstructor u = utc in u.utcModuleName) ->
            deriveNewtypeInstance className tys utc
          _ -> throwError (errorMessage (ExpectedTypeConstructor className tys lastTy))
        Nothing -> throwError (errorMessage (InvalidNewtypeInstance className tys))

-- Map PSString constant (Functor uses "map" not "bimap")
sFunMap :: PSString
sFunMap = mkString "map"

-- ---------------------------------------------------------------------------
-- deriveNewtypeInstance
-- ---------------------------------------------------------------------------

deriveNewtypeInstance
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadWriter MultipleErrors m
  => Qualified (ProperName ClassName)
  -> Array SourceType
  -> UnwrappedTypeConstructor
  -> m Expr
deriveNewtypeInstance className tys (UnwrappedTypeConstructor utcRec) = do
  verifySuperclasses
  Tuple dtype (Tuple tyKindNames (Tuple tyArgNames ctors)) <- lookupTypeDecl utcRec.utcModuleName utcRec.utcTyCon
  go dtype tyKindNames tyArgNames ctors
  where
  mn = utcRec.utcModuleName
  dargs = utcRec.utcArgs
  dkargs = utcRec.utcKindArgs

  go (Just Newtype) tyKindNames tyArgNames ctors =
    case ctors of
      [Tuple _ [wrapped]] -> do
        wrapped' <- replaceAllTypeSynonyms wrapped
        case stripRight (takeReverse (Array.length tyArgNames - Array.length dargs) tyArgNames) wrapped' of
          Just wrapped'' -> do
            let subst = Array.zipWith (\(Tuple name _) t -> Tuple name t) tyArgNames dargs
                        <> Array.zip tyKindNames dkargs
            wrapped''' <- replaceAllTypeSynonyms (replaceAllTypeVars subst wrapped'')
            tys' <- traverse replaceAllTypeSynonyms tys
            let initTys = fromMaybe [] (init tys')
            pure (DeferredDictionary className (initTys <> [wrapped''']))
          Nothing -> throwError (errorMessage (InvalidNewtypeInstance className tys))
      _ -> throwError (errorMessage (InvalidNewtypeInstance className tys))
  go _ _ _ _ = throwError (errorMessage (InvalidNewtypeInstance className tys))

  takeReverse :: Int -> Array (Tuple String (Maybe SourceType)) -> Array (Tuple String (Maybe SourceType))
  takeReverse n arr = Array.take n (Array.reverse arr)

  stripRight :: Array (Tuple String (Maybe SourceType)) -> SourceType -> Maybe SourceType
  stripRight [] ty = Just ty
  stripRight args ty =
    case uncons args of
      Nothing -> Just ty
      Just { head: Tuple arg _, tail: rest } ->
        case ty of
          TypeApp _ t (TypeVar _ arg') | arg == arg' -> stripRight rest t
          _ -> Nothing

  verifySuperclasses :: m Unit
  verifySuperclasses = do
    env <- getEnv
    let Environment envRec = env
    for_ (Map.lookup className envRec.typeClasses) $ \(TypeClassData tcd) ->
      for_ tcd.typeClassSuperclasses $ \(Constraint c) -> do
        let constraintClass' = case c.constraintClass of
                                  Qualified (ByModuleName m) n -> Tuple m n
                                  Qualified (BySourcePos _) n  -> Tuple (internalError "verifySuperclasses: unknown class module") n
        for_ (Map.lookup c.constraintClass envRec.typeClasses) $ \(TypeClassData superTcd) ->
          when (not (Array.null tcd.typeClassArguments) &&
                maybe false (\arg -> any (_ == fst arg) (usedTypeVariables =<< c.constraintArgs))
                    (last tcd.typeClassArguments)) $ do
            let determined = map (\i -> srcTypeVar (fst (unsafeIndex tcd.typeClassArguments i)))
                              (Array.nub (Array.concatMap (\(FunctionalDependency fd) -> fd.fdDetermined)
                                (Array.filter (\(FunctionalDependency fd) -> fd.fdDeterminers == [Array.length tcd.typeClassArguments - 1])
                                  superTcd.typeClassDependencies)))
            let lastArg = last tcd.typeClassArguments
                lastConstraintArg = last c.constraintArgs
                initConstraintArgs = fromMaybe [] (init c.constraintArgs)
            case Tuple lastArg lastConstraintArg of
              Tuple (Just (Tuple lastArgName _)) (Just lastCA) ->
                if lastCA == srcTypeVar lastArgName &&
                   all (\ca -> any (_ == ca) determined) initConstraintArgs
                then do
                  for_ (extractNewtypeName mn tys) $ \ntName -> do
                    unless (hasNewtypeSuperclassInstance constraintClass' ntName envRec.typeClassDictionaries) $
                      tell (errorMessage (MissingNewtypeSuperclassInstance c.constraintClass className tys))
                else tell (errorMessage (UnverifiableSuperclassInstance c.constraintClass className tys))
              _ -> tell (errorMessage (UnverifiableSuperclassInstance c.constraintClass className tys))

  hasNewtypeSuperclassInstance
    :: Tuple ModuleName (ProperName ClassName)
    -> Tuple ModuleName (ProperName TypeName)
    -> Map QualifiedBy (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array (TypeClassDictionaryInScope (Qualified Ident)))))
    -> Boolean
  hasNewtypeSuperclassInstance (Tuple suModule suClass) nt@(Tuple newtypeModule _) dicts =
    let su = Qualified (ByModuleName suModule) suClass
        lookIn mn' =
          let mnDicts = Map.lookup (ByModuleName mn') dicts
              clsDicts = mnDicts >>= Map.lookup su
              allTcds = clsDicts >>= \m -> Just (Array.concatMap Array.fromFoldable (Array.fromFoldable (Map.values m)))
          in case allTcds of
               Nothing -> false
               Just tcds ->
                 any (\(TypeClassDictionaryInScope tcd) ->
                   case extractNewtypeName mn' tcd.tcdInstanceTypes of
                     Just nt' -> nt' == nt
                     Nothing -> false) tcds
    in lookIn suModule || lookIn newtypeModule

  internalError :: String -> ModuleName
  internalError _ = ModuleName "??"

  unsafeIndex :: forall a. Array a -> Int -> a
  unsafeIndex arr i = unsafePartial (Array.unsafeIndex arr i)

  internalErrorExpr :: String -> Tuple String (Maybe SourceType)
  internalErrorExpr msg = Tuple msg Nothing

  all :: forall a. (a -> Boolean) -> Array a -> Boolean
  all p = Array.all p

-- ---------------------------------------------------------------------------
-- deriveEq
-- ---------------------------------------------------------------------------

deriveEq
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => UnwrappedTypeConstructor
  -> m (Array (Tuple PSString Expr))
deriveEq utc = do
  TypeInfo ti <- lookupTypeInfo utc
  eqFun <- mkEqFunction ti.tiCtors
  pure [Tuple sEq eqFun]
  where
  mkEqFunction ctors = do
    x <- freshIdent "x"
    y <- freshIdent "y"
    alts <- traverse mkCtorClause ctors
    pure (lamCase2 x y (addCatch alts))

  preludeConj l r = App (App iConj l) r
  preludeEq l r = App (App iEq l) r
  preludeEq1 l r = App (App iEq1 l) r

  addCatch xs =
    let catchAll = CaseAlternative { caseAlternativeBinders: [NullBinder, NullBinder], caseAlternativeResult: unguarded (mkLit (BooleanLiteral false)) }
    in if Array.length xs /= 1 then xs <> [catchAll] else xs

  mkCtorClause (Tuple ctorName ctorTys) = do
    let UnwrappedTypeConstructor utcRec = utc
    identsL <- traverse (\_ -> freshIdent "l") ctorTys
    identsR <- traverse (\_ -> freshIdent "r") ctorTys
    tys' <- traverse replaceAllTypeSynonyms ctorTys
    let tests = Array.zipWith (\(Tuple l r) t -> toEqTest (mkVar l) (mkVar r) t) (Array.zip identsL identsR) tys'
        caseBinder idents = mkCtorBinder utcRec.utcModuleName ctorName (map mkBinder idents)
    pure (CaseAlternative
      { caseAlternativeBinders: [caseBinder identsL, caseBinder identsR]
      , caseAlternativeResult: unguarded (conjAll tests)
      })

  conjAll = case _ of
    [] -> mkLit (BooleanLiteral true)
    xs -> foldl1Array preludeConj xs

  toEqTest l r ty
    | Just fields <- decomposeRec =<< objectType ty =
        conjAll (map (\(Tuple (Label str) typ) -> toEqTest (Accessor str l) (Accessor str r) typ) fields)
    | isAppliedVar ty = preludeEq1 l r
    | otherwise = preludeEq l r

-- | Helper: zipWith3 for arrays
zipWith3' :: forall a b c d. (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
zipWith3' f as bs cs = Array.zipWith (\(Tuple a b) c -> f a b c) (Array.zip as bs) cs

-- | foldl1 for non-empty arrays (uses first element as start)
foldl1Array :: forall a. (a -> a -> a) -> Array a -> a
foldl1Array f arr = case uncons arr of
  Nothing -> unsafeCrashWith "foldl1Array: empty array"
  Just { head, tail } -> foldl f head tail

-- | foldr1 for non-empty arrays
foldr1Array :: forall a. (a -> a -> a) -> Array a -> a
foldr1Array f arr = case Array.unsnoc arr of
  Nothing -> unsafeCrashWith "foldr1Array: empty array"
  Just { init: ini, last: l } -> foldr f l ini

-- ---------------------------------------------------------------------------
-- deriveEq1
-- ---------------------------------------------------------------------------

deriveEq1 :: forall m. Applicative m => m (Array (Tuple PSString Expr))
deriveEq1 = pure [Tuple sEq1 iEq]

-- ---------------------------------------------------------------------------
-- deriveOrd
-- ---------------------------------------------------------------------------

deriveOrd
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => UnwrappedTypeConstructor
  -> m (Array (Tuple PSString Expr))
deriveOrd utc = do
  TypeInfo ti <- lookupTypeInfo utc
  compareFun <- mkCompareFunction ti.tiCtors
  pure [Tuple sCompare compareFun]
  where
  mkCompareFunction ctors = do
    x <- freshIdent "x"
    y <- freshIdent "y"
    clauseArrays <- traverse mkCtorClauses (splitLast ctors)
    pure (lamCase2 x y (addCatch (Array.concat clauseArrays)))

  splitLast :: forall a. Array a -> Array (Tuple a Boolean)
  splitLast arr = case Array.unsnoc arr of
    Nothing -> []
    Just { init: ini, last: l } -> map (\x -> Tuple x false) ini <> [Tuple l true]

  addCatch xs =
    let catchAll = CaseAlternative { caseAlternativeBinders: [NullBinder, NullBinder], caseAlternativeResult: unguarded (orderingCtor "EQ") }
    in if Array.null xs then [catchAll] else xs

  orderingMod = ModuleName "Data.Ordering"

  orderingCtor name = mkCtor orderingMod (ProperName name)

  orderingBinder name = mkCtorBinder orderingMod (ProperName name) []

  ordCompare l r = App (App iCompare l) r
  ordCompare1 l r = App (App iCompare1 l) r

  mkCtorClauses (Tuple (Tuple ctorName ctorTys) isLast) = do
    let UnwrappedTypeConstructor utcRec = utc
        localMn = utcRec.utcModuleName
    identsL <- traverse (\_ -> freshIdent "l") ctorTys
    identsR <- traverse (\_ -> freshIdent "r") ctorTys
    tys' <- traverse replaceAllTypeSynonyms ctorTys
    let tests = zipWith3' (\l r t -> toOrdering (mkVar l) (mkVar r) t) identsL identsR tys'
        caseBinder idents = mkCtorBinder localMn ctorName (map mkBinder idents)
        nullCaseBinder = mkCtorBinder localMn ctorName (Array.replicate (Array.length ctorTys) NullBinder)
        extras
          | not isLast =
              [ CaseAlternative { caseAlternativeBinders: [nullCaseBinder, NullBinder], caseAlternativeResult: unguarded (orderingCtor "LT") }
              , CaseAlternative { caseAlternativeBinders: [NullBinder, nullCaseBinder], caseAlternativeResult: unguarded (orderingCtor "GT") }
              ]
          | otherwise = []
    pure (Array.cons
      (CaseAlternative
        { caseAlternativeBinders: [caseBinder identsL, caseBinder identsR]
        , caseAlternativeResult: unguarded (appendAll tests)
        })
      extras)

  appendAll = case _ of
    [] -> orderingCtor "EQ"
    [x] -> x
    xs -> case Array.uncons xs of
      Nothing -> orderingCtor "EQ"
      Just { head: x, tail: rest } -> Case [x]
        [ CaseAlternative { caseAlternativeBinders: [orderingBinder "LT"], caseAlternativeResult: unguarded (orderingCtor "LT") }
        , CaseAlternative { caseAlternativeBinders: [orderingBinder "GT"], caseAlternativeResult: unguarded (orderingCtor "GT") }
        , CaseAlternative { caseAlternativeBinders: [NullBinder], caseAlternativeResult: unguarded (appendAll rest) }
        ]

  toOrdering l r ty
    | Just fields <- decomposeRec =<< objectType ty =
        appendAll (map (\(Tuple (Label str) typ) -> toOrdering (Accessor str l) (Accessor str r) typ) fields)
    | isAppliedVar ty = ordCompare1 l r
    | otherwise = ordCompare l r

-- ---------------------------------------------------------------------------
-- deriveOrd1
-- ---------------------------------------------------------------------------

deriveOrd1 :: forall m. Applicative m => m (Array (Tuple PSString Expr))
deriveOrd1 = pure [Tuple sCompare1 iCompare]

-- ---------------------------------------------------------------------------
-- ParamUsage — for Functor/Foldable/Traversable derivation
-- ---------------------------------------------------------------------------

data ParamUsage c
  = IsParam
  | IsLParam
  | MentionsParam (ParamUsage c)
  | MentionsParamBi (These (ParamUsage c) (ParamUsage c))
  | MentionsParamContravariantly c (ContravariantParamUsage c)
  | IsRecord (NonEmptyList (Tuple PSString (ParamUsage c)))

data ContravariantParamUsage c
  = MentionsParamContra (ParamUsage c)
  | MentionsParamPro (These (ParamUsage c) (ParamUsage c))

data CovariantClasses = CovariantClasses
  { monoClass :: Qualified (ProperName ClassName)
  , biClass   :: Qualified (ProperName ClassName)
  }

data ContravariantClasses = ContravariantClasses
  { contraClass :: Qualified (ProperName ClassName)
  , proClass    :: Qualified (ProperName ClassName)
  }

data ContravarianceSupport c = ContravarianceSupport
  { contravarianceWitness   :: c
  , paramIsContravariant    :: Boolean
  , lparamIsContravariant   :: Boolean
  , contravariantClasses    :: ContravariantClasses
  }

-- ---------------------------------------------------------------------------
-- validateParamsInTypeConstructors
-- ---------------------------------------------------------------------------

validateParamsInTypeConstructors
  :: forall c m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Qualified (ProperName ClassName)
  -> UnwrappedTypeConstructor
  -> Boolean
  -> CovariantClasses
  -> Maybe (ContravarianceSupport c)
  -> m (Array (Tuple (ProperName ConstructorName) (Array (Maybe (ParamUsage c)))))
validateParamsInTypeConstructors derivingClass utc isBi (CovariantClasses cv) contravarianceSupport = do
  TypeInfo ti <- lookupTypeInfo utc
  -- determine the type parameters we're tracking
  result <- liftEither $
    let revParams = Array.reverse ti.tiTypeParams
    in if not isBi
      then case Array.head revParams of
        Just x  -> Right (Tuple Nothing x)
        Nothing -> Left (errorMessage (KindsDoNotUnify kindType (kindType `fnArrow` kindType)))
      else case Tuple (Array.index revParams 0) (Array.index revParams 1) of
        Tuple (Just y) (Just x) -> Right (Tuple (Just x) y)
        Tuple (Just _) Nothing  -> Left (errorMessage (KindsDoNotUnify kindType (kindType `fnArrow` kindType)))
        _                       -> Left (errorMessage (KindsDoNotUnify kindType (kindType `fnArrow` kindType `fnArrow` kindType)))
  let Tuple mbLParam param = result
  ctors' <- traverse (traverseTuple (traverse replaceAllTypeSynonyms)) ti.tiCtors
  tcds' <- getTypeClassDictionaries
  let tcds = (map (map (Map.mapMaybe NEL.fromFoldable)) tcds' :: InstanceContext)
      paramIsContra = any (\(ContravarianceSupport cs) -> cs.paramIsContravariant) contravarianceSupport
      lparamIsContra = any (\(ContravarianceSupport cs) -> cs.lparamIsContravariant) contravarianceSupport
      params = case mbLParam of
        Nothing -> That param
        Just lp -> Both lp param
  Tuple ctorUsages problemSpans <- runWriterT do
    traverse (traverseTuple (traverse (typeToUsageOf tcds ti.tiArgSubst params false))) ctors'
  let relatedClasses = [cv.monoClass, cv.biClass] <>
        Array.concatMap (\(ContravarianceSupport cs) ->
          let ContravariantClasses cc = cs.contravariantClasses
          in [cc.contraClass, cc.proClass]) (Array.fromFoldable contravarianceSupport)
  case NEL.fromFoldable (Array.nub problemSpans) of
    Nothing -> pure unit
    Just sss -> throwError (addHint (RelatedPositions sss) (errorMessage (CannotDeriveInvalidConstructorArg derivingClass relatedClasses (isJust contravarianceSupport))))
  pure ctorUsages
  where
  kindType :: SourceType
  kindType = TypeConstructor nullSourceAnn (Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Type"))

  fnArrow :: SourceType -> SourceType -> SourceType
  fnArrow a b = TypeApp nullSourceAnn (TypeApp nullSourceAnn (TypeConstructor nullSourceAnn (Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Function"))) a) b

  traverseTuple :: forall a b c f. Functor f => (b -> f c) -> Tuple a b -> f (Tuple a c)
  traverseTuple f (Tuple a b) = map (Tuple a) (f b)

  typeToUsageOf
    :: InstanceContext
    -> Array (Tuple String SourceType)
    -> These String String
    -> Boolean
    -> SourceType
    -> WriterT (Array SourceSpan) m (Maybe (ParamUsage c))
  typeToUsageOf tcds subst params0 isNegative = go params0 isNegative
    where
    go params isNeg ty =
      let goCo = go params isNeg
          goContra = go params (not isNeg)

          assertNoParamUsedIn :: SourceType -> WriterT (Array SourceSpan) m Unit
          assertNoParamUsedIn ty' = do
            void (traverseBothThese (\p -> assertParamNotUsedIn p ty') params)

          assertParamNotUsedIn :: String -> SourceType -> WriterT (Array SourceSpan) m Unit
          assertParamNotUsedIn paramName = everythingOnTypes (*>) $ \ty' ->
            case ty' of
              TypeVar (Tuple ss _) name | name == paramName -> tell [ss]
              _ -> pure unit

          headOfTypeWithSubst :: SourceType -> Qualified (Either String (ProperName TypeName))
          headOfTypeWithSubst = headOfType <<< replaceAllTypeVars subst

          tryBiClasses ht tyLArg tyArg =
            if hasInstance tcds ht cv.biClass
            then goCo tyLArg >>= preferMonoClass MentionsParamBi
            else case contravarianceSupport of
              Just (ContravarianceSupport cs) ->
                let ContravariantClasses cc = cs.contravariantClasses
                in if hasInstance tcds ht cc.proClass
                   then goContra tyLArg >>= preferMonoClass (\us -> MentionsParamContravariantly cs.contravarianceWitness (MentionsParamPro us))
                   else assertNoParamUsedIn tyLArg *> tryMonoClasses ht tyArg
              Nothing ->
                assertNoParamUsedIn tyLArg *> tryMonoClasses ht tyArg
            where
            preferMonoClass f lUsage = do
              rUsage <- goCo tyArg
              case Tuple lUsage rUsage of
                Tuple Nothing _ | hasInstance tcds ht cv.monoClass ->
                  pure (map MentionsParam rUsage)
                _ ->
                  pure (alignMaybes lUsage rUsage >>= \t -> Just (f t))

          tryMonoClasses ht tyArg =
            if hasInstance tcds ht cv.monoClass
            then map (map MentionsParam) (goCo tyArg)
            else case contravarianceSupport of
              Just (ContravarianceSupport cs) ->
                let ContravariantClasses cc = cs.contravariantClasses
                in if hasInstance tcds ht cc.contraClass
                   then map (map (\u -> MentionsParamContravariantly cs.contravarianceWitness (MentionsParamContra u))) (goContra tyArg)
                   else assertNoParamUsedIn tyArg $> Nothing
              Nothing ->
                assertNoParamUsedIn tyArg $> Nothing

      in case ty of
          ForAll _ _ name _ innerTy _ ->
            map join (traverse (\p -> go p isNeg innerTy) (filterThese (_ /= name) params))

          ConstrainedType _ _ innerTy ->
            goCo innerTy

          TypeApp _ (TypeConstructor _ recQual) row
            | recQual == Prim.tyRecord ->
                let fields = decomposeRec' row
                in map (map IsRecord <<< NEL.fromFoldable <<< Array.catMaybes) $
                   traverse (\(Tuple (Label lbl) fieldTy) ->
                     map (map (Tuple lbl)) (goCo fieldTy)) fields

          TypeApp _ (TypeApp _ tyFn tyLArg) tyArg ->
            assertNoParamUsedIn tyFn *> tryBiClasses (headOfTypeWithSubst tyFn) tyLArg tyArg

          TypeApp _ tyFn tyArg ->
            assertNoParamUsedIn tyFn *> tryMonoClasses (headOfTypeWithSubst tyFn) tyArg

          TypeVar (Tuple ss _) name ->
            let checkL = case params of
                  This lp -> if name == lp then (when (lparamIsContra /= isNeg) (tell [ss])) $> Just IsLParam else pure Nothing
                  Both lp _ -> if name == lp then (when (lparamIsContra /= isNeg) (tell [ss])) $> Just IsLParam else pure Nothing
                  _ -> pure Nothing
                checkR = case params of
                  That p -> if name == p then (when (paramIsContravariant' /= isNeg) (tell [ss])) $> Just IsParam else pure Nothing
                  Both _ p -> if name == p then (when (paramIsContravariant' /= isNeg) (tell [ss])) $> Just IsParam else pure Nothing
                  _ -> pure Nothing
            in do
              l <- checkL
              r <- checkR
              pure (l <|> r)

          _ ->
            assertNoParamUsedIn ty $> Nothing

  paramIsContravariant' = case contravarianceSupport of
    Just (ContravarianceSupport cs) -> cs.paramIsContravariant
    Nothing -> false

  lparamIsContra = case contravarianceSupport of
    Just (ContravarianceSupport cs) -> cs.lparamIsContravariant
    Nothing -> false

  hasInstance :: InstanceContext -> Qualified (Either String (ProperName TypeName)) -> Qualified (ProperName ClassName) -> Boolean
  hasInstance tcds ht@(Qualified qb _) cn@(Qualified cqb _) =
    any (any tcdAppliesToType <<< findDicts tcds cn)
        (Array.nub [byNullSourcePos, cqb, qb])
    where
    tcdAppliesToType (TypeClassDictionaryInScope tcd) = case tcd.tcdInstanceTypes of
      [singleTy] -> headOfType singleTy == ht
      _ -> false

  headOfType :: SourceType -> Qualified (Either String (ProperName TypeName))
  headOfType = go'
    where
    go' (TypeApp _ t _) = go' t
    go' (KindApp _ t _) = go' t
    go' (TypeVar _ nm) = Qualified byNullSourcePos (Left nm)
    go' (Skolem _ nm _ _ _) = Qualified byNullSourcePos (Left nm)
    go' (TypeConstructor _ (Qualified qb nm)) = Qualified qb (Right nm)
    go' _ = Qualified byNullSourcePos (Left "??")

-- ---------------------------------------------------------------------------
-- TraversalExprs and ContraversalExprs
-- ---------------------------------------------------------------------------

data TraversalExprs = TraversalExprs
  { recurseVar    :: Expr
  , birecurseVar  :: Expr
  , lrecurseExpr  :: Expr
  , rrecurseExpr  :: Expr
  }

data ContraversalExprs = ContraversalExprs
  { crecurseVar   :: Expr
  , direcurseVar  :: Expr
  , lcrecurseVar  :: Expr
  , rprorecurseVar :: Expr
  }

appBirecurseExprs :: TraversalExprs -> These Expr Expr -> Expr
appBirecurseExprs (TraversalExprs te) = these
  (\l -> App te.lrecurseExpr l)
  (\r -> App te.rrecurseExpr r)
  (\l r -> App (App te.birecurseVar l) r)

appDirecurseExprs :: ContraversalExprs -> These Expr Expr -> Expr
appDirecurseExprs (ContraversalExprs ce) = these
  (\l -> App ce.lcrecurseVar l)
  (\r -> App ce.rprorecurseVar r)
  (\l r -> App (App ce.direcurseVar l) r)

-- ---------------------------------------------------------------------------
-- mkCasesForTraversal
-- ---------------------------------------------------------------------------

mkCasesForTraversal
  :: forall c f m
   . Applicative f
  => MonadSupply m
  => ModuleName
  -> (ParamUsage c -> Expr -> f Expr)
  -> (f Expr -> m Expr)
  -> Array (Tuple (ProperName ConstructorName) (Array (Maybe (ParamUsage c))))
  -> m (Array CaseAlternative)
mkCasesForTraversal mn handleArg extractExpr ctors =
  for ctors $ \(Tuple ctorName ctorUsages) -> do
    ctorArgs <- traverse (\usage -> freshIdent "v" >>= \i -> pure (Tuple i usage)) ctorUsages
    let ctorExpr = mkCtor mn ctorName
        caseBinder = mkCtorBinder mn ctorName (map (mkBinder <<< fst) ctorArgs)
    resultExpr <- extractExpr $
      map (foldl App ctorExpr) $
      for ctorArgs $ \(Tuple ident mbUsage) ->
        maybe pure handleArg mbUsage (mkVar ident)
    pure (CaseAlternative { caseAlternativeBinders: [caseBinder], caseAlternativeResult: unguarded resultExpr })

-- ---------------------------------------------------------------------------
-- usingLamIdent and traverseFields
-- ---------------------------------------------------------------------------

usingLamIdent :: forall m. MonadSupply m => (Expr -> m Expr) -> m Expr
usingLamIdent cb = do
  ident <- freshIdent "v"
  map (lam ident) (cb (mkVar ident))

traverseFields
  :: forall c f
   . Applicative f
  => (ParamUsage c -> Expr -> f Expr)
  -> NonEmptyList (Tuple PSString (ParamUsage c))
  -> Expr
  -> f Expr
traverseFields f fields r =
  map (ObjectUpdate r) $
  for (NEL.toUnfoldable fields :: Array _) $ \(Tuple lbl usage) ->
    map (Tuple lbl) (f usage (Accessor lbl r))

unnestRecords
  :: forall c f
   . Applicative f
  => (ParamUsage c -> Expr -> f Expr)
  -> ParamUsage c
  -> Expr
  -> f Expr
unnestRecords f = go
  where
  go (IsRecord fields) = traverseFields go fields
  go usage = f usage

-- The fully polymorphic traversal ops are specialized per use.

-- ---------------------------------------------------------------------------
-- deriveFunctor (map, bimap, cmap, dimap)
-- ---------------------------------------------------------------------------

deriveFunctor
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => Maybe Boolean
  -> Boolean
  -> PSString
  -> Qualified (ProperName ClassName)
  -> UnwrappedTypeConstructor
  -> m (Array (Tuple PSString Expr))
deriveFunctor mbLParamIsContravariant paramIsContravariant mapName nm utc = do
  let isBi = isJust mbLParamIsContravariant
      functorClasses = CovariantClasses { monoClass: clsFunctor, biClass: clsBifunctor }
      contravariantClasses = ContravariantClasses { contraClass: clsContravariant, proClass: clsProfunctor }
  ctors <- validateParamsInTypeConstructors nm utc isBi functorClasses $ Just $ ContravarianceSupport
    { contravarianceWitness: unit
    , paramIsContravariant
    , lparamIsContravariant: fromMaybe false mbLParamIsContravariant
    , contravariantClasses
    }
  let mapExprs = TraversalExprs
        { recurseVar: iMap
        , birecurseVar: iBimap
        , lrecurseExpr: iLmap
        , rrecurseExpr: iRmap
        }
      cmapExprs = ContraversalExprs
        { crecurseVar: iCmap
        , direcurseVar: iDimap
        , lcrecurseVar: iLcmap
        , rprorecurseVar: iProfunctorRmap
        }
  mapFun <- mkFunctorTraversal (let UnwrappedTypeConstructor u = utc in u.utcModuleName) isBi mapExprs (const cmapExprs) ctors
  pure [Tuple mapName mapFun]

mkFunctorTraversal
  :: forall m
   . MonadSupply m
  => ModuleName
  -> Boolean
  -> TraversalExprs
  -> (Unit -> ContraversalExprs)
  -> Array (Tuple (ProperName ConstructorName) (Array (Maybe (ParamUsage Unit))))
  -> m Expr
mkFunctorTraversal mn isBi te@(TraversalExprs teRec) getContraversalExprs ctors = do
  f <- freshIdent "f"
  g <- if isBi then freshIdent "g" else pure f
  alts <- mkCasesForTraversal mn (handleValue f g) identity ctors
  pure (lam f (applyWhen isBi (lam g) (lamCase (Ident "$__traversal") alts)))
  where
  handleValue f g = unnestRecords $ \usage inputExpr ->
    flip App inputExpr <$> mkFnExprForValue f g usage

  mkFnExprForValue :: Ident -> Ident -> ParamUsage Unit -> m Expr
  mkFnExprForValue f g = case _ of
    IsParam ->
      pure (mkVar g)
    IsLParam ->
      pure (mkVar f)
    MentionsParam innerUsage ->
      map (App teRec.recurseVar) (mkFnExprForValue f g innerUsage)
    MentionsParamBi theseInnerUsages ->
      map (appBirecurseExprs te) (traverseBothThese (mkFnExprForValue f g) theseInnerUsages)
    MentionsParamContravariantly c contraUsage ->
      let ce = getContraversalExprs c
          ContraversalExprs ceRec = ce
      in case contraUsage of
           MentionsParamContra innerUsage ->
             map (App ceRec.crecurseVar) (mkFnExprForValue f g innerUsage)
           MentionsParamPro theseInnerUsages ->
             map (appDirecurseExprs ce) (traverseBothThese (mkFnExprForValue f g) theseInnerUsages)
    IsRecord fields ->
      usingLamIdent $ \r ->
        map (\body -> body) (traverseFields (unnestRecords (\u i -> flip App i <$> mkFnExprForValue f g u)) fields r)

-- ---------------------------------------------------------------------------
-- deriveFoldable
-- ---------------------------------------------------------------------------

deriveFoldable
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => Boolean
  -> Qualified (ProperName ClassName)
  -> UnwrappedTypeConstructor
  -> m (Array (Tuple PSString Expr))
deriveFoldable isBi nm utc = do
  let localMn = let UnwrappedTypeConstructor u = utc in u.utcModuleName
      foldableClasses = CovariantClasses { monoClass: clsFoldable, biClass: clsBifoldable }
  ctors <- validateParamsInTypeConstructors nm utc isBi foldableClasses Nothing
  foldlFun <- mkAsymmetricFoldFunction false foldlExprs ctors
  foldrFun <- mkAsymmetricFoldFunction true foldrExprs ctors
  foldMapFun <- mkFoldMapTraversal localMn isBi foldMapExprs ctors
  pure
    [ Tuple (if isBi then sBifoldl else sFoldl) foldlFun
    , Tuple (if isBi then sBifoldr else sFoldr) foldrFun
    , Tuple (if isBi then sBifoldMap else sFoldMap) foldMapFun
    ]
  where
  localMn = let UnwrappedTypeConstructor u = utc in u.utcModuleName

  foldlExprs = TraversalExprs
    { recurseVar: iFoldl
    , birecurseVar: iBifoldl
    , lrecurseExpr: App (App iFlip iBifoldl) iConst
    , rrecurseExpr: App iBifoldl iConst
    }
  foldrExprs = TraversalExprs
    { recurseVar: iFoldr
    , birecurseVar: iBifoldr
    , lrecurseExpr: App (App iFlip iBifoldr) (App iConst iIdentity)
    , rrecurseExpr: App iBifoldr (App iConst iIdentity)
    }
  foldMapExprs = TraversalExprs
    { recurseVar: iFoldMap
    , birecurseVar: iBifoldMap
    , lrecurseExpr: App (App iFlip iBifoldMap) iMempty
    , rrecurseExpr: App iBifoldMap iMempty
    }

  mkAsymmetricFoldFunction
    :: Boolean
    -> TraversalExprs
    -> Array (Tuple (ProperName ConstructorName) (Array (Maybe (ParamUsage Void))))
    -> m Expr
  mkAsymmetricFoldFunction isRightFold te@(TraversalExprs teRec) ctors' = do
    f <- freshIdent "f"
    g <- if isBi then freshIdent "g" else pure f
    z <- freshIdent "z"
    let
      appCombiner :: Tuple Boolean Expr -> Expr -> Expr -> Expr
      appCombiner (Tuple isFlipped fn) =
        applyWhen (isFlipped == isRightFold) flip' $ \acc x -> App (App fn acc) x

      mkCombinerExpr :: ParamUsage Void -> m Expr
      mkCombinerExpr usage = do
        Tuple isFlipped fn <- getCombiner usage
        pure (if isFlipped then App iFlip fn else fn)

      handleValue :: ParamUsage Void -> Expr -> Const (m (Expr -> Expr)) Expr
      handleValue = unnestRecords $ \usage inputExpr ->
        toConst (map (\cb -> \acc -> cb acc inputExpr) (getCombiner usage >>= \(Tuple isFlipped fn) -> pure (appCombiner (Tuple isFlipped fn))))

      getCombiner :: ParamUsage Void -> m (Tuple Boolean Expr)
      getCombiner = case _ of
        IsParam ->
          pure (Tuple false (mkVar g))
        IsLParam ->
          pure (Tuple false (mkVar f))
        MentionsParam innerUsage ->
          map (\e -> Tuple isRightFold (App teRec.recurseVar e)) (mkCombinerExpr innerUsage)
        MentionsParamBi theseInnerUsages ->
          map (\e -> Tuple isRightFold e) (map (appBirecurseExprs te) (traverseBothThese mkCombinerExpr theseInnerUsages))
        IsRecord fields ->
          map (\e -> Tuple false e) $
          usingLamIdent $ \lVar ->
          usingLamIdent $ \rVar -> do
            -- fold over fields
            let foldFieldsOf r0 = traverseFields handleValue fields r0
            if isRightFold
              then extractExprStartingWith lVar (foldFieldsOf rVar)
              else extractExprStartingWith rVar (foldFieldsOf lVar)
        MentionsParamContravariantly c _ -> absurd c

      extractExprStartingWith :: Expr -> Const (m (Expr -> Expr)) Expr -> m Expr
      extractExprStartingWith z' (Const fns) = do
        combiners <- sequence fns
        if isRightFold
          then pure (foldr (\combiner acc -> combiner acc) z' combiners)
          else pure (foldl (\acc combiner -> combiner acc) z' combiners)

    alts <- mkCasesForTraversal localMn handleValue (extractExprStartingWith (mkVar z)) ctors'
    pure (lam f (applyWhen isBi (lam g) (lam z (lamCase (Ident "$__fold") alts))))

  flip' :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
  flip' f x y = f y x

-- ---------------------------------------------------------------------------
-- mkFoldMapTraversal (using Const-based applicative)
-- ---------------------------------------------------------------------------

mkFoldMapTraversal
  :: forall m
   . MonadSupply m
  => ModuleName
  -> Boolean
  -> TraversalExprs
  -> Array (Tuple (ProperName ConstructorName) (Array (Maybe (ParamUsage Void))))
  -> m Expr
mkFoldMapTraversal mn isBi te@(TraversalExprs teRec) ctors = do
  f <- freshIdent "f"
  g <- if isBi then freshIdent "g" else pure f
  alts <- mkCasesForTraversal mn (handleValue f g) extractExpr ctors
  pure (lam f (applyWhen isBi (lam g) (lamCase (Ident "$__foldMap") alts)))
  where
  handleValue :: Ident -> Ident -> ParamUsage Void -> Expr -> Const (m Expr) Expr
  handleValue f g = unnestRecords $ \usage inputExpr ->
    toConst (map (\fn -> App fn inputExpr) (mkFnExprForValue f g usage))

  mkFnExprForValue :: Ident -> Ident -> ParamUsage Void -> m Expr
  mkFnExprForValue f g = case _ of
    IsParam -> pure (mkVar g)
    IsLParam -> pure (mkVar f)
    MentionsParam innerUsage ->
      map (App teRec.recurseVar) (mkFnExprForValue f g innerUsage)
    MentionsParamBi theseInnerUsages ->
      map (appBirecurseExprs te) (traverseBothThese (mkFnExprForValue f g) theseInnerUsages)
    IsRecord fields ->
      usingLamIdent $ \r -> extractExpr (traverseFields (unnestRecords (\u i -> toConst (map (\fn -> App fn i) (mkFnExprForValue f g u)))) fields r)
    MentionsParamContravariantly c _ -> absurd c

  extractExpr :: Const (m Expr) Expr -> m Expr
  extractExpr (Const exprs) = do
    es <- sequence exprs
    case es of
      [] -> pure iMempty
      _  -> pure (foldr1Array (\x y -> App (App iAppend x) y) es)

-- ---------------------------------------------------------------------------
-- deriveTraversable
-- ---------------------------------------------------------------------------

deriveTraversable
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => Boolean
  -> Qualified (ProperName ClassName)
  -> UnwrappedTypeConstructor
  -> m (Array (Tuple PSString Expr))
deriveTraversable isBi nm utc = do
  let localMn = let UnwrappedTypeConstructor u = utc in u.utcModuleName
      traversableClasses = CovariantClasses { monoClass: clsTraversable, biClass: clsBitraversable }
  ctors <- validateParamsInTypeConstructors nm utc isBi traversableClasses Nothing
  traverseFun <- mkTraversableTraversal localMn isBi traverseExprs ctors
  let sequenceExprBase = if isBi
        then App (App iBitraverse iIdentity) iIdentity
        else App iTraverse iIdentity
  sequenceFun <- usingLamIdent $ \_ -> pure (App sequenceExprBase (mkVar (Ident "$__v")))
  sequenceFun' <- usingLamIdent $ \v -> pure (App sequenceExprBase v)
  pure
    [ Tuple (if isBi then sBitraverse else sTraverse) traverseFun
    , Tuple (if isBi then sBisequence else sSequence) sequenceFun'
    ]
  where
  traverseExprs = TraversalExprs
    { recurseVar: iTraverse
    , birecurseVar: iBitraverse
    , lrecurseExpr: iLtraverse
    , rrecurseExpr: iRtraverse
    }

-- | For traversals, we use a WriterT-based applicative to collect intermediate bindings.
mkTraversableTraversal
  :: forall m
   . MonadSupply m
  => ModuleName
  -> Boolean
  -> TraversalExprs
  -> Array (Tuple (ProperName ConstructorName) (Array (Maybe (ParamUsage Void))))
  -> m Expr
mkTraversableTraversal mn isBi te@(TraversalExprs teRec) ctors = do
  f <- freshIdent "f"
  g <- if isBi then freshIdent "g" else pure f
  alts <- mkCasesForTraversal mn (handleValue f g) extractExpr ctors
  pure (lam f (applyWhen isBi (lam g) (lamCase (Ident "$__traverse") alts)))
  where
  handleValue :: Ident -> Ident -> ParamUsage Void -> Expr -> WriterT (Array (Tuple Ident (m Expr))) m Expr
  handleValue f g = unnestRecords $ \usage inputExpr -> do
    fn <- lift (mkFnExprForValue f g usage)
    ident <- lift (freshIdent "v")
    tell [Tuple ident (pure (App fn inputExpr))]
    pure (mkVar ident)

  mkFnExprForValue :: Ident -> Ident -> ParamUsage Void -> m Expr
  mkFnExprForValue f g = case _ of
    IsParam -> pure (mkVar g)
    IsLParam -> pure (mkVar f)
    MentionsParam innerUsage ->
      map (App teRec.recurseVar) (mkFnExprForValue f g innerUsage)
    MentionsParamBi theseInnerUsages ->
      map (appBirecurseExprs te) (traverseBothThese (mkFnExprForValue f g) theseInnerUsages)
    IsRecord fields ->
      usingLamIdent $ \r -> extractExpr (traverseFields (unnestRecords (visitField f g)) fields r)
    MentionsParamContravariantly c _ -> absurd c

  visitField :: Ident -> Ident -> ParamUsage Void -> Expr -> WriterT (Array (Tuple Ident (m Expr))) m Expr
  visitField f g u i = do
    fn <- lift (mkFnExprForValue f g u)
    ident <- lift (freshIdent "v")
    tell [Tuple ident (pure (App fn i))]
    pure (mkVar ident)

  extractExpr :: WriterT (Array (Tuple Ident (m Expr))) m Expr -> m Expr
  extractExpr wt = do
    Tuple result bindings <- runWriterT wt
    let Tuple ctx argMs = Array.unzip bindings
    resolved <- sequence argMs
    pure (mkApps resolved (foldr lam result ctx))

  mkApps :: Array Expr -> Expr -> Expr
  mkApps arr body = case Array.uncons arr of
    Nothing -> App iPure body
    Just { head: h, tail: t } ->
      foldl (\acc x -> App (App iApply acc) x) (App (App iMap body) h) t

-- ---------------------------------------------------------------------------
-- applyWhen
-- ---------------------------------------------------------------------------

applyWhen :: forall a. Boolean -> (a -> a) -> a -> a
applyWhen true f x = f x
applyWhen false _ x = x

