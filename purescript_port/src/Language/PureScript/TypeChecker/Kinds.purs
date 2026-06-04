-- | Kind checker for PureScript types.
-- | Port of Language.PureScript.TypeChecker.Kinds from Haskell.
module Language.PureScript.TypeChecker.Kinds
  ( kindOf
  , kindOfWithUnknowns
  , kindOfWithScopedVars
  , kindOfData
  , kindOfTypeSynonym
  , kindOfClass
  , kindsOfAll
  , unifyKinds
  , unifyKinds'
  , subsumesKind
  , instantiateKind
  , checkKind
  , inferKind
  , elaborateKind
  , checkConstraint
  , checkInstanceDeclaration
  , checkKindDeclaration
  , checkTypeKind
  , unknownsWithKinds
  , freshKind
  , freshKindWithKind
  , applySubst
  , kindType
  , kindSymbol
  , kindRow
  , kindOfEnvironment
  , unapplyTypes
  ) where

import Prelude

import Control.Monad (when, unless)
import Control.Monad.Error.Class (class MonadError, throwError, catchError)
import Control.Monad.State.Class (class MonadState, gets, modify_)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Supply.Class (class MonadSupply, fresh)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromJust, maybe)
import Data.Set as Set
import Data.Traversable (traverse, for)
import Data.Foldable (traverse_, for_)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

import Language.PureScript.AST.Declarations
  ( DataConstructorDeclaration(..)
  , Declaration(..)
  , TypeDeclarationData(..)
  , ErrorMessageHint(..)
  , mapDataCtorFields
  , traverseDataCtorFields
  )
import Language.PureScript.AST.Traversals (accumTypes)
import Language.PureScript.AST.SourcePos (SourceAnn, SourceSpan, nullSourceAnn, nullSourceSpan)
import Language.PureScript.Environment
  ( DataDeclType(..)
  , Environment(..)
  , NameKind(..)
  , NameVisibility(..)
  , TypeClassData(..)
  , TypeKind(..)
  , makeTypeClassData
  , tyFunction
  , function
  )
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , errorMessage
  , errorMessage'
  , internalCompilerError
  , rethrowWithPosition
  )
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , Ident
  , ModuleName
  , Name(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , byNullSourcePos
  , coerceProperName
  , mkQualified
  , runProperName
  )
import Language.PureScript.TypeChecker.Monad
  ( CheckState(..)
  , Substitution(..)
  , UnkLevel(..)
  , Unknown
  , bindTypes
  , getEnv
  , lookupTypeVariable
  , modifyEnv
  , withErrorMessageHint
  , withFreshSubstitution
  , unsafeCheckCurrentModule
  )
import Language.PureScript.TypeChecker.Skolems (newSkolemConstant, newSkolemScope, skolemize)
import Language.PureScript.TypeChecker.Synonyms (SynonymMap, replaceAllTypeSynonyms)
import Language.PureScript.Types
  ( Constraint(..)
  , RowListItem(..)
  , SkolemScope(..)
  , SourceConstraint
  , SourceType
  , Type(..)
  , TypeVarVisibility(..)
  , alignRowsWith
  , addVisibility
  , completeBinderList
  , everywhereOnTypes
  , everywhereOnTypesM
  , freeTypeVariables
  , getAnnForType
  , isREmptyKinded
  , mapConstraintArgsAll
  , mkForAll
  , replaceAllTypeVars
  , replaceTypeVars
  , rowFromList
  , rowToList
  , setAnnForType
  , srcKindApp
  , srcKindedType
  , srcREmpty
  , srcTypeApp
  , srcTypeConstructor
  , srcTypeVar
  )
import Language.PureScript.Names (ModuleName(..))
import Language.PureScript.Constants.Prim as C
import Data.List.NonEmpty as NEL

-- ---------------------------------------------------------------------------
-- Well-known kinds
-- ---------------------------------------------------------------------------

kindType :: SourceType
kindType = srcTypeConstructor (Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Type"))

kindConstraint :: SourceType
kindConstraint = srcTypeConstructor (Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Constraint"))

kindSymbol :: SourceType
kindSymbol = srcTypeConstructor (Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Symbol"))

kindOfEnvironment :: SourceType
kindOfEnvironment = kindType

kindRow :: SourceType -> SourceType
kindRow = srcTypeApp (srcTypeConstructor (Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Row")))

tyInt :: SourceType
tyInt = srcTypeConstructor C.tyInt

-- | `forall k. Row k`
kindOfREmpty :: SourceType
kindOfREmpty =
  ForAll nullSourceAnn TypeVarInvisible "k" (Just kindType) (kindRow (TypeVar nullSourceAnn "k")) Nothing

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Compute all unification variable indices appearing in a type.
unknownsInType :: forall a. Type a -> Array Int
unknownsInType = go
  where
  go (TUnknown _ u) = [u]
  go (TypeApp _ t1 t2) = go t1 <> go t2
  go (KindApp _ t1 t2) = go t1 <> go t2
  go (ForAll _ _ _ mbK ty _) = fromMaybe [] (map go mbK) <> go ty
  go (ConstrainedType _ (Constraint c) ty) =
    Array.concatMap go c.constraintKindArgs
    <> Array.concatMap go c.constraintArgs
    <> go ty
  go (Skolem _ _ mbK _ _) = fromMaybe [] (map go mbK)
  go (RCons _ _ ty rest) = go ty <> go rest
  go (KindedType _ ty k) = go ty <> go k
  go _ = []

-- | Deduplicated list of unknown indices in a type.
unknowns :: forall a. Type a -> Array Int
unknowns = Array.nub <<< unknownsInType

-- | Compute all type variable names used in a type (not just free ones).
usedTypeVariables :: forall a. Type a -> Array String
usedTypeVariables = Array.nub <<< go
  where
  go (TypeVar _ v) = [v]
  go (TypeApp _ t1 t2) = go t1 <> go t2
  go (KindApp _ t1 t2) = go t1 <> go t2
  go (ForAll _ _ arg mbK ty _) = [arg] <> fromMaybe [] (map go mbK) <> go ty
  go (ConstrainedType _ (Constraint c) ty) =
    Array.concatMap go c.constraintKindArgs
    <> Array.concatMap go c.constraintArgs
    <> go ty
  go (Skolem _ _ mbK _ _) = fromMaybe [] (map go mbK)
  go (RCons _ _ ty rest) = go ty <> go rest
  go (KindedType _ ty k) = go ty <> go k
  go _ = []

-- | Replace unknowns with named type variables using a mapping.
replaceUnknownsWithVars :: Array (Tuple Unknown (Tuple String SourceType)) -> SourceType -> SourceType
replaceUnknownsWithVars binders ty
  | Array.null binders = ty
  | otherwise = everywhereOnTypes go ty
  where
  go (TUnknown ann unk) = case Array.find (\(Tuple u _) -> u == unk) binders of
    Just (Tuple _ (Tuple name _)) -> TypeVar ann name
    Nothing -> TUnknown ann unk
  go other = other

-- | Generate fresh variable names for unknowns, avoiding existing names.
unknownVarNames :: Array String -> Array (Tuple Unknown SourceType) -> Array (Tuple Unknown (Tuple String SourceType))
unknownVarNames used unks =
  Array.zipWith (\(Tuple a b) n -> Tuple a (Tuple n b)) unks freshNames
  where
  allVars :: Array String
  allVars = case Array.length unks of
    1 -> Array.cons "k" (map (\i -> "k" <> show i) (Array.range 1 100))
    _ -> map (\i -> "k" <> show i) (Array.range 1 100)

  freshNames :: Array String
  freshNames = Array.take (Array.length unks) (Array.filter (\v -> not (Array.elem v used)) allVars)

-- | Generalize a type over given unknowns.
generalizeUnknowns :: Array (Tuple Unknown SourceType) -> SourceType -> SourceType
generalizeUnknowns unks ty =
  generalizeUnknownsWithVars (unknownVarNames (usedTypeVariables ty) unks) ty

generalizeUnknownsWithVars :: Array (Tuple Unknown (Tuple String SourceType)) -> SourceType -> SourceType
generalizeUnknownsWithVars binders ty =
  mkForAll
    (map (\(Tuple _ (Tuple name k)) -> Tuple (getAnnForType ty) (Tuple name (Just (replaceUnknownsWithVars binders k)))) binders)
    (replaceUnknownsWithVars binders ty)

-- ---------------------------------------------------------------------------
-- Substitution
-- ---------------------------------------------------------------------------

-- | Substitute solved unknowns in a type
applySubst :: Substitution -> SourceType -> SourceType
applySubst (Substitution sub) = everywhereOnTypes go
  where
  go (TUnknown ann u) = case Map.lookup u sub.substType of
    Nothing -> TUnknown ann u
    Just (TUnknown ann' u1) | u1 == u -> TUnknown ann' u1
    Just t -> applySubst (Substitution sub) t
  go other = other

-- | Apply current substitution to a type
applyM :: forall m. MonadState CheckState m => SourceType -> m SourceType
applyM ty = do
  subst <- gets \(CheckState cs) -> cs.checkSubstitution
  pure (applySubst subst ty)

-- ---------------------------------------------------------------------------
-- Fresh unknowns
-- ---------------------------------------------------------------------------

freshUnknown :: forall m. MonadState CheckState m => m Unknown
freshUnknown = do
  k <- gets \(CheckState cs) -> cs.checkNextType
  modify_ \(CheckState cs) -> CheckState cs { checkNextType = k + 1 }
  pure k

freshKind :: forall m. MonadState CheckState m => SourceSpan -> m SourceType
freshKind ss = freshKindWithKind ss kindType

freshKindWithKind :: forall m. MonadState CheckState m => SourceSpan -> SourceType -> m SourceType
freshKindWithKind ss kind = do
  u <- freshUnknown
  addUnsolved Nothing u kind
  pure (TUnknown (Tuple ss []) u)

addUnsolved :: forall m. MonadState CheckState m => Maybe UnkLevel -> Unknown -> SourceType -> m Unit
addUnsolved lvl unk kind = modify_ \(CheckState cs) ->
  let Substitution subs = cs.checkSubstitution
      newLvl = UnkLevel (case lvl of
        Nothing -> NEL.singleton unk
        Just (UnkLevel lvl') -> NEL.snoc lvl' unk)
      uns = Map.insert unk (Tuple newLvl kind) subs.substUnsolved
  in CheckState cs { checkSubstitution = Substitution subs { substUnsolved = uns } }

solve :: forall m. MonadState CheckState m => Unknown -> SourceType -> m Unit
solve unk solution = modify_ \(CheckState cs) ->
  let Substitution subs = cs.checkSubstitution
  in CheckState cs { checkSubstitution = Substitution subs { substType = Map.insert unk solution subs.substType } }

lookupUnsolved
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Unknown
  -> m (Tuple UnkLevel SourceType)
lookupUnsolved u = do
  uns <- gets \(CheckState cs) -> let Substitution subs = cs.checkSubstitution in subs.substUnsolved
  case Map.lookup u uns of
    Nothing -> internalCompilerError ("Unsolved unification variable ?" <> show u <> " is not bound")
    Just res -> pure res

-- ---------------------------------------------------------------------------
-- Bind local type variables (like Haskell's bindLocalTypeVariables)
-- ---------------------------------------------------------------------------

bindLocalTypeVariables
  :: forall m a
   . MonadState CheckState m
  => ModuleName
  -> Array (Tuple (ProperName TypeName) SourceType)
  -> m a
  -> m a
bindLocalTypeVariables moduleName bindings =
  bindTypes
    (Map.fromFoldable
      (map (\(Tuple pn kind) -> Tuple (Qualified (ByModuleName moduleName) pn) (Tuple kind LocalTypeVariable))
        bindings))

-- ---------------------------------------------------------------------------
-- unknownsWithKinds
-- ---------------------------------------------------------------------------

unknownsWithKinds
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Array Unknown
  -> m (Array (Tuple Unknown SourceType))
unknownsWithKinds us = do
  results <- traverse go us
  -- Deduplicate by unknown index, keep first occurrence, sort by level
  let flat = Array.concatMap identity results
      deduped = Array.nubByEq (\(Tuple u1 _) (Tuple u2 _) -> u1 == u2) flat
  pure (map snd deduped)
  where
  go u = do
    Tuple lvl ty <- lookupUnsolved u
    ty' <- applyM ty
    let uks = unknowns ty'
    rest <- traverse go uks
    let restFlat = Array.concatMap identity rest
    pure (Array.cons (Tuple lvl (Tuple u ty')) restFlat)

-- ---------------------------------------------------------------------------
-- Core kind inference
-- ---------------------------------------------------------------------------

inferKind
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> m (Tuple SourceType SourceType)
inferKind tyToInfer =
  withErrorMessageHint (ErrorInferringKind tyToInfer)
    $ rethrowWithPosition (fst (getAnnForType tyToInfer))
    $ go tyToInfer
  where
  go = case _ of
    ty@(TypeConstructor ann v@(Qualified qb name)) -> do
      env <- getEnv
      let Environment e = env
      -- Normalize BySourcePos qualifiers to ByModuleName for same-module references
      mResult <- case qb of
        BySourcePos _ -> do
          mn <- unsafeCheckCurrentModule
          pure (Map.lookup (Qualified (ByModuleName mn) name) e.types)
        ByModuleName _ -> pure (Map.lookup v e.types)
      case mResult of
        Nothing ->
          throwError (errorMessage' (fst ann) (UnknownName (map TyName v)))
        Just (Tuple kind LocalTypeVariable) -> do
          kind' <- applyM kind
          pure (Tuple ty (setAnnForType ann kind'))
        Just (Tuple kind _) ->
          pure (Tuple ty (setAnnForType ann kind))

    ConstrainedType ann' con@(Constraint c) ty -> do
      env <- getEnv
      let Environment e = env
          clsAsType = coerceProperName <$> c.constraintClass
      case Map.lookup clsAsType e.types of
        Nothing ->
          throwError (errorMessage' (fst c.constraintAnn) (UnknownName (map TyClassName c.constraintClass)))
        Just _ -> do
          con' <- checkConstraint con
          ty' <- checkIsSaturatedType ty
          pure (Tuple (ConstrainedType ann' con' ty') (setAnnForType ann' kindType))

    ty@(TypeLevelString ann _) ->
      pure (Tuple ty (setAnnForType ann kindSymbol))

    ty@(TypeLevelInt ann _) ->
      pure (Tuple ty (setAnnForType ann tyInt))

    ty@(TypeVar ann v) -> do
      moduleName <- unsafeCheckCurrentModule
      kind <- applyM =<< lookupTypeVariable moduleName (Qualified byNullSourcePos (ProperName v))
      pure (Tuple ty (setAnnForType ann kind))

    ty@(Skolem ann _ mbK _ _) -> do
      kind <- applyM (unsafePartial (fromJust mbK))
      pure (Tuple ty (setAnnForType ann kind))

    ty@(TUnknown ann u) -> do
      kind <- applyM <<< snd =<< lookupUnsolved u
      pure (Tuple ty (setAnnForType ann kind))

    ty@(TypeWildcard ann _) -> do
      k <- freshKind (fst ann)
      pure (Tuple ty (setAnnForType ann k))

    ty@(REmpty ann) ->
      pure (Tuple ty (setAnnForType ann kindOfREmpty))

    ty@(RCons ann _ _ _) -> do
      let Tuple rowList rowTail = rowToList ty
      kr <- freshKind (fst ann)
      rowList' <- for rowList \(RowListItem item) ->
        RowListItem <<< item { rowListType = _ } <$> checkKind item.rowListType kr
      rowTail' <- checkKind rowTail (kindRow kr)
      kr' <- applyM kr
      pure (Tuple (rowFromList (Tuple rowList' rowTail')) (setAnnForType ann (kindRow kr')))

    TypeApp ann t1 t2 -> do
      Tuple t1' k1 <- go t1
      inferAppKind ann (Tuple t1' k1) t2

    KindApp ann t1 t2 -> do
      Tuple t1' kind <- do
        Tuple t1'' k <- go t1
        k' <- applyM k
        pure (Tuple t1'' k')
      case kind of
        ForAll _ _ arg (Just argKind) resKind _ -> do
          t2' <- checkKind t2 argKind
          pure (Tuple (KindApp ann t1' t2') (replaceTypeVars arg t2' resKind))
        _ ->
          internalCompilerError "inferKind: unkinded forall binder"

    KindedType _ t1 t2 -> do
      t2' <- replaceAllTypeSynonyms <<< fst =<< go t2
      t1' <- checkKind t1 t2'
      t2'' <- applyM t2'
      pure (Tuple t1' t2'')

    ForAll ann vis arg mbKind ty sc -> do
      moduleName <- unsafeCheckCurrentModule
      kind <- case mbKind of
        Just k -> replaceAllTypeSynonyms =<< checkIsSaturatedType k
        Nothing -> freshKind (fst ann)
      Tuple ty' unks <- bindLocalTypeVariables moduleName [Tuple (ProperName arg) kind] do
        ty' <- applyM =<< checkIsSaturatedType ty
        unks <- unknownsWithKinds (unknowns ty')
        pure (Tuple ty' unks)
      for_ unks \(Tuple u k) -> addUnsolved Nothing u k
      pure (Tuple (ForAll ann vis arg (Just kind) ty' sc) (setAnnForType ann kindType))

    ParensInType _ ty ->
      go ty

    ty ->
      internalCompilerError ("inferKind: Unimplemented case: " <> show ty)

inferAppKind
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceAnn
  -> Tuple SourceType SourceType
  -> SourceType
  -> m (Tuple SourceType SourceType)
inferAppKind ann (Tuple fn fnKind) arg = case fnKind of
  TypeApp _ (TypeApp _ arrKind argKind) resKind | eqType arrKind tyFunction -> do
    arg' <- checkKind' false arg argKind
    resKind' <- applyM resKind
    pure (Tuple (TypeApp ann fn arg') resKind')
  TUnknown _ u -> do
    Tuple lvl _ <- lookupUnsolved u
    u1 <- freshUnknown
    u2 <- freshUnknown
    addUnsolved (Just lvl) u1 kindType
    addUnsolved (Just lvl) u2 kindType
    solve u (setAnnForType ann (TypeApp nullSourceAnn (TypeApp nullSourceAnn tyFunction (TUnknown ann u1)) (TUnknown ann u2)))
    arg' <- checkKind arg (TUnknown ann u1)
    pure (Tuple (TypeApp ann fn arg') (TUnknown ann u2))
  ForAll _ _ a (Just k) ty _ -> do
    u <- freshUnknown
    addUnsolved Nothing u k
    inferAppKind ann (Tuple (KindApp ann fn (TUnknown ann u)) (replaceTypeVars a (TUnknown ann u) ty)) arg
  _ ->
    cannotApplyTypeToType fn arg

eqType :: forall a b. Type a -> Type b -> Boolean
eqType t1 t2 = (void t1 :: Type Unit) == void t2

cannotApplyTypeToType
  :: forall m a
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m a
cannotApplyTypeToType fn arg = do
  argKind <- snd <$> inferKind arg
  retKind <- freshKind nullSourceSpan
  _ <- checkKind fn (TypeApp nullSourceAnn (TypeApp nullSourceAnn tyFunction argKind) retKind)
  internalCompilerError "Cannot apply type to type"

checkKind
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m SourceType
checkKind = checkKind' false

checkIsSaturatedType
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> m SourceType
checkIsSaturatedType ty = checkKind' true ty kindType

checkKind'
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Boolean
  -> SourceType
  -> SourceType
  -> m SourceType
checkKind' requireSynonymsToExpand ty kind2 =
  withErrorMessageHint (ErrorCheckingKind ty kind2)
    $ rethrowWithPosition (fst (getAnnForType ty))
    $ do
        Tuple ty' kind1 <- inferKind ty
        kind1' <- applyM kind1
        kind2' <- applyM kind2
        when requireSynonymsToExpand (void (replaceAllTypeSynonyms ty'))
        instantiateKind (Tuple ty' kind1') kind2'

instantiateKind
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Tuple SourceType SourceType
  -> SourceType
  -> m SourceType
instantiateKind (Tuple ty kind1) kind2 = case kind1 of
  ForAll _ _ a (Just k) t _ | shouldInstantiate kind2 -> do
    let ann = getAnnForType ty
    u <- freshKindWithKind (fst ann) k
    instantiateKind (Tuple (KindApp ann ty u) (replaceTypeVars a u t)) kind2
  _ -> do
    subsumesKind kind1 kind2
    pure ty
  where
  shouldInstantiate = case _ of
    ForAll _ _ _ _ _ _ -> false
    _ -> true

subsumesKind
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m Unit
subsumesKind = go
  where
  go k1 k2 = case Tuple k1 k2 of
    Tuple (TypeApp _ (TypeApp _ arr1 a1) a2) (TypeApp _ (TypeApp _ arr2 b1) b2)
      | eqType arr1 tyFunction
      , eqType arr2 tyFunction -> do
          go b1 a1
          join (go <$> applyM a2 <*> applyM b2)
    Tuple a (ForAll ann _ var mbKind b mbScope) -> do
      scope <- case mbScope of
        Just s -> pure s
        Nothing -> newSkolemScope
      skolc <- newSkolemConstant
      go a (skolemize ann var mbKind skolc scope b)
    Tuple (ForAll ann _ var (Just kind) a _) b -> do
      a' <- freshKindWithKind (fst ann) kind
      go (replaceTypeVars var a' a) b
    Tuple (TUnknown ann u) b@(TypeApp _ (TypeApp _ arr _) _)
      | eqType arr tyFunction
      , not (Array.elem u (unknowns b)) -> do
          uarr <- solveUnknownAsFunction ann u
          join (go <$> applyM uarr <*> pure b)
    Tuple a@(TypeApp _ (TypeApp _ arr _) _) (TUnknown ann u)
      | eqType arr tyFunction
      , not (Array.elem u (unknowns a)) -> do
          uarr <- solveUnknownAsFunction ann u
          join (go <$> pure a <*> applyM uarr)
    Tuple a b ->
      unifyKinds a b

unifyKinds
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m Unit
unifyKinds = unifyKindsWithFailure \w1 w2 ->
  throwError (errorMessage (KindsDoNotUnify w1 w2))

unifyKinds'
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m Unit
unifyKinds' = unifyKindsWithFailure \_ _ ->
  internalCompilerError "unifyKinds': kinds do not unify"

checkTypeKind
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m Unit
checkTypeKind ty kind =
  unifyKindsWithFailure (\_ _ -> throwError (errorMessage (ExpectedType ty kind))) kind kindType

unifyKindsWithFailure
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => (SourceType -> SourceType -> m Unit)
  -> SourceType
  -> SourceType
  -> m Unit
unifyKindsWithFailure onFailure = go
  where
  goWithLabel lbl t1 t2 = withErrorMessageHint (ErrorInRowLabel lbl) (go t1 t2)

  go k1 k2 = case Tuple k1 k2 of
    Tuple (TypeApp _ p1 p2) (TypeApp _ p3 p4) -> do
      go p1 p3
      join (go <$> applyM p2 <*> applyM p4)
    Tuple (KindApp _ p1 p2) (KindApp _ p3 p4) -> do
      go p1 p3
      join (go <$> applyM p2 <*> applyM p4)
    Tuple r1@(RCons _ _ _ _) r2 ->
      unifyRows r1 r2
    Tuple r1 r2@(RCons _ _ _ _) ->
      unifyRows r1 r2
    Tuple r1@(REmpty _) r2 ->
      unifyRows r1 r2
    Tuple r1 r2@(REmpty _) ->
      unifyRows r1 r2
    Tuple w1 w2 | eqType w1 w2 ->
      pure unit
    Tuple (TUnknown _ a') p1 ->
      solveUnknown a' p1
    Tuple p1 (TUnknown _ a') ->
      solveUnknown a' p1
    Tuple w1 w2 ->
      onFailure w1 w2

  unifyRows r1 r2 = do
    let Tuple matches rest = alignRowsWith goWithLabel r1 r2
    traverse_ identity matches
    unifyTails rest

  unifyTails rest = case rest of
    Tuple (Tuple [] (TUnknown _ a')) (Tuple rs p1) ->
      solveUnknown a' (rowFromList (Tuple rs p1))
    Tuple (Tuple rs p1) (Tuple [] (TUnknown _ a')) ->
      solveUnknown a' (rowFromList (Tuple rs p1))
    Tuple (Tuple [] w1) (Tuple [] w2) | eqType w1 w2 ->
      pure unit
    Tuple (Tuple rs1 (TUnknown _ u1)) (Tuple rs2 (TUnknown _ u2)) | u1 /= u2 -> do
      rest' <- freshKind nullSourceSpan
      solveUnknown u1 (rowFromList (Tuple rs2 rest'))
      solveUnknown u2 (rowFromList (Tuple rs1 rest'))
    Tuple w1 w2 ->
      onFailure (rowFromList w1) (rowFromList w2)

solveUnknown
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Unknown
  -> SourceType
  -> m Unit
solveUnknown a' p1 = do
  p2 <- promoteKind a' p1
  w1 <- snd <$> lookupUnsolved a'
  join (unifyKinds <$> applyM w1 <*> elaborateKind p2)
  solve a' p2

solveUnknownAsFunction
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceAnn
  -> Unknown
  -> m SourceType
solveUnknownAsFunction ann u = do
  lvl <- fst <$> lookupUnsolved u
  u1 <- freshUnknown
  u2 <- freshUnknown
  addUnsolved (Just lvl) u1 kindType
  addUnsolved (Just lvl) u2 kindType
  let uarr = setAnnForType ann (TypeApp nullSourceAnn (TypeApp nullSourceAnn tyFunction (TUnknown ann u1)) (TUnknown ann u2))
  solve u uarr
  pure uarr

promoteKind
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Unknown
  -> SourceType
  -> m SourceType
promoteKind u2 ty = do
  Tuple lvl2 _ <- lookupUnsolved u2
  everywhereOnTypesM (goPromote lvl2) ty
  where
  goPromote :: UnkLevel -> SourceType -> m SourceType
  goPromote lvl2 ty' = case ty' of
    TUnknown ann u1 -> do
      when (u1 == u2) (throwError (errorMessage (InfiniteKind ty')))
      Tuple lvl1 k <- lookupUnsolved u1
      if lvl1 < lvl2 then
        pure ty'
      else do
        k' <- promoteKind u2 =<< applyM k
        u1' <- freshUnknown
        Tuple lvl2' _ <- lookupUnsolved u2
        addUnsolved (Just lvl2') u1' k'
        solve u1 (TUnknown ann u1')
        pure (TUnknown ann u1')
    _ -> pure ty'

elaborateKind
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> m SourceType
elaborateKind = case _ of
  TypeLevelString ann _ ->
    pure (setAnnForType ann kindSymbol)
  TypeLevelInt ann _ ->
    pure (setAnnForType ann tyInt)
  TypeConstructor ann v@(Qualified qb name) -> do
    env <- getEnv
    let Environment e = env
    mResult <- case qb of
      BySourcePos _ -> do
        mn <- unsafeCheckCurrentModule
        pure (Map.lookup (Qualified (ByModuleName mn) name) e.types)
      ByModuleName _ -> pure (Map.lookup v e.types)
    case mResult of
      Nothing ->
        throwError (errorMessage' (fst ann) (UnknownName (map TyName v)))
      Just (Tuple kind _) ->
        setAnnForType ann <$> applyM kind
  TypeVar ann a -> do
    moduleName <- unsafeCheckCurrentModule
    kind <- applyM =<< lookupTypeVariable moduleName (Qualified byNullSourcePos (ProperName a))
    pure (setAnnForType ann kind)
  Skolem ann _ mbK _ _ -> do
    k <- case mbK of
      Nothing -> internalCompilerError "Skolem has no kind"
      Just k' -> pure k'
    kind <- applyM k
    pure (setAnnForType ann kind)
  TUnknown ann a' -> do
    kind <- snd <$> lookupUnsolved a'
    setAnnForType ann <$> applyM kind
  REmpty ann ->
    pure (setAnnForType ann kindOfREmpty)
  RCons ann _ t1 _ -> do
    k1 <- elaborateKind t1
    pure (setAnnForType ann (kindRow k1))
  ty@(TypeApp ann t1 t2) -> do
    k1 <- elaborateKind t1
    case k1 of
      TypeApp _ (TypeApp _ k _) w2 | eqType k tyFunction ->
        pure (setAnnForType ann w2)
      TUnknown a u -> do
        _ <- solveUnknownAsFunction a u
        elaborateKind ty
      _ ->
        internalCompilerError "elaborateKind: cannot apply type to type"
  KindApp ann t1 t2 -> do
    k1 <- elaborateKind t1
    case k1 of
      ForAll _ _ a _ n _ ->
        flip (replaceTypeVars a) n <<< setAnnForType ann <$> applyM t2
      _ ->
        internalCompilerError "elaborateKind: cannot apply kind to type"
  ForAll ann _ _ _ _ _ ->
    pure (setAnnForType ann kindType)
  ConstrainedType ann _ _ ->
    pure (setAnnForType ann kindType)
  KindedType ann _ k ->
    pure (setAnnForType ann k)
  ty ->
    throwError (errorMessage' (fst (getAnnForType ty)) (UnsupportedTypeInKind ty))

-- ---------------------------------------------------------------------------
-- Entry points
-- ---------------------------------------------------------------------------

kindOfWithUnknowns
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> m (Tuple SourceType (Array (Tuple Unknown SourceType)))
kindOfWithUnknowns ty = do
  Tuple ty' kind <- kindOf ty
  unks <- unknownsWithKinds (unknowns ty')
  pure (Tuple kind unks)

kindOf
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> m (Tuple SourceType SourceType)
kindOf ty = do
  Tuple (Tuple _ ty') kind <- kindOfWithScopedVars ty
  pure (Tuple ty' kind)

kindOfWithScopedVars
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> m (Tuple (Tuple (Array (Tuple String SourceType)) SourceType) SourceType)
kindOfWithScopedVars ty = do
  Tuple ty' kind <- do
    Tuple ty'' k <- inferKind ty
    ty''' <- applyM ty''
    k' <- replaceAllTypeSynonyms =<< applyM k
    pure (Tuple ty''' k')
  let binders = case completeBinderList ty' of
        Just (Tuple bs _) -> map (\(Tuple _ (Tuple v k)) -> Tuple v k) bs
        Nothing -> []
  pure (Tuple (Tuple binders ty') kind)

-- ---------------------------------------------------------------------------
-- Constraint checking
-- ---------------------------------------------------------------------------

checkConstraint
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceConstraint
  -> m SourceConstraint
checkConstraint (Constraint c) = do
  let ty = Array.foldl (TypeApp c.constraintAnn)
             (Array.foldl (KindApp c.constraintAnn) (TypeConstructor c.constraintAnn (coerceProperName <$> c.constraintClass)) c.constraintKindArgs)
             c.constraintArgs
  ty' <- checkKind ty kindConstraint
  let Tuple _ (Tuple kinds' args') = unapplyTypes ty'
  pure (Constraint c { constraintKindArgs = kinds', constraintArgs = args' })

applyConstraint
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceConstraint
  -> m SourceConstraint
applyConstraint (Constraint c) = do
  let ty = Array.foldl (TypeApp c.constraintAnn)
             (Array.foldl (KindApp c.constraintAnn) (TypeConstructor c.constraintAnn (coerceProperName <$> c.constraintClass)) c.constraintKindArgs)
             c.constraintArgs
  ty' <- applyM ty
  let Tuple _ (Tuple kinds' args') = unapplyTypes ty'
  pure (Constraint c { constraintKindArgs = kinds', constraintArgs = args' })

-- | Split a fully-applied type into its constructor and kind/type arguments.
-- | Returns (constructor, kindArgs, typeArgs)
unapplyTypes :: SourceType -> Tuple SourceType (Tuple (Array SourceType) (Array SourceType))
unapplyTypes = goTypes []
  where
  goTypes args ty = case ty of
    TypeApp _ f arg -> goTypes (Array.cons arg args) f
    KindApp _ f arg -> goKinds [arg] args f
    other -> Tuple other (Tuple [] args)

  goKinds kargs args ty = case ty of
    KindApp _ f arg -> goKinds (Array.cons arg kargs) args f
    other -> Tuple other (Tuple kargs args)

-- | Split a constrained type into its constraints and body.
unapplyConstraints :: SourceType -> Tuple (Array SourceConstraint) SourceType
unapplyConstraints = go []
  where
  go acc (ConstrainedType _ c ty) = go (Array.snoc acc c) ty
  go acc ty = Tuple acc ty

-- ---------------------------------------------------------------------------
-- Instance declaration checking
-- ---------------------------------------------------------------------------

-- | Check an instance declaration.
-- | Returns (deps', kinds', tys', vars) matching Haskell's checkInstanceDeclaration.
checkInstanceDeclaration
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => ModuleName
  -> Tuple SourceAnn (Tuple (Array SourceConstraint) (Tuple (Qualified (ProperName ClassName)) (Array SourceType)))
  -> m (Tuple (Array SourceConstraint) (Tuple (Array SourceType) (Tuple (Array SourceType) (Array (Tuple String SourceType)))))
checkInstanceDeclaration _mn (Tuple _sa (Tuple deps (Tuple _className tys))) =
  -- Stub: return deps unchanged, no kinds, same tys, no vars
  pure (Tuple deps (Tuple [] (Tuple tys [])))

-- ---------------------------------------------------------------------------
-- Kind declaration checking
-- ---------------------------------------------------------------------------

checkKindDeclaration
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => ModuleName
  -> SourceType
  -> m SourceType
checkKindDeclaration _ ty = do
  Tuple ty' kind <- kindOf ty
  checkTypeKind kind kindType
  ty'' <- replaceAllTypeSynonyms ty'
  unks <- unknownsWithKinds (unknowns ty'')
  finalTy <- (generalizeUnknowns unks) <$> freshenForAlls ty' ty''
  checkQuantification finalTy
  checkValidKind finalTy
  where

  freshVar :: String -> m String
  freshVar arg = (arg <> _) <<< show <$> fresh

  freshenForAlls :: SourceType -> SourceType -> m SourceType
  freshenForAlls orig elaborated = case Tuple orig elaborated of
    Tuple (ForAll _ _ v1 _ ty1 _) (ForAll a2 vis v2 k2 ty2 sc2) | v1 == v2 -> do
      ty2' <- freshenForAlls ty1 ty2
      pure (ForAll a2 vis v2 k2 ty2' sc2)
    Tuple _ ty2 -> goFresh ty2
    where
    goFresh = case _ of
      ForAll a' vis v' k' ty' sc' -> do
        v'' <- freshVar v'
        ty'' <- goFresh (replaceTypeVars v' (TypeVar a' v'') ty')
        pure (ForAll a' vis v'' k' ty'' sc')
      other -> pure other

  checkValidKind :: SourceType -> m SourceType
  checkValidKind = everywhereOnTypesM \ty' -> case ty' of
    ConstrainedType ann _ _ ->
      throwError (errorMessage' (fst ann) (UnsupportedTypeInKind ty'))
    other -> pure other

-- ---------------------------------------------------------------------------
-- checkTypeKind (with source span / name overload from stub)
-- ---------------------------------------------------------------------------

-- NOTE: The original stub had a different signature for checkTypeKind.
-- We keep the core one above and add this as a no-op for compatibility.
-- (The real checkTypeKind is above, taking two SourceType args.)

-- ---------------------------------------------------------------------------
-- Quantification checks
-- ---------------------------------------------------------------------------

checkQuantification
  :: forall m
   . MonadError MultipleErrors m
  => SourceType
  -> m Unit
checkQuantification ty = do
  let binders = case completeBinderList ty of
        Just (Tuple bs _) -> bs
        Nothing -> []
  let vars = collectBadVars [] [] binders
  unless (Array.null vars) $
    throwError (Array.foldMap (\(Tuple ann arg) -> errorMessage' (fst ann) (QuantificationCheckFailureInKind arg)) vars)
  where
  collectBadVars :: Array (Tuple SourceAnn String) -> Array String -> Array (Tuple SourceAnn (Tuple String SourceType)) -> Array (Tuple SourceAnn String)
  collectBadVars acc _ binders
    | Array.null binders = Array.reverse acc
  collectBadVars acc sco binders = case Array.uncons binders of
    Nothing -> Array.reverse acc
    Just { head: Tuple ann (Tuple arg k), tail: rest }
      | not (Array.all (\v -> Array.elem v sco) (freeTypeVariables k)) -> goDeps acc arg rest
      | otherwise -> collectBadVars acc (Array.cons arg sco) rest

  goDeps :: Array (Tuple SourceAnn String) -> String -> Array (Tuple SourceAnn (Tuple String SourceType)) -> Array (Tuple SourceAnn String)
  goDeps acc _ binders
    | Array.null binders = acc
  goDeps acc karg binders = case Array.uncons binders of
    Nothing -> acc
    Just { head: Tuple ann (Tuple arg k), tail: rest } ->
      let isDep = Array.elem karg (freeTypeVariables k)
      in if isDep && arg == karg then Array.cons (Tuple ann arg) acc
         else if isDep then goDeps (Array.cons (Tuple ann arg) acc) karg rest
         else goDeps acc karg rest


checkVisibleTypeQuantification
  :: forall m
   . MonadError MultipleErrors m
  => SourceType
  -> m Unit
checkVisibleTypeQuantification ty = do
  let vars = freeTypeVariables ty
  unless (Array.null vars) $
    throwError (Array.foldMap (errorMessage <<< VisibleQuantificationCheckFailureInType) vars)

checkEscapedSkolems
  :: forall m
   . MonadError MultipleErrors m
  => SourceType
  -> m Unit
checkEscapedSkolems ty = do
  let errors = collectSkolems ty
  traverse_ throwError errors
  where
  collectSkolems :: SourceType -> Array MultipleErrors
  collectSkolems t = map (toSkolemError t) (findSkolems t)

  findSkolems :: SourceType -> Array (Tuple SourceAnn String)
  findSkolems (Skolem ann name _ _ _) = [Tuple ann name]
  findSkolems (TypeApp _ t1 t2) = findSkolems t1 <> findSkolems t2
  findSkolems (KindApp _ t1 t2) = findSkolems t1 <> findSkolems t2
  findSkolems (ForAll _ _ _ mbK body _) = fromMaybe [] (map findSkolems mbK) <> findSkolems body
  findSkolems (RCons _ _ t1 t2) = findSkolems t1 <> findSkolems t2
  findSkolems (KindedType _ t k) = findSkolems t <> findSkolems k
  findSkolems (ConstrainedType _ (Constraint c) t) =
    Array.concatMap findSkolems c.constraintKindArgs
    <> Array.concatMap findSkolems c.constraintArgs
    <> findSkolems t
  findSkolems _ = []

  toSkolemError t (Tuple ann name) =
    errorMessage' (fst (getAnnForType t)) (EscapedSkolem name (Just (fst ann)) t)

checkTypeQuantification
  :: forall m
   . MonadError MultipleErrors m
  => SourceType
  -> m Unit
checkTypeQuantification ty = do
  let errs = collectErrors ty
  unless (Array.null errs) (throwError (Array.foldMap identity errs))
  where
  collectErrors :: SourceType -> Array MultipleErrors
  collectErrors t = go true t
    where
    go _ (ForAll ann _ _ _ _ _) =
      let unks = unknowns t
      in if Array.null unks then []
         else [errorMessage' (fst ann) (QuantificationCheckFailureInType unks t)]
    go _ (KindApp ann _ _) =
      let unks = unknowns t
      in if Array.null unks then []
         else [errorMessage' (fst ann) (QuantificationCheckFailureInType unks t)]
    go _ (ConstrainedType ann _ _) =
      let unks = unknowns t
      in if Array.null unks then []
         else [errorMessage' (fst ann) (QuantificationCheckFailureInType unks t)]
    go _ _ = []

-- ---------------------------------------------------------------------------
-- Higher-level kind-of functions
-- ---------------------------------------------------------------------------

-- | Look up existing kind signature or create a fresh unknown.
existingSignatureOrFreshKind
  :: forall m
   . MonadState CheckState m
  => ModuleName
  -> SourceSpan
  -> ProperName TypeName
  -> m SourceType
existingSignatureOrFreshKind moduleName ss name = do
  env <- getEnv
  let Environment e = env
  case Map.lookup (Qualified (ByModuleName moduleName) name) e.types of
    Nothing -> freshKind ss
    Just (Tuple kind _) -> pure kind

-- ---------------------------------------------------------------------------
-- checkClassMemberDeclaration / applyClassMemberDeclaration / mapTypeDeclaration
-- ---------------------------------------------------------------------------

checkClassMemberDeclaration
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Declaration
  -> m Declaration
checkClassMemberDeclaration = case _ of
  TypeDeclaration (TypeDeclarationData td) -> do
    ty' <- checkKind td.tydeclType kindType
    pure (TypeDeclaration (TypeDeclarationData td { tydeclType = ty' }))
  _ -> internalCompilerError "Invalid class member declaration"

applyClassMemberDeclaration
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Declaration
  -> m Declaration
applyClassMemberDeclaration = case _ of
  TypeDeclaration (TypeDeclarationData td) -> do
    ty' <- applyM td.tydeclType
    pure (TypeDeclaration (TypeDeclarationData td { tydeclType = ty' }))
  _ -> internalCompilerError "Invalid class member declaration"

mapTypeDeclaration :: (SourceType -> SourceType) -> Declaration -> Declaration
mapTypeDeclaration f = case _ of
  TypeDeclaration (TypeDeclarationData td) ->
    TypeDeclaration (TypeDeclarationData td { tydeclType = f td.tydeclType })
  other -> other

-- ---------------------------------------------------------------------------
-- inferDataDeclaration
-- ---------------------------------------------------------------------------

-- | Type args: (SourceAnn, ProperName TypeName, Array (Tuple String (Maybe SourceType)), Array DataConstructorDeclaration)
-- | Result: Array (Tuple DataConstructorDeclaration SourceType)
inferDataDeclaration
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => ModuleName
  -> Tuple SourceAnn (Tuple (ProperName TypeName) (Tuple (Array (Tuple String (Maybe SourceType))) (Array DataConstructorDeclaration)))
  -> m (Array (Tuple DataConstructorDeclaration SourceType))
inferDataDeclaration moduleName (Tuple ann (Tuple tyName (Tuple tyArgs ctors))) = do
  tyKind <- applyM =<< lookupTypeVariable moduleName (Qualified byNullSourcePos tyName)
  let mBinders = completeBinderList tyKind
      Tuple sigBinders tyKind' = unsafePartial (fromJust mBinders)
  bindLocalTypeVariables moduleName (map (\(Tuple _ (Tuple v k)) -> Tuple (ProperName v) k) sigBinders) $ do
    tyArgs' <- for tyArgs (traverse (maybe (freshKind (fst ann)) (\t -> replaceAllTypeSynonyms =<< applyM =<< checkIsSaturatedType t)))
    subsumesKind (Array.foldr (\(Tuple _ k) acc -> function k acc) kindType tyArgs') tyKind'
    bindLocalTypeVariables moduleName (map (\(Tuple v k) -> Tuple (ProperName v) k) tyArgs') $ do
      let tyCtorName = srcTypeConstructor (mkQualified tyName moduleName)
          tyCtor = Array.foldl (\ty (Tuple _ (Tuple v _)) -> srcKindApp ty (srcTypeVar v)) tyCtorName sigBinders
          tyCtor' = Array.foldl (\ty (Tuple v _) -> srcTypeApp ty (srcTypeVar v)) tyCtor tyArgs'
          ctorBinders = map (\(Tuple sann (Tuple v mbk)) -> Tuple sann (Tuple v (Just mbk)))
                          (sigBinders <> map (\(Tuple v k) -> Tuple nullSourceAnn (Tuple v k)) tyArgs')
          visibility = map (\(Tuple v _) -> Tuple v TypeVarVisible) tyArgs
      for ctors (map (map (addVisibility visibility <<< mkForAll ctorBinders)) <<< inferDataConstructor tyCtor')

-- ---------------------------------------------------------------------------
-- inferDataConstructor
-- ---------------------------------------------------------------------------

inferDataConstructor
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> DataConstructorDeclaration
  -> m (Tuple DataConstructorDeclaration SourceType)
inferDataConstructor tyCtor (DataConstructorDeclaration dc) = do
  dataCtorFields' <- traverse (traverse checkIsSaturatedType) dc.dataCtorFields
  checkedTyCtor <- checkKind tyCtor kindType
  let dataCtor = Array.foldr (\(Tuple _ fieldTy) acc -> function fieldTy acc) checkedTyCtor dataCtorFields'
  pure (Tuple (DataConstructorDeclaration dc { dataCtorFields = dataCtorFields' }) dataCtor)

-- ---------------------------------------------------------------------------
-- inferTypeSynonym
-- ---------------------------------------------------------------------------

-- | Type args: (SourceAnn, ProperName TypeName, Array (Tuple String (Maybe SourceType)), SourceType)
-- | Result: SourceType (elaborated body)
inferTypeSynonym
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => ModuleName
  -> Tuple SourceAnn (Tuple (ProperName TypeName) (Tuple (Array (Tuple String (Maybe SourceType))) SourceType))
  -> m SourceType
inferTypeSynonym moduleName (Tuple ann (Tuple tyName (Tuple tyArgs tyBody))) = do
  tyKind <- applyM =<< lookupTypeVariable moduleName (Qualified byNullSourcePos tyName)
  let mBinders = completeBinderList tyKind
      Tuple sigBinders tyKind' = unsafePartial (fromJust mBinders)
  bindLocalTypeVariables moduleName (map (\(Tuple _ (Tuple v k)) -> Tuple (ProperName v) k) sigBinders) $ do
    kindRes <- freshKind (fst ann)
    tyArgs' <- for tyArgs (traverse (maybe (freshKind (fst ann)) (\t -> replaceAllTypeSynonyms =<< applyM =<< checkIsSaturatedType t)))
    unifyKinds tyKind' (Array.foldr (\(Tuple _ k) acc -> function k acc) kindRes tyArgs')
    bindLocalTypeVariables moduleName (map (\(Tuple v k) -> Tuple (ProperName v) k) tyArgs') $ do
      tyBodyAndKind <- traverse applyM =<< inferKind tyBody
      instantiateKind tyBodyAndKind =<< applyM kindRes

-- ---------------------------------------------------------------------------
-- inferClassDeclaration
-- ---------------------------------------------------------------------------

-- | Type args: (SourceAnn, ProperName ClassName, Array (Tuple String (Maybe SourceType)), Array SourceConstraint, Array Declaration)
-- | Result: (Array (Tuple String SourceType), Array SourceConstraint, Array Declaration)
inferClassDeclaration
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => ModuleName
  -> Tuple SourceAnn (Tuple (ProperName ClassName) (Tuple (Array (Tuple String (Maybe SourceType))) (Tuple (Array SourceConstraint) (Array Declaration))))
  -> m (Tuple (Array (Tuple String SourceType)) (Tuple (Array SourceConstraint) (Array Declaration)))
inferClassDeclaration moduleName (Tuple ann (Tuple clsName (Tuple clsArgs (Tuple superClasses decls)))) = do
  clsKind <- applyM =<< lookupTypeVariable moduleName (Qualified byNullSourcePos (coerceProperName clsName))
  let mBinders = completeBinderList clsKind
      Tuple sigBinders clsKind' = unsafePartial (fromJust mBinders)
  bindLocalTypeVariables moduleName (map (\(Tuple _ (Tuple v k)) -> Tuple (ProperName v) k) sigBinders) $ do
    clsArgs' <- for clsArgs (traverse (maybe (freshKind (fst ann)) (\t -> replaceAllTypeSynonyms =<< applyM =<< checkIsSaturatedType t)))
    unifyKinds clsKind' (Array.foldr (\(Tuple _ k) acc -> function k acc) kindConstraint clsArgs')
    bindLocalTypeVariables moduleName (map (\(Tuple v k) -> Tuple (ProperName v) k) clsArgs') $ do
      superClasses' <- for superClasses checkConstraint
      decls' <- for decls checkClassMemberDeclaration
      pure (Tuple clsArgs' (Tuple superClasses' decls'))

-- ---------------------------------------------------------------------------
-- kindsOfAll — main kind-inference function for binding groups
-- ---------------------------------------------------------------------------

-- | Infer kinds for a group of type synonyms, data declarations, and class declarations.
-- |
-- | Returns:
-- |   - Array (Tuple SourceType SourceType)          -- syn results: (elaborated body, kind)
-- |   - Array (Tuple (Array (Tuple DataConstructorDeclaration SourceType)) SourceType)  -- dat results: (ctors, kind)
-- |   - Array (Tuple (Array (Tuple String SourceType)) (Tuple (Array SourceConstraint) (Tuple (Array Declaration) SourceType)))  -- cls results
kindsOfAll
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => ModuleName
  -> Array (Tuple SourceAnn (Tuple (ProperName TypeName) (Tuple (Array (Tuple String (Maybe SourceType))) SourceType)))
  -> Array (Tuple SourceAnn (Tuple (ProperName TypeName) (Tuple (Array (Tuple String (Maybe SourceType))) (Array DataConstructorDeclaration))))
  -> Array (Tuple SourceAnn (Tuple (ProperName ClassName) (Tuple (Array (Tuple String (Maybe SourceType))) (Tuple (Array SourceConstraint) (Array Declaration)))))
  -> m
       ( Tuple
           (Array (Tuple SourceType SourceType))
           ( Tuple
               (Array (Tuple (Array (Tuple DataConstructorDeclaration SourceType)) SourceType))
               (Array (Tuple (Array (Tuple String SourceType)) (Tuple (Array SourceConstraint) (Tuple (Array Declaration) SourceType))))
           )
       )
kindsOfAll moduleName syns dats clss = withFreshSubstitution $ do
  -- Build dicts of name -> fresh kind variable (or existing signature)
  synDict <- for syns \(Tuple sa (Tuple synName _)) ->
    Tuple synName <$> existingSignatureOrFreshKind moduleName (fst sa) synName
  datDict <- for dats \(Tuple sa (Tuple datName _)) ->
    Tuple datName <$> existingSignatureOrFreshKind moduleName (fst sa) datName
  clsDict <- for clss \(Tuple sa (Tuple clsName _)) ->
    Tuple (coerceProperName clsName) <$> existingSignatureOrFreshKind moduleName (fst sa) (coerceProperName clsName)

  let bindingGroup = synDict <> datDict <> clsDict

  bindLocalTypeVariables moduleName bindingGroup $ do
    synResults <- for syns (inferTypeSynonym moduleName)
    datResults <- for dats (inferDataDeclaration moduleName)
    clsResults <- for clss (inferClassDeclaration moduleName)

    -- Apply substitution and collect unknowns for each entry
    synResultsWithUnks <- for (Array.zip synDict synResults) \(Tuple (Tuple synName synKind) synBody) -> do
      synKind' <- applyM synKind
      synBody' <- applyM synBody
      pure (Tuple (Tuple (Tuple synName synKind') synBody') (unknowns synKind'))

    datResultsWithUnks <- for (Array.zip datDict datResults) \(Tuple (Tuple datName datKind) ctors) -> do
      datKind' <- applyM datKind
      ctors' <- for ctors \(Tuple ctor ty) -> do
        ctor' <- traverseDataCtorFields (traverse (traverse applyM)) ctor
        ty' <- applyM ty
        pure (Tuple ctor' ty')
      pure (Tuple (Tuple (Tuple datName datKind') ctors') (unknowns datKind'))

    clsResultsWithUnks <- for (Array.zip clsDict clsResults) \(Tuple (Tuple clsName clsKind) (Tuple args (Tuple supersR declsR))) -> do
      clsKind' <- applyM clsKind
      args' <- for args (traverse applyM)
      supers' <- traverse applyConstraint supersR
      declsR' <- traverse applyClassMemberDeclaration declsR
      pure (Tuple (Tuple (Tuple clsName clsKind') (Tuple args' (Tuple supers' declsR'))) (unknowns clsKind'))

    -- Collect all unknown indices across all declarations
    let synUnks = map (\(Tuple (Tuple (Tuple synName _) _) unks) -> Tuple synName unks) synResultsWithUnks
        datUnks = map (\(Tuple (Tuple (Tuple datName _) _) unks) -> Tuple datName unks) datResultsWithUnks
        clsUnks = map (\(Tuple (Tuple (Tuple clsName _) _) unks) -> Tuple clsName unks) clsResultsWithUnks
        tysUnks = synUnks <> datUnks <> clsUnks

    allUnks <- unknownsWithKinds (Array.nub (Array.concatMap snd tysUnks))

    -- Build substitution: each name -> (type-ctor-applied-to-unknowns, those unknowns)
    let mkTySub (Tuple name unks) =
          let tyCtorName = mkQualified name moduleName
              tyUnks = Array.filter (\(Tuple u _) -> Array.elem u unks) allUnks
              tyCtor = Array.foldl (\ty (Tuple u _) -> srcKindApp ty (TUnknown nullSourceAnn u)) (srcTypeConstructor tyCtorName) tyUnks
          in Tuple tyCtorName (Tuple tyCtor tyUnks)
        tySubs = map mkTySub tysUnks

        findSub name = Array.findMap (\(Tuple k v) -> if k == name then Just v else Nothing) tySubs

        replaceTypeCtors = everywhereOnTypes \ty -> case ty of
          TypeConstructor _ name ->
            case findSub name of
              Just (Tuple tyCtor _) -> tyCtor
              Nothing -> ty
          other -> other

        usedTypeVariablesInDecls = (accumTypes usedTypeVariables).decl

    -- Process class results
    let clsResultsWithKinds = map
          (\(Tuple (Tuple (Tuple clsName clsKind) (Tuple args (Tuple supersR declsR))) _) ->
            let mSub = findSub (mkQualified clsName moduleName)
                tyUnks = case mSub of
                  Just (Tuple _ us) -> us
                  Nothing -> []
                usedVars =
                  usedTypeVariables clsKind
                  <> Array.concatMap (usedTypeVariables <<< snd) args
                  <> Array.concatMap (\(Constraint c) -> Array.concatMap usedTypeVariables (c.constraintKindArgs <> c.constraintArgs)) supersR
                  <> Array.concatMap usedTypeVariablesInDecls declsR
                unkBinders = unknownVarNames usedVars tyUnks
                args' = map (map (replaceUnknownsWithVars unkBinders <<< replaceTypeCtors)) args
                supers' = map (mapConstraintArgsAll (map (replaceUnknownsWithVars unkBinders <<< replaceTypeCtors))) supersR
                decls' = map (mapTypeDeclaration (replaceUnknownsWithVars unkBinders <<< replaceTypeCtors)) declsR
                clsKind' = generalizeUnknownsWithVars unkBinders clsKind
            in Tuple args' (Tuple supers' (Tuple decls' clsKind'))
          ) clsResultsWithUnks

    -- Process data results
    datResultsWithKinds <- for datResultsWithUnks \(Tuple (Tuple (Tuple datName datKind) ctors) _) -> do
      let mSub = findSub (mkQualified datName moduleName)
          tyUnks = case mSub of
            Just (Tuple _ us) -> us
            Nothing -> []
          replaceDataCtorField ty = replaceUnknownsWithVars (unknownVarNames (usedTypeVariables ty) tyUnks) (replaceTypeCtors ty)
          ctors' = map
            (\(Tuple ctor ty) ->
              Tuple
                (mapDataCtorFields (map (map replaceDataCtorField)) ctor)
                (generalizeUnknowns tyUnks (replaceTypeCtors ty))
            ) ctors
      for_ ctors' \(Tuple _ ty) -> checkTypeQuantification ty
      pure (Tuple ctors' (generalizeUnknowns tyUnks datKind))

    -- Process synonym results
    synResultsWithKinds <- for synResultsWithUnks \(Tuple (Tuple (Tuple synName synKind) synBody) _) -> do
      let mSub = findSub (mkQualified synName moduleName)
          tyUnks = case mSub of
            Just (Tuple _ us) -> us
            Nothing -> []
          unkBinders = unknownVarNames (usedTypeVariables synKind <> usedTypeVariables synBody) tyUnks
          genBody = replaceUnknownsWithVars unkBinders (replaceTypeCtors synBody)
          genSig = generalizeUnknownsWithVars unkBinders synKind
      checkEscapedSkolems genBody
      checkTypeQuantification genBody
      checkVisibleTypeQuantification genSig
      pure (Tuple genBody genSig)

    pure (Tuple synResultsWithKinds (Tuple datResultsWithKinds clsResultsWithKinds))

-- ---------------------------------------------------------------------------
-- kindOfData / kindOfTypeSynonym / kindOfClass
-- These delegate to kindsOfAll with a single-element list.
-- ---------------------------------------------------------------------------

-- | Infer the kind of a single data declaration.
-- | Input: (sa, name, args, dctors) — returns (dataCtors, ctorKind).
kindOfData
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadWriter MultipleErrors m
  => MonadSupply m
  => ModuleName
  -> Tuple SourceAnn (Tuple (ProperName TypeName) (Tuple (Array (Tuple String (Maybe SourceType))) (Array DataConstructorDeclaration)))
  -> m (Tuple (Array (Tuple DataConstructorDeclaration SourceType)) SourceType)
kindOfData mn decl = do
  Tuple _syns (Tuple datResults _clss) <- kindsOfAll mn [] [decl] []
  case Array.uncons datResults of
    Just { head } -> pure head
    Nothing -> internalCompilerError "kindOfData: empty result"

-- | Infer the kind of a single type synonym.
-- | Input: (sa, name, args, ty) — returns (elabTy, kind).
kindOfTypeSynonym
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadWriter MultipleErrors m
  => MonadSupply m
  => ModuleName
  -> Tuple SourceAnn (Tuple (ProperName TypeName) (Tuple (Array (Tuple String (Maybe SourceType))) SourceType))
  -> m (Tuple SourceType SourceType)
kindOfTypeSynonym mn decl = do
  Tuple synResults _rest <- kindsOfAll mn [decl] [] []
  case Array.uncons synResults of
    Just { head } -> pure head
    Nothing -> internalCompilerError "kindOfTypeSynonym: empty result"

-- | Infer the kind of a single type class.
-- | Input: (sa, pn, args, implies, decls) — returns (args', implies', decls', kind).
kindOfClass
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadWriter MultipleErrors m
  => MonadSupply m
  => ModuleName
  -> Tuple SourceAnn (Tuple (ProperName ClassName) (Tuple (Array (Tuple String (Maybe SourceType))) (Tuple (Array SourceConstraint) (Array Declaration))))
  -> m (Tuple (Array (Tuple String SourceType)) (Tuple (Array SourceConstraint) (Tuple (Array Declaration) SourceType)))
kindOfClass mn decl = do
  Tuple _syns (Tuple _dats clsResults) <- kindsOfAll mn [] [] [decl]
  case Array.uncons clsResults of
    Just { head } -> pure head
    Nothing -> internalCompilerError "kindOfClass: empty result"


