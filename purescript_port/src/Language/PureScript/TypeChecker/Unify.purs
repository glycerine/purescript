module Language.PureScript.TypeChecker.Unify
  ( freshType
  , freshTypeWithKind
  , solveType
  , substituteType
  , unknownsInType
  , unifyTypes
  , unifyRows
  , replaceTypeWildcards
  , varIfUnknown
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State.Class (class MonadState, gets, modify_)
import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.SourcePos (SourceAnn, SourceSpan)
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , errorMessage
  , onErrorMessages
  , rethrow
  , warnWithPosition
  , withoutPosition
  )
import Language.PureScript.TypeChecker.Kinds
  ( elaborateKind
  , freshKind
  , freshKindWithKind
  , instantiateKind
  , unifyKinds'
  , applySubst
  , kindType
  )
import Language.PureScript.TypeChecker.Monad
  ( CheckState(..)
  , Substitution(..)
  , UnkLevel(..)
  , Unknown
  , getLocalContext
  , guardWith
  , lookupUnkName
  , withErrorMessageHint
  )
import Language.PureScript.TypeChecker.Skolems (newSkolemConstant, newSkolemScope, skolemize)
import Language.PureScript.Types
  ( Constraint(..)
  , RowListItem(..)
  , SourceConstraint
  , SourceType
  , Type(..)
  , WildcardData(..)
  , alignRowsWith
  , everythingOnTypes
  , everywhereOnTypesM
  , getAnnForType
  , isREmptyKinded
  , mkForAll
  , rowFromList
  , srcTUnknown
  )
import Language.PureScript.AST.Declarations (ErrorMessageHint(..))
import Data.List.NonEmpty as NEL

-- | Generate a fresh type variable with an unknown kind
freshType :: forall m. MonadState CheckState m => m SourceType
freshType = do
  CheckState cs <- gets identity
  let t = cs.checkNextType
      Substitution sub = cs.checkSubstitution
  modify_ \(CheckState s) ->
    let Substitution subs = s.checkSubstitution
        newUnsolved = Map.insert t (Tuple (UnkLevel (NEL.singleton t)) kindType)
                    $ Map.insert (t + 1) (Tuple (UnkLevel (NEL.singleton (t + 1))) (srcTUnknown t))
                    $ subs.substUnsolved
    in CheckState s
         { checkNextType = t + 2
         , checkSubstitution = Substitution subs { substUnsolved = newUnsolved }
         }
  pure (srcTUnknown (cs.checkNextType + 1))

-- | Generate a fresh type variable with a known kind
freshTypeWithKind :: forall m. MonadState CheckState m => SourceType -> m SourceType
freshTypeWithKind kind = do
  t <- gets \(CheckState cs) -> cs.checkNextType
  modify_ \(CheckState cs) ->
    let Substitution subs = cs.checkSubstitution
        newUnsolved = Map.insert t (Tuple (UnkLevel (NEL.singleton t)) kind) subs.substUnsolved
    in CheckState cs
         { checkNextType = t + 1
         , checkSubstitution = Substitution subs { substUnsolved = newUnsolved }
         }
  pure (srcTUnknown t)

-- | Apply a substitution to a type
substituteType :: Substitution -> SourceType -> SourceType
substituteType = applySubst

-- | Update the substitution to solve a type constraint
solveType
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Int
  -> SourceType
  -> m Unit
solveType u t = rethrow (onErrorMessages withoutPosition) do
  occursCheck u t
  k1 <- elaborateKind t
  subst <- gets \(CheckState cs) -> cs.checkSubstitution
  let Substitution subs = subst
  k2 <- case Map.lookup u subs.substUnsolved of
    Nothing -> pure kindType
    Just (Tuple _ kind) -> pure (applySubst subst kind)
  t' <- instantiateKind (Tuple t k1) k2
  modify_ \(CheckState cs) ->
    let Substitution s = cs.checkSubstitution
    in CheckState cs
         { checkSubstitution = Substitution s { substType = Map.insert u t' s.substType } }

-- | Occurs check: ensure an unknown doesn't appear in a type
occursCheck :: forall m. MonadError MultipleErrors m => Int -> SourceType -> m Unit
occursCheck _ (TUnknown _ _) = pure unit
occursCheck u t = void $ everywhereOnTypesM go t
  where
  go (TUnknown _ u') | u == u' = throwError (errorMessage (InfiniteType t))
  go other = pure other

-- | Compute a list of all unknowns appearing in a type
unknownsInType :: forall a. Type a -> Array (Tuple a Int)
unknownsInType t = everythingOnTypes (<>) go t
  where
  go (TUnknown ann u) = [Tuple ann u]
  go _ = []

-- | Unify two types, updating the current substitution
unifyTypes
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m Unit
unifyTypes t1 t2 = do
  subst <- gets \(CheckState cs) -> cs.checkSubstitution
  withErrorMessageHint (ErrorUnifyingTypes t1 t2) $
    unifyTypes' (applySubst subst t1) (applySubst subst t2)
  where
  unifyTypes' (TUnknown _ u1) (TUnknown _ u2) | u1 == u2 = pure unit
  unifyTypes' (TUnknown _ u) t = solveType u t
  unifyTypes' t (TUnknown _ u) = solveType u t
  unifyTypes' (ForAll ann1 _ ident1 mbK1 ty1 sc1) (ForAll ann2 _ ident2 mbK2 ty2 sc2) =
    case Tuple sc1 sc2 of
      Tuple (Just sc1') (Just sc2') -> do
        sko <- newSkolemConstant
        scope <- newSkolemScope
        let sk1 = skolemize ann1 ident1 mbK1 sko sc1' ty1
            sk2 = skolemize ann2 ident2 mbK2 sko sc2' ty2
        unifyTypes sk1 sk2
      _ -> throwError (errorMessage (InternalCompilerError "callstack" "unifyTypes: unspecified skolem scope"))
  unifyTypes' (ForAll ann _ ident mbK ty1 (Just sc)) ty2 = do
    sko <- newSkolemConstant
    scope <- newSkolemScope
    let sk = skolemize ann ident mbK sko sc ty1
    unifyTypes sk ty2
  unifyTypes' (ForAll _ _ _ _ _ Nothing) _ =
    throwError (errorMessage (InternalCompilerError "callstack" "unifyTypes: unspecified skolem scope"))
  unifyTypes' ty f@(ForAll _ _ _ _ _ _) = unifyTypes f ty
  unifyTypes' (TypeVar _ v1) (TypeVar _ v2) | v1 == v2 = pure unit
  unifyTypes' ty1@(TypeConstructor _ c1) ty2@(TypeConstructor _ c2) =
    guardWith (errorMessage (TypesDoNotUnify ty1 ty2)) (c1 == c2)
  unifyTypes' (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = pure unit
  unifyTypes' (TypeLevelInt _ n1) (TypeLevelInt _ n2) | n1 == n2 = pure unit
  unifyTypes' (TypeApp _ t3 t4) (TypeApp _ t5 t6) = do
    unifyTypes t3 t5
    unifyTypes t4 t6
  unifyTypes' (KindApp _ t3 t4) (KindApp _ t5 t6) = do
    unifyKinds' t3 t5
    unifyTypes t4 t6
  unifyTypes' (Skolem _ _ _ s1 _) (Skolem _ _ _ s2 _) | s1 == s2 = pure unit
  unifyTypes' (KindedType _ ty1 _) ty2 = unifyTypes ty1 ty2
  unifyTypes' ty1 (KindedType _ ty2 _) = unifyTypes ty1 ty2
  unifyTypes' r1@(RCons _ _ _ _) r2 = unifyRows r1 r2
  unifyTypes' r1 r2@(RCons _ _ _ _) = unifyRows r1 r2
  unifyTypes' r1 r2 | isREmptyKinded r1 = unifyRows r1 r2
  unifyTypes' r1 r2 | isREmptyKinded r2 = unifyRows r1 r2
  unifyTypes' (ConstrainedType _ (Constraint c1) ty1) (ConstrainedType _ (Constraint c2) ty2)
    | c1.constraintClass == c2.constraintClass && c1.constraintData == c2.constraintData = do
        _ <- Array.zipWithA (\a b -> unifyTypes a b) c1.constraintArgs c2.constraintArgs
        unifyTypes ty1 ty2
  unifyTypes' ty1@(ConstrainedType _ _ _) ty2 =
    throwError (errorMessage (ConstrainedTypeUnified ty1 ty2))
  unifyTypes' t3 t4@(ConstrainedType _ _ _) = unifyTypes' t4 t3
  unifyTypes' t3 t4 = throwError (errorMessage (TypesDoNotUnify t3 t4))

-- | Unify two rows, updating the current substitution
unifyRows
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m Unit
unifyRows r1 r2 = do
  traverse_ identity matches
  unifyTails (Tuple lhs lhsTail) (Tuple rhs rhsTail)
  where
  unifyTypesWithLabel l t1 t2 = withErrorMessageHint (ErrorInRowLabel l) (unifyTypes t1 t2)

  Tuple ms (Tuple (Tuple lhs lhsTail) (Tuple rhs rhsTail)) = alignRowsWith unifyTypesWithLabel r1 r2
  matches = ms

  unifyTails
    :: Tuple (Array (RowListItem SourceAnn)) SourceType
    -> Tuple (Array (RowListItem SourceAnn)) SourceType
    -> m Unit
  unifyTails (Tuple [] (TUnknown _ u)) (Tuple sd r) = solveType u (rowFromList (Tuple sd r))
  unifyTails (Tuple sd r) (Tuple [] (TUnknown _ u)) = solveType u (rowFromList (Tuple sd r))
  unifyTails (Tuple [] t1) (Tuple [] t2) | isREmptyKinded t1 && isREmptyKinded t2 = pure unit
  unifyTails (Tuple [] (TypeVar _ v1)) (Tuple [] (TypeVar _ v2)) | v1 == v2 = pure unit
  unifyTails (Tuple [] (Skolem _ _ _ s1 _)) (Tuple [] (Skolem _ _ _ s2 _)) | s1 == s2 = pure unit
  unifyTails (Tuple sd1 (TUnknown a u1)) (Tuple sd2 (TUnknown _ u2)) | u1 /= u2 = do
    traverse_ (\(RowListItem item) -> occursCheck u2 item.rowListType) sd1
    traverse_ (\(RowListItem item) -> occursCheck u1 item.rowListType) sd2
    rest' <- freshTypeWithKind =<< elaborateKind (TUnknown a u1)
    solveType u1 (rowFromList (Tuple sd2 rest'))
    solveType u2 (rowFromList (Tuple sd1 rest'))
  unifyTails _ _ = throwError (errorMessage (TypesDoNotUnify r1 r2))

-- | Replace type wildcards with fresh unknowns
replaceTypeWildcards
  :: forall m
   . MonadWriter MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> m SourceType
replaceTypeWildcards = everywhereOnTypesM replace
  where
  replace (TypeWildcard ann wdata) = do
    t <- freshType
    ctx <- getLocalContext
    case wdata of
      HoleWildcard n -> tell (errorMessage (HoleInferredType n t ctx Nothing))
      UnnamedWildcard -> tell (errorMessage (WildcardInferredType t ctx))
      IgnoredWildcard -> pure unit
    pure t
  replace other = pure other

-- | Replace outermost unsolved unification variables with named type variables
varIfUnknown
  :: forall m
   . MonadState CheckState m
  => Array (Tuple Unknown SourceType)
  -> SourceType
  -> m SourceType
varIfUnknown unks ty = do
  bn' <- traverse toBinding unks
  ty' <- go ty
  pure (mkForAll bn' ty')
  where
  toName :: Unknown -> m String
  toName u = do
    n <- lookupUnkName u
    pure (fromMaybe "t" n <> show u)

  toBinding :: Tuple Unknown SourceType -> m (Tuple SourceAnn (Tuple String (Maybe SourceType)))
  toBinding (Tuple u k) = do
    u' <- toName u
    k' <- go k
    pure (Tuple (getAnnForType ty) (Tuple u' (Just k')))

  go :: SourceType -> m SourceType
  go = everywhereOnTypesM goInner

  goInner :: SourceType -> m SourceType
  goInner (TUnknown ann u) = TypeVar ann <$> toName u
  goInner t = pure t

void :: forall f a. Functor f => f a -> f Unit
void = map (const unit)
