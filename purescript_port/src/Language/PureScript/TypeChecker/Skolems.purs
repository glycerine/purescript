module Language.PureScript.TypeChecker.Skolems
  ( newSkolemConstant
  , introduceSkolemScope
  , newSkolemScope
  , skolemize
  , skolemizeTypesInValue
  , skolemEscapeCheck
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State.Class (class MonadState, gets, modify_)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations
  ( ErrorMessageHint(..)
  , Expr(..)
  )
import Language.PureScript.AST.SourcePos (SourceAnn, SourceSpan, nonEmptySpan)
import Language.PureScript.AST.Traversals
  ( defS
  , everythingWithContextOnValues
  , everywhereWithContextOnValuesM
  )
import Language.PureScript.Errors
  ( ErrorMessage(..)
  , MultipleErrors
  , SimpleErrorMessage(..)
  , positionedError
  , singleError
  )
import Language.PureScript.TypeChecker.Monad (CheckState(..))
import Language.PureScript.Types
  ( SkolemScope(..)
  , SourceType
  , Type(..)
  , everythingOnTypes
  , everywhereOnTypesM
  , replaceTypeVars
  )

newSkolemConstant :: forall m. MonadState CheckState m => m Int
newSkolemConstant = do
  s <- gets \(CheckState cs) -> cs.checkNextSkolem
  modify_ \(CheckState cs) -> CheckState cs { checkNextSkolem = s + 1 }
  pure s

introduceSkolemScope :: forall m a. MonadState CheckState m => Type a -> m (Type a)
introduceSkolemScope = everywhereOnTypesM go
  where
  go (ForAll ann vis ident mbK ty Nothing) = ForAll ann vis ident mbK ty <<< Just <$> newSkolemScope
  go other = pure other

newSkolemScope :: forall m. MonadState CheckState m => m SkolemScope
newSkolemScope = do
  s <- gets \(CheckState cs) -> cs.checkNextSkolemScope
  modify_ \(CheckState cs) -> CheckState cs { checkNextSkolemScope = s + 1 }
  pure (SkolemScope s)

skolemize :: forall a. a -> String -> Maybe (Type a) -> Int -> SkolemScope -> Type a -> Type a
skolemize ann ident mbK sko scope = replaceTypeVars ident (Skolem ann ident mbK sko scope)

skolemizeTypesInValue :: SourceAnn -> String -> Maybe SourceType -> Int -> SkolemScope -> Expr -> Expr
skolemizeTypesInValue ann ident mbK sko scope expr =
  unwrap ((everywhereWithContextOnValuesM [] defS onExpr onBinder defS defS defS).expr expr)
  where
  onExpr :: Array String -> Expr -> Identity (Tuple (Array String) Expr)
  onExpr sco e@(DeferredDictionary c ts)
    | not (Array.elem ident sco) =
        pure (Tuple sco (DeferredDictionary c (map (skolemize ann ident mbK sko scope) ts)))
  onExpr sco e@(TypedValue check val ty)
    | not (Array.elem ident sco) =
        pure (Tuple (sco <> peelTypeVars ty) (TypedValue check val (skolemize ann ident mbK sko scope ty)))
  onExpr sco e@(VisibleTypeApp val ty)
    | not (Array.elem ident sco) =
        pure (Tuple (sco <> peelTypeVars ty) (VisibleTypeApp val (skolemize ann ident mbK sko scope ty)))
  onExpr sco other = pure (Tuple sco other)

  onBinder :: Array String -> Binder -> Identity (Tuple (Array String) Binder)
  onBinder sco (TypedBinder ty b)
    | not (Array.elem ident sco) =
        pure (Tuple (sco <> peelTypeVars ty) (TypedBinder (skolemize ann ident mbK sko scope ty) b))
  onBinder sco other = pure (Tuple sco other)

  peelTypeVars :: SourceType -> Array String
  peelTypeVars (ForAll _ _ i _ ty _) = Array.cons i (peelTypeVars ty)
  peelTypeVars _ = []

skolemEscapeCheck :: forall m. MonadError MultipleErrors m => Expr -> m Unit
skolemEscapeCheck (TypedValue false _ _) = pure unit
skolemEscapeCheck expr@(TypedValue _ _ _) =
  traverse_ (throwError <<< singleError)
    ((everythingWithContextOnValues (Tuple Set.empty Nothing) [] (<>) def go def def def).expr expr)
  where
  def :: forall t. Tuple (Set SkolemScope) (Maybe SourceSpan) -> t -> Tuple (Tuple (Set SkolemScope) (Maybe SourceSpan)) (Array ErrorMessage)
  def s _ = Tuple s []

  go :: Tuple (Set SkolemScope) (Maybe SourceSpan)
     -> Expr
     -> Tuple (Tuple (Set SkolemScope) (Maybe SourceSpan)) (Array ErrorMessage)
  go (Tuple scopes _) (PositionedValue ss _ _) = Tuple (Tuple scopes (Just ss)) []
  go (Tuple scopes ssUsed) val@(TypedValue _ _ ty) =
    let newScopes = collectScopes ty
        allScopes = Set.union (Set.fromFoldable newScopes) scopes
        errs = Array.mapMaybe (\(Tuple3 ssBound name scope') ->
          if Set.member scope' allScopes then Nothing
          else Just (ErrorMessage
            (case ssUsed of
              Nothing -> [ErrorInExpression val]
              Just ss -> Array.cons (positionedError ss) [ErrorInExpression val])
            (EscapedSkolem name (nonEmptySpan ssBound) ty)))
          (collectSkolems ty)
    in Tuple (Tuple allScopes ssUsed) errs
  go scos _ = Tuple scos []

  collectScopes :: SourceType -> Array SkolemScope
  collectScopes (ForAll _ _ _ _ t (Just sco)) = Array.cons sco (collectScopes t)
  collectScopes _ = []

  collectSkolems :: SourceType -> Array (Tuple3 SourceAnn String SkolemScope)
  collectSkolems = everythingOnTypes (<>) collect
    where
    collect (Skolem ss name _ _ scope') = [Tuple3 ss name scope']
    collect _ = []
skolemEscapeCheck _ = pure unit

data Tuple3 a b c = Tuple3 a b c
