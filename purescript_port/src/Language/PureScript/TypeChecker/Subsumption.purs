-- | Subsumption checking
module Language.PureScript.TypeChecker.Subsumption
  ( subsumes
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State.Class (class MonadState)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Declarations (ErrorMessageHint(..), Expr(..))
import Language.PureScript.AST.SourcePos (nullSourceAnn)
import Language.PureScript.Environment (tyFunction, tyRecord)
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , errorMessage
  , internalCompilerError
  )
import Language.PureScript.Label (Label(..))
import Language.PureScript.TypeChecker.Monad
  ( CheckState
  , getHints
  , getTypeClassDictionaries
  , withErrorMessageHint
  )
import Language.PureScript.TypeChecker.Skolems (newSkolemConstant, skolemize)
import Language.PureScript.TypeChecker.Unify (freshTypeWithKind, unifyTypes)
import Language.PureScript.Types
  ( RowListItem(..)
  , SourceType
  , Type(..)
  , alignRowsWith
  , isREmpty
  , replaceTypeVars
  , rowFromList
  )

-- | Check that one type subsumes another, rethrowing errors to provide a better error message.
-- | Returns a coercion function (identity when no elaboration is needed).
subsumes
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m (Expr -> Expr)
subsumes ty1 ty2 =
  withErrorMessageHint (ErrorInSubsumption ty1 ty2) $
    subsumes' true ty1 ty2

-- | Check that one type subsumes another.
-- | The Bool flag indicates whether we're in elaboration mode (true) or no-elaboration mode (false).
subsumes'
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Boolean
  -> SourceType
  -> SourceType
  -> m (Expr -> Expr)
subsumes' mode (ForAll _ _ ident mbK ty1 _) ty2 = do
  u <- case mbK of
    Just k -> freshTypeWithKind k
    Nothing -> internalCompilerError "subsumes: unelaborated forall"
  let replaced = replaceTypeVars ident u ty1
  subsumes' mode replaced ty2
subsumes' mode ty1 (ForAll _ _ ident mbK ty2 sco) =
  case sco of
    Just sco' -> do
      sko <- newSkolemConstant
      let sk = skolemize nullSourceAnn ident mbK sko sco' ty2
      subsumes' mode ty1 sk
    Nothing -> internalCompilerError "subsumes: unspecified skolem scope"
subsumes' mode (TypeApp _ (TypeApp _ f1 arg1) ret1) (TypeApp _ (TypeApp _ f2 arg2) ret2)
  | f1 == tyFunction && f2 == tyFunction = do
      _ <- subsumes' false arg2 arg1
      _ <- subsumes' false ret1 ret2
      pure identity
subsumes' mode (KindedType _ ty1 _) ty2 =
  subsumes' mode ty1 ty2
subsumes' mode ty1 (KindedType _ ty2 _) =
  subsumes' mode ty1 ty2
subsumes' true (ConstrainedType _ con ty1) ty2 = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  elaborate <- subsumes' true ty1 ty2
  let addDicts val = App val (TypeClassDictionary con dicts hints)
  pure (elaborate <<< addDicts)
subsumes' mode (TypeApp _ f1 r1) (TypeApp _ f2 r2)
  | f1 == tyRecord && f2 == tyRecord = do
      let goWithLabel l t1 t2 = withErrorMessageHint (ErrorInRowLabel l) $ subsumes' false t1 t2
      let Tuple common (Tuple (Tuple ts1' r1') (Tuple ts2' r2')) = alignRowsWith goWithLabel r1 r2
      when (isREmpty r1') do
        case firstMissingProp ts2' ts1' of
          Just lbl -> throwError (errorMessage (PropertyIsMissing lbl))
          Nothing -> pure unit
      when (isREmpty r2') do
        case firstMissingProp ts1' ts2' of
          Just lbl -> throwError (errorMessage (AdditionalProperty lbl))
          Nothing -> pure unit
      sequence_ common
      unifyTypes (rowFromList (Tuple ts1' r1')) (rowFromList (Tuple ts2' r2'))
      pure identity
  where
  firstMissingProp
    :: Array (RowListItem _)
    -> Array (RowListItem _)
    -> Maybe Label
  firstMissingProp t1 t2 =
    let labels2 = map (\(RowListItem item) -> item.rowListLabel) t2
        missing = Array.filter (\(RowListItem item) -> not (Array.elem item.rowListLabel labels2)) t1
    in map (\(RowListItem item) -> item.rowListLabel) (Array.head missing)
subsumes' mode ty1 ty2@(TypeApp _ obj _)
  | obj == tyRecord =
      subsumes' mode ty2 ty1
subsumes' _mode ty1 ty2 = do
  unifyTypes ty1 ty2
  pure identity
