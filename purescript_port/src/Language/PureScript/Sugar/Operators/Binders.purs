module Language.PureScript.Sugar.Operators.Binders
  ( matchBinderOperators
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Operators (Associativity)
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Names (OpName, ValueOpName, Qualified)
import Language.PureScript.Sugar.Operators.Common (matchOperators)

matchBinderOperators
  :: forall m
   . MonadError MultipleErrors m
  => Array (Array (Tuple (Qualified (OpName ValueOpName)) Associativity))
  -> Binder
  -> m Binder
matchBinderOperators = matchOperators isBinOp extractOp fromOp reapply identity
  where

  isBinOp :: Binder -> Boolean
  isBinOp (BinaryNoParensBinder _ _ _) = true
  isBinOp _ = false

  extractOp :: Binder -> Maybe (Tuple Binder (Tuple Binder Binder))
  extractOp (BinaryNoParensBinder op l r) = Just (Tuple op (Tuple l r))
  extractOp _ = Nothing

  fromOp :: Binder -> Maybe (Tuple SourceSpan (Qualified (OpName ValueOpName)))
  fromOp (OpBinder ss q) = Just (Tuple ss q)
  fromOp _ = Nothing

  reapply :: SourceSpan -> Qualified (OpName ValueOpName) -> Binder -> Binder -> Binder
  reapply ss op l r = BinaryNoParensBinder (OpBinder ss op) l r
