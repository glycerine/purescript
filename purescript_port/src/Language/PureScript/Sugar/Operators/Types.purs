module Language.PureScript.Sugar.Operators.Types
  ( matchTypeOperators
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Operators (Associativity)
import Data.Tuple (Tuple(..))
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Names (OpName, TypeOpName, Qualified)
import Language.PureScript.Sugar.Operators.Common (matchOperators)
import Language.PureScript.Types (SourceType, Type(..), srcTypeApp)

matchTypeOperators
  :: forall m
   . MonadError MultipleErrors m
  => SourceSpan
  -> Array (Array (Tuple (Qualified (OpName TypeOpName)) Associativity))
  -> SourceType
  -> m SourceType
matchTypeOperators ss = matchOperators isBinOp extractOp fromOp reapply identity
  where

  isBinOp :: SourceType -> Boolean
  isBinOp (BinaryNoParensType _ _ _ _) = true
  isBinOp _ = false

  extractOp :: SourceType -> Maybe (Tuple SourceType (Tuple SourceType SourceType))
  extractOp (BinaryNoParensType _ op l r) = Just (Tuple op (Tuple l r))
  extractOp _ = Nothing

  fromOp :: SourceType -> Maybe (Tuple SourceSpan (Qualified (OpName TypeOpName)))
  fromOp (TypeOp _ q) = Just (Tuple ss q)
  fromOp _ = Nothing

  reapply :: SourceSpan -> Qualified (OpName TypeOpName) -> SourceType -> SourceType -> SourceType
  reapply _ op l r = srcTypeApp (srcTypeApp (TypeOp (Tuple ss []) op) l) r
