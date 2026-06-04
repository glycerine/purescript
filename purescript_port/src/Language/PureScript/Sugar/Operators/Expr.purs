module Language.PureScript.Sugar.Operators.Expr
  ( matchExprOperators
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Parsing (Parser)
import Parsing.Combinators (try, (<?>))
import Parsing.Expr (Operator(..), Assoc(..)) as PE

import Language.PureScript.AST.Declarations (Expr(..))
import Language.PureScript.AST.Operators (Associativity)
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Names (OpName, ValueOpName, Qualified(..))
import Language.PureScript.Sugar.Operators.Common (Chain, matchOperators, token)

matchExprOperators
  :: forall m
   . MonadError MultipleErrors m
  => Array (Array (Tuple (Qualified (OpName ValueOpName)) Associativity))
  -> Expr
  -> m Expr
matchExprOperators = matchOperators isBinOp extractOp fromOp reapply modOpTable
  where

  isBinOp :: Expr -> Boolean
  isBinOp (BinaryNoParens _ _ _) = true
  isBinOp _ = false

  extractOp :: Expr -> Maybe (Tuple Expr (Tuple Expr Expr))
  extractOp (BinaryNoParens op l r) =
    case op of
      PositionedValue _ _ op' -> Just (Tuple op' (Tuple l r))
      _ -> Just (Tuple op (Tuple l r))
  extractOp _ = Nothing

  fromOp :: Expr -> Maybe (Tuple SourceSpan (Qualified (OpName ValueOpName)))
  fromOp (Op ss q) = Just (Tuple ss q)
  fromOp _ = Nothing

  reapply :: SourceSpan -> Qualified (OpName ValueOpName) -> Expr -> Expr -> Expr
  reapply ss op l r = BinaryNoParens (Op ss op) l r

  modOpTable
    :: Array (Array (PE.Operator Identity (Chain Expr) Expr))
    -> Array (Array (PE.Operator Identity (Chain Expr) Expr))
  modOpTable table =
    [ [ PE.Infix (try (BinaryNoParens <$> parseTicks)) PE.AssocLeft ] ]
    <> table

  parseTicks :: Parser (Chain Expr) Expr
  parseTicks = token (either (const Nothing) fromOther) <?> "infix function"
    where
    fromOther (Op _ _) = Nothing
    fromOther v = Just v
