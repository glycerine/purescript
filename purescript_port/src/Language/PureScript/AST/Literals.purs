module Language.PureScript.AST.Literals
  ( Literal(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Language.PureScript.PSString (PSString)

-- | Literal values parameterised over the expression type (used for both Exprs and Binders).
data Literal a
  = NumericLiteral (Either Int Number)
  | StringLiteral PSString
  | CharLiteral Char
  | BooleanLiteral Boolean
  | ArrayLiteral (Array a)
  | ObjectLiteral (Array (Tuple PSString a))

derive instance eqLiteral :: Eq a => Eq (Literal a)
derive instance ordLiteral :: Ord a => Ord (Literal a)
derive instance functorLiteral :: Functor Literal

instance showLiteral :: Show a => Show (Literal a) where
  show (NumericLiteral (Left i))  = "(NumericLiteral (Left " <> show i <> "))"
  show (NumericLiteral (Right n)) = "(NumericLiteral (Right " <> show n <> "))"
  show (StringLiteral s)          = "(StringLiteral " <> show s <> ")"
  show (CharLiteral c)            = "(CharLiteral " <> show c <> ")"
  show (BooleanLiteral b)         = "(BooleanLiteral " <> show b <> ")"
  show (ArrayLiteral xs)          = "(ArrayLiteral " <> show xs <> ")"
  show (ObjectLiteral kvs)        = "(ObjectLiteral " <> show kvs <> ")"
