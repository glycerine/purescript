-- | The core functional representation for binders.
module Language.PureScript.CoreFn.Binders
  ( Binder(..)
  , extractBinderAnn
  ) where

import Prelude

import Language.PureScript.AST.Literals (Literal)
import Language.PureScript.Names
  ( ConstructorName
  , Ident
  , ProperName
  , Qualified
  , TypeName
  )

data Binder a
  = NullBinder a
  | LiteralBinder a (Literal (Binder a))
  | VarBinder a Ident
  | ConstructorBinder a (Qualified (ProperName TypeName)) (Qualified (ProperName ConstructorName)) (Array (Binder a))
  | NamedBinder a Ident (Binder a)

derive instance eqBinder :: Eq a => Eq (Binder a)
derive instance ordBinder :: Ord a => Ord (Binder a)
derive instance functorBinder :: Functor Binder

instance showBinder :: Show a => Show (Binder a) where
  show (NullBinder a) = "(NullBinder " <> show a <> ")"
  show (LiteralBinder a _) = "(LiteralBinder " <> show a <> " ...)"
  show (VarBinder a i) = "(VarBinder " <> show a <> " " <> show i <> ")"
  show (ConstructorBinder a t c _) = "(ConstructorBinder " <> show a <> " " <> show t <> " " <> show c <> " ...)"
  show (NamedBinder a i _) = "(NamedBinder " <> show a <> " " <> show i <> " ...)"

extractBinderAnn :: forall a. Binder a -> a
extractBinderAnn (NullBinder a) = a
extractBinderAnn (LiteralBinder a _) = a
extractBinderAnn (VarBinder a _) = a
extractBinderAnn (ConstructorBinder a _ _ _) = a
extractBinderAnn (NamedBinder a _ _) = a
