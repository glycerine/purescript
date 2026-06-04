-- | The core functional representation.
module Language.PureScript.CoreFn.Expr
  ( Bind(..)
  , CaseAlternative(..)
  , Expr(..)
  , Guard
  , extractAnn
  , modifyAnn
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Literals (Literal)
import Language.PureScript.CoreFn.Binders (Binder)
import Language.PureScript.Names
  ( ConstructorName
  , Ident
  , ProperName
  , Qualified
  , TypeName
  )
import Language.PureScript.PSString (PSString)

data Expr a
  = Literal a (Literal (Expr a))
  | Constructor a (ProperName TypeName) (ProperName ConstructorName) (Array Ident)
  | Accessor a PSString (Expr a)
  | ObjectUpdate a (Expr a) (Maybe (Array PSString)) (Array (Tuple PSString (Expr a)))
  | Abs a Ident (Expr a)
  | App a (Expr a) (Expr a)
  | Var a (Qualified Ident)
  | Case a (Array (Expr a)) (Array (CaseAlternative a))
  | Let a (Array (Bind a)) (Expr a)

derive instance eqExpr :: Eq a => Eq (Expr a)
derive instance ordExpr :: Ord a => Ord (Expr a)
derive instance functorExpr :: Functor Expr

instance showExpr :: Show a => Show (Expr a) where
  show (Literal a _) = "(Literal " <> show a <> " ...)"
  show (Constructor a t c _) = "(Constructor " <> show a <> " " <> show t <> " " <> show c <> " ...)"
  show (Accessor a ps _) = "(Accessor " <> show a <> " ...)"
  show (ObjectUpdate a _ _ _) = "(ObjectUpdate " <> show a <> " ...)"
  show (Abs a i _) = "(Abs " <> show a <> " " <> show i <> " ...)"
  show (App a _ _) = "(App " <> show a <> " ...)"
  show (Var a q) = "(Var " <> show a <> " " <> show q <> ")"
  show (Case a _ _) = "(Case " <> show a <> " ...)"
  show (Let a _ _) = "(Let " <> show a <> " ...)"

data Bind a
  = NonRec a Ident (Expr a)
  | Rec (Array (Tuple (Tuple a Ident) (Expr a)))

derive instance eqBind :: Eq a => Eq (Bind a)
derive instance ordBind :: Ord a => Ord (Bind a)
derive instance functorBind :: Functor Bind

instance showBind :: Show a => Show (Bind a) where
  show (NonRec a i _) = "(NonRec " <> show a <> " " <> show i <> " ...)"
  show (Rec _) = "(Rec ...)"

type Guard a = Expr a

data CaseAlternative a = CaseAlternative
  { caseAlternativeBinders :: Array (Binder a)
  , caseAlternativeResult :: Either (Array (Tuple (Guard a) (Expr a))) (Expr a)
  }

derive instance eqCaseAlternative :: Eq a => Eq (CaseAlternative a)
derive instance ordCaseAlternative :: Ord a => Ord (CaseAlternative a)

instance showCaseAlternative :: Show a => Show (CaseAlternative a) where
  show (CaseAlternative c) = "(CaseAlternative ...)"

instance functorCaseAlternative :: Functor CaseAlternative where
  map f (CaseAlternative c) = CaseAlternative
    { caseAlternativeBinders: map (map f) c.caseAlternativeBinders
    , caseAlternativeResult: case c.caseAlternativeResult of
        Left guards -> Left (map (\(Tuple g e) -> Tuple (map f g) (map f e)) guards)
        Right e -> Right (map f e)
    }

extractAnn :: forall a. Expr a -> a
extractAnn (Literal a _) = a
extractAnn (Constructor a _ _ _) = a
extractAnn (Accessor a _ _) = a
extractAnn (ObjectUpdate a _ _ _) = a
extractAnn (Abs a _ _) = a
extractAnn (App a _ _) = a
extractAnn (Var a _) = a
extractAnn (Case a _ _) = a
extractAnn (Let a _ _) = a

modifyAnn :: forall a. (a -> a) -> Expr a -> Expr a
modifyAnn f (Literal a b) = Literal (f a) b
modifyAnn f (Constructor a b c d) = Constructor (f a) b c d
modifyAnn f (Accessor a b c) = Accessor (f a) b c
modifyAnn f (ObjectUpdate a b c d) = ObjectUpdate (f a) b c d
modifyAnn f (Abs a b c) = Abs (f a) b c
modifyAnn f (App a b c) = App (f a) b c
modifyAnn f (Var a b) = Var (f a) b
modifyAnn f (Case a b c) = Case (f a) b c
modifyAnn f (Let a b c) = Let (f a) b c
