-- | CoreFn traversal helpers.
module Language.PureScript.CoreFn.Traversals
  ( everywhereOnValues
  , traverseCoreFn
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))

type EverywhereResult a =
  { bind :: Bind a -> Bind a
  , expr :: Expr a -> Expr a
  , binder :: Binder a -> Binder a
  }

everywhereOnValues
  :: forall a
   . (Bind a -> Bind a)
  -> (Expr a -> Expr a)
  -> (Binder a -> Binder a)
  -> EverywhereResult a
everywhereOnValues f g h =
  { bind: f'
  , expr: g'
  , binder: h'
  }
  where
  f' (NonRec a name e) = f (NonRec a name (g' e))
  f' (Rec es) = f (Rec (map (\(Tuple ai e) -> Tuple ai (g' e)) es))

  g' (Literal ann e) = g (Literal ann (handleLiteral g' e))
  g' (Accessor ann prop e) = g (Accessor ann prop (g' e))
  g' (ObjectUpdate ann obj copy vs) = g (ObjectUpdate ann (g' obj) copy (map (\(Tuple k v) -> Tuple k (g' v)) vs))
  g' (Abs ann name e) = g (Abs ann name (g' e))
  g' (App ann v1 v2) = g (App ann (g' v1) (g' v2))
  g' (Case ann vs alts) = g (Case ann (map g' vs) (map handleCaseAlternative alts))
  g' (Let ann ds e) = g (Let ann (map f' ds) (g' e))
  g' e = g e

  h' (LiteralBinder a b) = h (LiteralBinder a (handleLiteral h' b))
  h' (NamedBinder a name b) = h (NamedBinder a name (h' b))
  h' (ConstructorBinder a q1 q2 bs) = h (ConstructorBinder a q1 q2 (map h' bs))
  h' b = h b

  handleCaseAlternative (CaseAlternative ca) = CaseAlternative
    { caseAlternativeBinders: map h' ca.caseAlternativeBinders
    , caseAlternativeResult: case ca.caseAlternativeResult of
        Left guards -> Left (map (\(Tuple g0 e) -> Tuple (g' g0) (g' e)) guards)
        Right e -> Right (g' e)
    }

  handleLiteral :: forall b. (b -> b) -> Literal b -> Literal b
  handleLiteral i (ArrayLiteral ls) = ArrayLiteral (map i ls)
  handleLiteral i (ObjectLiteral ls) = ObjectLiteral (map (\(Tuple k v) -> Tuple k (i v)) ls)
  handleLiteral _ other = other

type TraverseResult f a =
  { bind :: Bind a -> f (Bind a)
  , expr :: Expr a -> f (Expr a)
  , binder :: Binder a -> f (Binder a)
  , caseAlt :: CaseAlternative a -> f (CaseAlternative a)
  }

traverseCoreFn
  :: forall f a
   . Applicative f
  => (Bind a -> f (Bind a))
  -> (Expr a -> f (Expr a))
  -> (Binder a -> f (Binder a))
  -> (CaseAlternative a -> f (CaseAlternative a))
  -> TraverseResult f a
traverseCoreFn f g h i =
  { bind: f'
  , expr: g'
  , binder: h'
  , caseAlt: i'
  }
  where
  f' (NonRec a name e) = NonRec a name <$> g e
  f' (Rec es) = Rec <$> traverse (\(Tuple ai e) -> Tuple ai <$> g e) es

  g' (Literal ann e) = Literal ann <$> handleLiteral g e
  g' (Accessor ann prop e) = Accessor ann prop <$> g e
  g' (ObjectUpdate ann obj copy vs) =
    (\obj' vs' -> ObjectUpdate ann obj' copy vs') <$> g obj <*> traverse (\(Tuple k v) -> Tuple k <$> g v) vs
  g' (Abs ann name e) = Abs ann name <$> g e
  g' (App ann v1 v2) = App ann <$> g v1 <*> g v2
  g' (Case ann vs alts) = Case ann <$> traverse g vs <*> traverse i alts
  g' (Let ann ds e) = Let ann <$> traverse f ds <*> g e
  g' e = pure e

  h' (LiteralBinder a b) = LiteralBinder a <$> handleLiteral h b
  h' (NamedBinder a name b) = NamedBinder a name <$> h b
  h' (ConstructorBinder a q1 q2 bs) = ConstructorBinder a q1 q2 <$> traverse h bs
  h' b = pure b

  i' (CaseAlternative ca) =
    (\binders result -> CaseAlternative { caseAlternativeBinders: binders, caseAlternativeResult: result })
      <$> traverse h ca.caseAlternativeBinders
      <*> case ca.caseAlternativeResult of
            Left guards -> Left <$> traverse (\(Tuple g0 e) -> Tuple <$> g g0 <*> g e) guards
            Right e -> Right <$> g e

  handleLiteral :: forall b. (b -> f b) -> Literal b -> f (Literal b)
  handleLiteral withItem (ArrayLiteral ls) = ArrayLiteral <$> traverse withItem ls
  handleLiteral withItem (ObjectLiteral ls) = ObjectLiteral <$> traverse (\(Tuple k v) -> Tuple k <$> withItem v) ls
  handleLiteral _ other = pure other
