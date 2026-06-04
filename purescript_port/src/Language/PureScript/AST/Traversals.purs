module Language.PureScript.AST.Traversals
  ( guardedExprM
  , mapGuardedExpr
  , litM
  , everywhereOnValues
  , everywhereOnValuesTopDownM
  , everywhereOnValuesM
  , everythingOnValues
  , everythingWithContextOnValues
  , everywhereWithContextOnValues
  , everywhereWithContextOnValuesM
  , ScopedIdent(..)
  , inScope
  , everythingWithScope
  , accumTypes
  , overTypes
  , defS
  , sndM
  ) where

import Prelude

import Control.Monad.State.Trans (StateT(..), runStateT)
import Data.Array as Array
import Data.Array (mapMaybe)
import Data.Foldable (fold, foldMap, foldl)
import Data.Identity (Identity(..))
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst)

import Language.PureScript.AST.Binders (Binder(..), binderNames)
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , DataConstructorDeclaration(..)
  , Declaration(..)
  , DoNotationElement(..)
  , Expr(..)
  , Guard(..)
  , GuardedExpr(..)
  , TypeDeclarationData(..)
  , TypeInstanceBody(..)
  , ValueDeclarationData(..)
  , mapTypeInstanceBody
  , traverseTypeInstanceBody
  )
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.Names (ClassName, Ident, ProperName, Qualified, QualifiedBy(..))
import Language.PureScript.AST.SourcePos (SourcePos(..))
import Language.PureScript.Types (Constraint(..), SourceConstraint, SourceType, mapConstraintArgs)
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope(..))

-- | Unwrap Identity
runIdentity :: forall a. Identity a -> a
runIdentity (Identity a) = a

-- -----------------------------------------------------------------------
-- Return-type records for traversal combinators
-- -----------------------------------------------------------------------

type EverywhereResult =
  { decl   :: Declaration -> Declaration
  , expr   :: Expr -> Expr
  , binder :: Binder -> Binder
  }

type EverywhereResultM m =
  { decl   :: Declaration -> m Declaration
  , expr   :: Expr -> m Expr
  , binder :: Binder -> m Binder
  }

type EverythingResult r =
  { decl       :: Declaration -> r
  , expr       :: Expr -> r
  , binder     :: Binder -> r
  , alt        :: CaseAlternative -> r
  , doNotation :: DoNotationElement -> r
  }

type WithContextResult =
  { decl       :: Declaration -> Declaration
  , expr       :: Expr -> Expr
  , binder     :: Binder -> Binder
  , alt        :: CaseAlternative -> CaseAlternative
  , doNotation :: DoNotationElement -> DoNotationElement
  , guard      :: Guard -> Guard
  }

type WithContextResultM m =
  { decl       :: Declaration -> m Declaration
  , expr       :: Expr -> m Expr
  , binder     :: Binder -> m Binder
  , alt        :: CaseAlternative -> m CaseAlternative
  , doNotation :: DoNotationElement -> m DoNotationElement
  , guard      :: Guard -> m Guard
  }

type WithScopeResult r =
  { decl       :: Set ScopedIdent -> Declaration -> r
  , expr       :: Set ScopedIdent -> Expr -> r
  , binder     :: Set ScopedIdent -> Binder -> r
  , alt        :: Set ScopedIdent -> CaseAlternative -> r
  , doNotation :: Set ScopedIdent -> DoNotationElement -> r
  }

-- -----------------------------------------------------------------------
-- Utilities
-- -----------------------------------------------------------------------

guardedExprM
  :: forall m. Applicative m
  => (Guard -> m Guard)
  -> (Expr -> m Expr)
  -> GuardedExpr
  -> m GuardedExpr
guardedExprM f g (GuardedExpr guards rhs) =
  GuardedExpr <$> traverse f guards <*> g rhs

mapGuardedExpr :: (Guard -> Guard) -> (Expr -> Expr) -> GuardedExpr -> GuardedExpr
mapGuardedExpr f g (GuardedExpr guards rhs) =
  GuardedExpr (map f guards) (g rhs)

litM :: forall m a. Monad m => (a -> m a) -> Literal a -> m (Literal a)
litM go (ObjectLiteral as) = ObjectLiteral <$> traverse (\(Tuple k v) -> Tuple k <$> go v) as
litM go (ArrayLiteral as)  = ArrayLiteral <$> traverse go as
litM _  other              = pure other

-- -----------------------------------------------------------------------
-- everywhereOnValues — bottom-up pure traversal
-- -----------------------------------------------------------------------

everywhereOnValues
  :: (Declaration -> Declaration)
  -> (Expr -> Expr)
  -> (Binder -> Binder)
  -> EverywhereResult
everywhereOnValues f g h = { decl: f', expr: g', binder: h' }
  where
  f' :: Declaration -> Declaration
  f' (DataBindingGroupDeclaration ds) =
    f (DataBindingGroupDeclaration (map f' ds))
  f' (ValueDeclaration (ValueDeclarationData vd)) =
    f (ValueDeclaration (ValueDeclarationData vd
      { valdeclBinders    = map h' vd.valdeclBinders
      , valdeclExpression = map (mapGuardedExpr handleGuard g') vd.valdeclExpression
      }))
  f' (BoundValueDeclaration sa b expr) =
    f (BoundValueDeclaration sa (h' b) (g' expr))
  f' (BindingGroupDeclaration ds) =
    f (BindingGroupDeclaration (map (\(Tuple nameAnn (Tuple nameKind val)) ->
      Tuple nameAnn (Tuple nameKind (g' val))) ds))
  f' (TypeClassDeclaration sa name args implies deps ds) =
    f (TypeClassDeclaration sa name args implies deps (map f' ds))
  f' (TypeInstanceDeclaration sa na ch idx name cs className args ds) =
    f (TypeInstanceDeclaration sa na ch idx name cs className args (mapTypeInstanceBody (map f') ds))
  f' other = f other

  g' :: Expr -> Expr
  g' (Literal ss l)            = g (Literal ss (lit g' l))
  g' (UnaryMinus ss v)         = g (UnaryMinus ss (g' v))
  g' (BinaryNoParens op v1 v2) = g (BinaryNoParens (g' op) (g' v1) (g' v2))
  g' (Parens v)                = g (Parens (g' v))
  g' (Accessor prop v)         = g (Accessor prop (g' v))
  g' (ObjectUpdate obj vs)     = g (ObjectUpdate (g' obj) (map (\(Tuple k v) -> Tuple k (g' v)) vs))
  g' (ObjectUpdateNested obj vs) = g (ObjectUpdateNested (g' obj) (map g' vs))
  g' (Abs binder v)            = g (Abs (h' binder) (g' v))
  g' (App v1 v2)               = g (App (g' v1) (g' v2))
  g' (VisibleTypeApp v ty)     = g (VisibleTypeApp (g' v) ty)
  g' (Unused v)                = g (Unused (g' v))
  g' (IfThenElse v1 v2 v3)    = g (IfThenElse (g' v1) (g' v2) (g' v3))
  g' (Case vs alts)            = g (Case (map g' vs) (map handleCaseAlternative alts))
  g' (TypedValue check v ty)   = g (TypedValue check (g' v) ty)
  g' (Let w ds v)              = g (Let w (map f' ds) (g' v))
  g' (Do m es)                 = g (Do m (map handleDoNotationElement es))
  g' (Ado m es v)              = g (Ado m (map handleDoNotationElement es) (g' v))
  g' (PositionedValue pos com v) = g (PositionedValue pos com (g' v))
  g' other                     = g other

  h' :: Binder -> Binder
  h' (ConstructorBinder ss ctor bs) = h (ConstructorBinder ss ctor (map h' bs))
  h' (BinaryNoParensBinder b1 b2 b3) = h (BinaryNoParensBinder (h' b1) (h' b2) (h' b3))
  h' (ParensInBinder b)          = h (ParensInBinder (h' b))
  h' (LiteralBinder ss l)        = h (LiteralBinder ss (lit h' l))
  h' (NamedBinder ss name b)     = h (NamedBinder ss name (h' b))
  h' (PositionedBinder pos com b) = h (PositionedBinder pos com (h' b))
  h' (TypedBinder t b)           = h (TypedBinder t (h' b))
  h' other                       = h other

  lit :: forall a. (a -> a) -> Literal a -> Literal a
  lit go (ArrayLiteral as)  = ArrayLiteral (map go as)
  lit go (ObjectLiteral as) = ObjectLiteral (map (\(Tuple k v) -> Tuple k (go v)) as)
  lit _  other              = other

  handleCaseAlternative :: CaseAlternative -> CaseAlternative
  handleCaseAlternative (CaseAlternative ca) = CaseAlternative ca
    { caseAlternativeBinders = map h' ca.caseAlternativeBinders
    , caseAlternativeResult  = map (mapGuardedExpr handleGuard g') ca.caseAlternativeResult
    }

  handleDoNotationElement :: DoNotationElement -> DoNotationElement
  handleDoNotationElement (DoNotationValue v) = DoNotationValue (g' v)
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind (h' b) (g' v)
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet (map f' ds)
  handleDoNotationElement (PositionedDoNotationElement pos com e) =
    PositionedDoNotationElement pos com (handleDoNotationElement e)

  handleGuard :: Guard -> Guard
  handleGuard (ConditionGuard e) = ConditionGuard (g' e)
  handleGuard (PatternGuard b e) = PatternGuard (h' b) (g' e)

-- -----------------------------------------------------------------------
-- everywhereOnValuesTopDownM — top-down monadic traversal
-- -----------------------------------------------------------------------

everywhereOnValuesTopDownM
  :: forall m. Monad m
  => (Declaration -> m Declaration)
  -> (Expr -> m Expr)
  -> (Binder -> m Binder)
  -> EverywhereResultM m
everywhereOnValuesTopDownM f g h =
  { decl: \d -> f d >>= f', expr: \e -> g e >>= g', binder: \b -> h b >>= h' }
  where
  f' :: Declaration -> m Declaration
  f' (DataBindingGroupDeclaration ds) =
    DataBindingGroupDeclaration <$> traverse (\d -> f d >>= f') ds
  f' (ValueDeclaration (ValueDeclarationData vd)) = do
    bs  <- traverse (\b -> h b >>= h') vd.valdeclBinders
    val <- traverse (guardedExprM handleGuard (\e -> g e >>= g')) vd.valdeclExpression
    f (ValueDeclaration (ValueDeclarationData vd { valdeclBinders = bs, valdeclExpression = val }))
  f' (BindingGroupDeclaration ds) =
    BindingGroupDeclaration <$>
      traverse (\(Tuple nameAnn (Tuple nameKind val)) ->
        (\v -> Tuple nameAnn (Tuple nameKind v)) <$> (g val >>= g')) ds
  f' (TypeClassDeclaration sa name args implies deps ds) =
    TypeClassDeclaration sa name args implies deps <$> traverse (\d -> f d >>= f') ds
  f' (TypeInstanceDeclaration sa na ch idx name cs className args ds) =
    TypeInstanceDeclaration sa na ch idx name cs className args <$>
      traverseTypeInstanceBody (traverse (\d -> f d >>= f')) ds
  f' (BoundValueDeclaration sa b expr) =
    BoundValueDeclaration sa <$> (h b >>= h') <*> (g expr >>= g')
  f' other = f other

  g' :: Expr -> m Expr
  g' (Literal ss l)          = Literal ss <$> litM (\e -> g e >>= g') l
  g' (UnaryMinus ss v)       = UnaryMinus ss <$> (g v >>= g')
  g' (BinaryNoParens op v1 v2) =
    BinaryNoParens <$> (g op >>= g') <*> (g v1 >>= g') <*> (g v2 >>= g')
  g' (Parens v)              = Parens <$> (g v >>= g')
  g' (Accessor prop v)       = Accessor prop <$> (g v >>= g')
  g' (ObjectUpdate obj vs)   =
    ObjectUpdate <$> (g obj >>= g') <*>
      traverse (\(Tuple k v) -> Tuple k <$> (g v >>= g')) vs
  g' (ObjectUpdateNested obj vs) =
    ObjectUpdateNested <$> (g obj >>= g') <*> traverse (\e -> g e >>= g') vs
  g' (Abs binder v)          = Abs <$> (h binder >>= h') <*> (g v >>= g')
  g' (App v1 v2)             = App <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (VisibleTypeApp v ty)   = VisibleTypeApp <$> (g v >>= g') <*> pure ty
  g' (Unused v)              = Unused <$> (g v >>= g')
  g' (IfThenElse v1 v2 v3)  =
    IfThenElse <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g')
  g' (Case vs alts)          =
    Case <$> traverse (\e -> g e >>= g') vs <*> traverse handleCaseAlternative alts
  g' (TypedValue check v ty) = TypedValue check <$> (g v >>= g') <*> pure ty
  g' (Let w ds v)            = Let w <$> traverse (\d -> f d >>= f') ds <*> (g v >>= g')
  g' (Do m es)               = Do m <$> traverse handleDoNotationElement es
  g' (Ado m es v)            = Ado m <$> traverse handleDoNotationElement es <*> (g v >>= g')
  g' (PositionedValue pos com v) = PositionedValue pos com <$> (g v >>= g')
  g' other                   = g other

  h' :: Binder -> m Binder
  h' (LiteralBinder ss l)    = LiteralBinder ss <$> litM (\b -> h b >>= h') l
  h' (ConstructorBinder ss ctor bs) =
    ConstructorBinder ss ctor <$> traverse (\b -> h b >>= h') bs
  h' (BinaryNoParensBinder b1 b2 b3) =
    BinaryNoParensBinder <$> (h b1 >>= h') <*> (h b2 >>= h') <*> (h b3 >>= h')
  h' (ParensInBinder b)      = ParensInBinder <$> (h b >>= h')
  h' (NamedBinder ss name b) = NamedBinder ss name <$> (h b >>= h')
  h' (PositionedBinder pos com b) = PositionedBinder pos com <$> (h b >>= h')
  h' (TypedBinder t b)       = TypedBinder t <$> (h b >>= h')
  h' other                   = h other

  handleCaseAlternative :: CaseAlternative -> m CaseAlternative
  handleCaseAlternative (CaseAlternative ca) = do
    bs  <- traverse (\b -> h b >>= h') ca.caseAlternativeBinders
    val <- traverse (guardedExprM handleGuard (\e -> g e >>= g')) ca.caseAlternativeResult
    pure (CaseAlternative ca { caseAlternativeBinders = bs, caseAlternativeResult = val })

  handleDoNotationElement :: DoNotationElement -> m DoNotationElement
  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> (g v >>= g')
  handleDoNotationElement (DoNotationBind b v) =
    DoNotationBind <$> (h b >>= h') <*> (g v >>= g')
  handleDoNotationElement (DoNotationLet ds) =
    DoNotationLet <$> traverse (\d -> f d >>= f') ds
  handleDoNotationElement (PositionedDoNotationElement pos com e) =
    PositionedDoNotationElement pos com <$> handleDoNotationElement e

  handleGuard :: Guard -> m Guard
  handleGuard (ConditionGuard e) = ConditionGuard <$> (g e >>= g')
  handleGuard (PatternGuard b e) = PatternGuard <$> (h b >>= h') <*> (g e >>= g')

-- -----------------------------------------------------------------------
-- everywhereOnValuesM — bottom-up monadic traversal
-- -----------------------------------------------------------------------

everywhereOnValuesM
  :: forall m. Monad m
  => (Declaration -> m Declaration)
  -> (Expr -> m Expr)
  -> (Binder -> m Binder)
  -> EverywhereResultM m
everywhereOnValuesM f g h = { decl: f', expr: g', binder: h' }
  where
  f' :: Declaration -> m Declaration
  f' (DataBindingGroupDeclaration ds) =
    (DataBindingGroupDeclaration <$> traverse f' ds) >>= f
  f' (ValueDeclaration (ValueDeclarationData vd)) = do
    bs  <- traverse h' vd.valdeclBinders
    val <- traverse (guardedExprM handleGuard g') vd.valdeclExpression
    f (ValueDeclaration (ValueDeclarationData vd { valdeclBinders = bs, valdeclExpression = val }))
  f' (BindingGroupDeclaration ds) =
    (BindingGroupDeclaration <$>
      traverse (\(Tuple nameAnn (Tuple nameKind val)) ->
        (\v -> Tuple nameAnn (Tuple nameKind v)) <$> g' val) ds) >>= f
  f' (BoundValueDeclaration sa b expr) =
    (BoundValueDeclaration sa <$> h' b <*> g' expr) >>= f
  f' (TypeClassDeclaration sa name args implies deps ds) =
    (TypeClassDeclaration sa name args implies deps <$> traverse f' ds) >>= f
  f' (TypeInstanceDeclaration sa na ch idx name cs className args ds) =
    (TypeInstanceDeclaration sa na ch idx name cs className args <$>
      traverseTypeInstanceBody (traverse f') ds) >>= f
  f' other = f other

  g' :: Expr -> m Expr
  g' (Literal ss l)          = (Literal ss <$> litM g' l) >>= g
  g' (UnaryMinus ss v)       = (UnaryMinus ss <$> g' v) >>= g
  g' (BinaryNoParens op v1 v2) =
    (BinaryNoParens <$> g' op <*> g' v1 <*> g' v2) >>= g
  g' (Parens v)              = (Parens <$> g' v) >>= g
  g' (Accessor prop v)       = (Accessor prop <$> g' v) >>= g
  g' (ObjectUpdate obj vs)   =
    (ObjectUpdate <$> g' obj <*> traverse (\(Tuple k v) -> Tuple k <$> g' v) vs) >>= g
  g' (ObjectUpdateNested obj vs) =
    (ObjectUpdateNested <$> g' obj <*> traverse g' vs) >>= g
  g' (Abs binder v)          = (Abs <$> h' binder <*> g' v) >>= g
  g' (App v1 v2)             = (App <$> g' v1 <*> g' v2) >>= g
  g' (VisibleTypeApp v ty)   = (VisibleTypeApp <$> g' v <*> pure ty) >>= g
  g' (Unused v)              = (Unused <$> g' v) >>= g
  g' (IfThenElse v1 v2 v3)  =
    (IfThenElse <$> g' v1 <*> g' v2 <*> g' v3) >>= g
  g' (Case vs alts)          =
    (Case <$> traverse g' vs <*> traverse handleCaseAlternative alts) >>= g
  g' (TypedValue check v ty) = (TypedValue check <$> g' v <*> pure ty) >>= g
  g' (Let w ds v)            = (Let w <$> traverse f' ds <*> g' v) >>= g
  g' (Do m es)               = (Do m <$> traverse handleDoNotationElement es) >>= g
  g' (Ado m es v)            =
    (Ado m <$> traverse handleDoNotationElement es <*> g' v) >>= g
  g' (PositionedValue pos com v) = (PositionedValue pos com <$> g' v) >>= g
  g' other                   = g other

  h' :: Binder -> m Binder
  h' (LiteralBinder ss l)    = (LiteralBinder ss <$> litM h' l) >>= h
  h' (ConstructorBinder ss ctor bs) =
    (ConstructorBinder ss ctor <$> traverse h' bs) >>= h
  h' (BinaryNoParensBinder b1 b2 b3) =
    (BinaryNoParensBinder <$> h' b1 <*> h' b2 <*> h' b3) >>= h
  h' (ParensInBinder b)      = (ParensInBinder <$> h' b) >>= h
  h' (NamedBinder ss name b) = (NamedBinder ss name <$> h' b) >>= h
  h' (PositionedBinder pos com b) = (PositionedBinder pos com <$> h' b) >>= h
  h' (TypedBinder t b)       = (TypedBinder t <$> h' b) >>= h
  h' other                   = h other

  handleCaseAlternative :: CaseAlternative -> m CaseAlternative
  handleCaseAlternative (CaseAlternative ca) = do
    bs  <- traverse h' ca.caseAlternativeBinders
    val <- traverse (guardedExprM handleGuard g') ca.caseAlternativeResult
    pure (CaseAlternative ca { caseAlternativeBinders = bs, caseAlternativeResult = val })

  handleDoNotationElement :: DoNotationElement -> m DoNotationElement
  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> g' v
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> h' b <*> g' v
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> traverse f' ds
  handleDoNotationElement (PositionedDoNotationElement pos com e) =
    PositionedDoNotationElement pos com <$> handleDoNotationElement e

  handleGuard :: Guard -> m Guard
  handleGuard (ConditionGuard e) = ConditionGuard <$> g' e
  handleGuard (PatternGuard b e) = PatternGuard <$> h' b <*> g' e

-- -----------------------------------------------------------------------
-- everythingOnValues — collecting bottom-up
-- -----------------------------------------------------------------------

everythingOnValues
  :: forall r
   . (r -> r -> r)
  -> (Declaration -> r)
  -> (Expr -> r)
  -> (Binder -> r)
  -> (CaseAlternative -> r)
  -> (DoNotationElement -> r)
  -> EverythingResult r
everythingOnValues combine f g h i j =
  { decl: f', expr: g', binder: h', alt: i', doNotation: j' }
  where
  f' :: Declaration -> r
  f' d@(DataBindingGroupDeclaration ds) =
    foldl combine (f d) (map f' (Array.fromFoldable ds))
  f' d@(ValueDeclaration (ValueDeclarationData vd)) =
    foldl combine (f d)
      ( map h' vd.valdeclBinders <>
        Array.concatMap (\(GuardedExpr grd v) -> map k' grd <> [g' v]) vd.valdeclExpression
      )
  f' d@(BindingGroupDeclaration ds) =
    foldl combine (f d) (map (\(Tuple _ (Tuple _ val)) -> g' val) (Array.fromFoldable ds))
  f' d@(TypeClassDeclaration _ _ _ _ _ ds) =
    foldl combine (f d) (map f' ds)
  f' d@(TypeInstanceDeclaration _ _ _ _ _ _ _ _ (ExplicitInstance ds)) =
    foldl combine (f d) (map f' ds)
  f' d@(BoundValueDeclaration _ b expr) = combine (combine (f d) (h' b)) (g' expr)
  f' d = f d

  g' :: Expr -> r
  g' v@(Literal _ l)          = lit (g v) g' l
  g' v@(UnaryMinus _ v1)      = combine (g v) (g' v1)
  g' v@(BinaryNoParens op v1 v2) = combine (combine (combine (g v) (g' op)) (g' v1)) (g' v2)
  g' v@(Parens v1)            = combine (g v) (g' v1)
  g' v@(Accessor _ v1)        = combine (g v) (g' v1)
  g' v@(ObjectUpdate obj vs)  =
    foldl combine (combine (g v) (g' obj)) (map (\(Tuple _ v2) -> g' v2) vs)
  g' v@(ObjectUpdateNested obj vs) =
    foldl combine (combine (g v) (g' obj)) (map g' vs)
  g' v@(Abs b v1)             = combine (combine (g v) (h' b)) (g' v1)
  g' v@(App v1 v2)            = combine (combine (g v) (g' v1)) (g' v2)
  g' v@(VisibleTypeApp v' _)  = combine (g v) (g' v')
  g' v@(Unused v1)            = combine (g v) (g' v1)
  g' v@(IfThenElse v1 v2 v3) =
    combine (combine (combine (g v) (g' v1)) (g' v2)) (g' v3)
  g' v@(Case vs alts)         =
    foldl combine (foldl combine (g v) (map g' vs)) (map i' alts)
  g' v@(TypedValue _ v1 _)    = combine (g v) (g' v1)
  g' v@(Let _ ds v1)          = foldl combine (g v) (map f' ds) `combine` g' v1
  g' v@(Do _ es)              = foldl combine (g v) (map j' es)
  g' v@(Ado _ es v1)          = foldl combine (g v) (map j' es) `combine` g' v1
  g' v@(PositionedValue _ _ v1) = combine (g v) (g' v1)
  g' v                        = g v

  h' :: Binder -> r
  h' b@(LiteralBinder _ l)   = lit (h b) h' l
  h' b@(ConstructorBinder _ _ bs) = foldl combine (h b) (map h' bs)
  h' b@(BinaryNoParensBinder b1 b2 b3) =
    combine (combine (combine (h b) (h' b1)) (h' b2)) (h' b3)
  h' b@(ParensInBinder b1)   = combine (h b) (h' b1)
  h' b@(NamedBinder _ _ b1)  = combine (h b) (h' b1)
  h' b@(PositionedBinder _ _ b1) = combine (h b) (h' b1)
  h' b@(TypedBinder _ b1)    = combine (h b) (h' b1)
  h' b                       = h b

  lit :: forall a. r -> (a -> r) -> Literal a -> r
  lit r go (ArrayLiteral as)  = foldl combine r (map go as)
  lit r go (ObjectLiteral as) = foldl combine r (map (\(Tuple _ v) -> go v) as)
  lit r _  _                  = r

  i' :: CaseAlternative -> r
  i' ca@(CaseAlternative cas) =
    foldl combine (i ca)
      ( map h' cas.caseAlternativeBinders <>
        Array.concatMap (\(GuardedExpr grd val) -> map k' grd <> [g' val])
          cas.caseAlternativeResult
      )

  j' :: DoNotationElement -> r
  j' e@(DoNotationValue v)    = combine (j e) (g' v)
  j' e@(DoNotationBind b v)   = combine (combine (j e) (h' b)) (g' v)
  j' e@(DoNotationLet ds)     = foldl combine (j e) (map f' ds)
  j' e@(PositionedDoNotationElement _ _ e1) = combine (j e) (j' e1)

  k' :: Guard -> r
  k' (ConditionGuard e)       = g' e
  k' (PatternGuard b e)       = combine (h' b) (g' e)

-- -----------------------------------------------------------------------
-- everythingWithContextOnValues — collecting with context
-- -----------------------------------------------------------------------

everythingWithContextOnValues
  :: forall s r
   . s
  -> r
  -> (r -> r -> r)
  -> (s -> Declaration       -> Tuple s r)
  -> (s -> Expr              -> Tuple s r)
  -> (s -> Binder            -> Tuple s r)
  -> (s -> CaseAlternative   -> Tuple s r)
  -> (s -> DoNotationElement -> Tuple s r)
  -> EverythingResult r
everythingWithContextOnValues s0 r0 combine f g h i j =
  { decl: f'' s0, expr: g'' s0, binder: h'' s0, alt: i'' s0, doNotation: j'' s0 }
  where
  f'' :: s -> Declaration -> r
  f'' s d = let (Tuple s' r) = f s d in combine r (f' s' d)

  f' :: s -> Declaration -> r
  f' s (DataBindingGroupDeclaration ds) =
    foldl combine r0 (map (f'' s) (Array.fromFoldable ds))
  f' s (ValueDeclaration (ValueDeclarationData vd)) =
    foldl combine r0
      ( map (h'' s) vd.valdeclBinders <>
        Array.concatMap (\(GuardedExpr grd v) -> map (k' s) grd <> [g'' s v])
          vd.valdeclExpression
      )
  f' s (BindingGroupDeclaration ds) =
    foldl combine r0 (map (\(Tuple _ (Tuple _ val)) -> g'' s val) (Array.fromFoldable ds))
  f' s (TypeClassDeclaration _ _ _ _ _ ds) =
    foldl combine r0 (map (f'' s) ds)
  f' s (TypeInstanceDeclaration _ _ _ _ _ _ _ _ (ExplicitInstance ds)) =
    foldl combine r0 (map (f'' s) ds)
  f' _ _ = r0

  g'' :: s -> Expr -> r
  g'' s v = let (Tuple s' r) = g s v in combine r (g' s' v)

  g' :: s -> Expr -> r
  g' s (Literal _ l)          = lit g'' s l
  g' s (UnaryMinus _ v1)      = g'' s v1
  g' s (BinaryNoParens op v1 v2) =
    combine (combine (g'' s op) (g'' s v1)) (g'' s v2)
  g' s (Parens v1)            = g'' s v1
  g' s (Accessor _ v1)        = g'' s v1
  g' s (ObjectUpdate obj vs)  =
    foldl combine (g'' s obj) (map (\(Tuple _ v) -> g'' s v) vs)
  g' s (ObjectUpdateNested obj vs) =
    foldl combine (g'' s obj) (map (g'' s) vs)
  g' s (Abs binder v1)        = combine (h'' s binder) (g'' s v1)
  g' s (App v1 v2)            = combine (g'' s v1) (g'' s v2)
  g' s (VisibleTypeApp v _)   = g'' s v
  g' s (Unused v)             = g'' s v
  g' s (IfThenElse v1 v2 v3) =
    combine (combine (g'' s v1) (g'' s v2)) (g'' s v3)
  g' s (Case vs alts)         =
    foldl combine (foldl combine r0 (map (g'' s) vs)) (map (i'' s) alts)
  g' s (TypedValue _ v1 _)    = g'' s v1
  g' s (Let _ ds v1)          =
    foldl combine r0 (map (f'' s) ds) `combine` g'' s v1
  g' s (Do _ es)              = foldl combine r0 (map (j'' s) es)
  g' s (Ado _ es v1)          =
    foldl combine r0 (map (j'' s) es) `combine` g'' s v1
  g' s (PositionedValue _ _ v1) = g'' s v1
  g' _ _                      = r0

  h'' :: s -> Binder -> r
  h'' s b = let (Tuple s' r) = h s b in combine r (h' s' b)

  h' :: s -> Binder -> r
  h' s (LiteralBinder _ l)   = lit h'' s l
  h' s (ConstructorBinder _ _ bs) = foldl combine r0 (map (h'' s) bs)
  h' s (BinaryNoParensBinder b1 b2 b3) =
    combine (combine (h'' s b1) (h'' s b2)) (h'' s b3)
  h' s (ParensInBinder b)    = h'' s b
  h' s (NamedBinder _ _ b1)  = h'' s b1
  h' s (PositionedBinder _ _ b1) = h'' s b1
  h' s (TypedBinder _ b1)    = h'' s b1
  h' _ _                     = r0

  lit :: forall a. (s -> a -> r) -> s -> Literal a -> r
  lit go s (ArrayLiteral as)  = foldl combine r0 (map (go s) as)
  lit go s (ObjectLiteral as) = foldl combine r0 (map (\(Tuple _ v) -> go s v) as)
  lit _ _  _                  = r0

  i'' :: s -> CaseAlternative -> r
  i'' s ca = let (Tuple s' r) = i s ca in combine r (i' s' ca)

  i' :: s -> CaseAlternative -> r
  i' s (CaseAlternative cas) =
    foldl combine r0
      ( map (h'' s) cas.caseAlternativeBinders <>
        Array.concatMap (\(GuardedExpr grd val) -> map (k' s) grd <> [g'' s val])
          cas.caseAlternativeResult
      )

  j'' :: s -> DoNotationElement -> r
  j'' s e = let (Tuple s' r) = j s e in combine r (j' s' e)

  j' :: s -> DoNotationElement -> r
  j' s (DoNotationValue v)    = g'' s v
  j' s (DoNotationBind b v)   = combine (h'' s b) (g'' s v)
  j' s (DoNotationLet ds)     = foldl combine r0 (map (f'' s) ds)
  j' s (PositionedDoNotationElement _ _ e1) = j'' s e1

  k' :: s -> Guard -> r
  k' s (ConditionGuard e)     = g'' s e
  k' s (PatternGuard b e)     = combine (h'' s b) (g'' s e)

-- -----------------------------------------------------------------------
-- everywhereWithContextOnValues — pure version using Identity
-- -----------------------------------------------------------------------

everywhereWithContextOnValues
  :: forall s
   . s
  -> (s -> Declaration       -> Tuple s Declaration)
  -> (s -> Expr              -> Tuple s Expr)
  -> (s -> Binder            -> Tuple s Binder)
  -> (s -> CaseAlternative   -> Tuple s CaseAlternative)
  -> (s -> DoNotationElement -> Tuple s DoNotationElement)
  -> (s -> Guard             -> Tuple s Guard)
  -> WithContextResult
everywhereWithContextOnValues s f g h i j k =
  let result = everywhereWithContextOnValuesM s
        (\s2 d  -> Identity (f s2 d))
        (\s2 e  -> Identity (g s2 e))
        (\s2 b  -> Identity (h s2 b))
        (\s2 ca -> Identity (i s2 ca))
        (\s2 dn -> Identity (j s2 dn))
        (\s2 gu -> Identity (k s2 gu))
  in { decl:       \d  -> runIdentity (result.decl d)
     , expr:       \e  -> runIdentity (result.expr e)
     , binder:     \b  -> runIdentity (result.binder b)
     , alt:        \a  -> runIdentity (result.alt a)
     , doNotation: \dn -> runIdentity (result.doNotation dn)
     , guard:      \gu -> runIdentity (result.guard gu)
     }

-- -----------------------------------------------------------------------
-- everywhereWithContextOnValuesM — monadic with context
-- -----------------------------------------------------------------------

everywhereWithContextOnValuesM
  :: forall m s. Monad m
  => s
  -> (s -> Declaration       -> m (Tuple s Declaration))
  -> (s -> Expr              -> m (Tuple s Expr))
  -> (s -> Binder            -> m (Tuple s Binder))
  -> (s -> CaseAlternative   -> m (Tuple s CaseAlternative))
  -> (s -> DoNotationElement -> m (Tuple s DoNotationElement))
  -> (s -> Guard             -> m (Tuple s Guard))
  -> WithContextResultM m
everywhereWithContextOnValuesM s0 f g h i j k =
  { decl: f'' s0, expr: g'' s0, binder: h'' s0, alt: i'' s0, doNotation: j'' s0, guard: k'' s0 }
  where
  f'' :: s -> Declaration -> m Declaration
  f'' s d = f s d >>= \(Tuple s' d') -> f' s' d'

  f' :: s -> Declaration -> m Declaration
  f' s (DataBindingGroupDeclaration ds) =
    DataBindingGroupDeclaration <$> traverse (f'' s) ds
  f' s (ValueDeclaration (ValueDeclarationData vd)) = do
    bs  <- traverse (h'' s) vd.valdeclBinders
    val <- traverse (guardedExprM (k' s) (g'' s)) vd.valdeclExpression
    f s (ValueDeclaration (ValueDeclarationData vd { valdeclBinders = bs, valdeclExpression = val }))
      >>= \(Tuple _ d') -> pure d'
  f' s (BindingGroupDeclaration ds) =
    BindingGroupDeclaration <$>
      traverse (\(Tuple nameAnn (Tuple nameKind val)) ->
        (\v -> Tuple nameAnn (Tuple nameKind v)) <$> g'' s val) ds
  f' s (TypeClassDeclaration sa name args implies deps ds) =
    TypeClassDeclaration sa name args implies deps <$> traverse (f'' s) ds
  f' s (TypeInstanceDeclaration sa na ch idx name cs className args ds) =
    TypeInstanceDeclaration sa na ch idx name cs className args <$>
      traverseTypeInstanceBody (traverse (f'' s)) ds
  f' _ other = pure other

  g'' :: s -> Expr -> m Expr
  g'' s e = g s e >>= \(Tuple s' e') -> g' s' e'

  g' :: s -> Expr -> m Expr
  g' s (Literal ss l)         = Literal ss <$> lit g'' s l
  g' s (UnaryMinus ss v)      = UnaryMinus ss <$> g'' s v
  g' s (BinaryNoParens op v1 v2) =
    BinaryNoParens <$> g'' s op <*> g'' s v1 <*> g'' s v2
  g' s (Parens v)             = Parens <$> g'' s v
  g' s (Accessor prop v)      = Accessor prop <$> g'' s v
  g' s (ObjectUpdate obj vs)  =
    ObjectUpdate <$> g'' s obj <*>
      traverse (\(Tuple key val) -> Tuple key <$> g'' s val) vs
  g' s (ObjectUpdateNested obj vs) =
    ObjectUpdateNested <$> g'' s obj <*> traverse (g'' s) vs
  g' s (Abs binder v)         = Abs <$> h' s binder <*> g'' s v
  g' s (App v1 v2)            = App <$> g'' s v1 <*> g'' s v2
  g' s (VisibleTypeApp v ty)  = VisibleTypeApp <$> g'' s v <*> pure ty
  g' s (Unused v)             = Unused <$> g'' s v
  g' s (IfThenElse v1 v2 v3) =
    IfThenElse <$> g'' s v1 <*> g'' s v2 <*> g'' s v3
  g' s (Case vs alts)         = Case <$> traverse (g'' s) vs <*> traverse (i'' s) alts
  g' s (TypedValue check v ty) = TypedValue check <$> g'' s v <*> pure ty
  g' s (Let w ds v)           = Let w <$> traverse (f'' s) ds <*> g'' s v
  g' s (Do m es)              = Do m <$> traverse (j'' s) es
  g' s (Ado m es v)           = Ado m <$> traverse (j'' s) es <*> g'' s v
  g' s (PositionedValue pos com v) = PositionedValue pos com <$> g'' s v
  g' _ other                  = pure other

  h'' :: s -> Binder -> m Binder
  h'' s b = h s b >>= \(Tuple s' b') -> h' s' b'

  h' :: s -> Binder -> m Binder
  h' s (LiteralBinder ss l)   = LiteralBinder ss <$> lit h'' s l
  h' s (ConstructorBinder ss ctor bs) =
    ConstructorBinder ss ctor <$> traverse (h'' s) bs
  h' s (BinaryNoParensBinder b1 b2 b3) =
    BinaryNoParensBinder <$> h'' s b1 <*> h'' s b2 <*> h'' s b3
  h' s (ParensInBinder b)     = ParensInBinder <$> h'' s b
  h' s (NamedBinder ss name b) = NamedBinder ss name <$> h'' s b
  h' s (PositionedBinder pos com b) = PositionedBinder pos com <$> h'' s b
  h' s (TypedBinder t b)      = TypedBinder t <$> h'' s b
  h' _ other                  = pure other

  lit :: forall a. (s -> a -> m a) -> s -> Literal a -> m (Literal a)
  lit go s (ArrayLiteral as)  = ArrayLiteral <$> traverse (go s) as
  lit go s (ObjectLiteral as) =
    ObjectLiteral <$> traverse (\(Tuple key v) -> Tuple key <$> go s v) as
  lit _ _ other               = pure other

  i'' :: s -> CaseAlternative -> m CaseAlternative
  i'' s ca = i s ca >>= \(Tuple s' ca') -> i' s' ca'

  i' :: s -> CaseAlternative -> m CaseAlternative
  i' s (CaseAlternative cas) = do
    bs  <- traverse (h'' s) cas.caseAlternativeBinders
    val <- traverse (guardedExprM' s) cas.caseAlternativeResult
    pure (CaseAlternative cas { caseAlternativeBinders = bs, caseAlternativeResult = val })

  -- Threads state through guards, exposing accumulated scope to the final expr
  guardedExprM' :: s -> GuardedExpr -> m GuardedExpr
  guardedExprM' s (GuardedExpr guards expr) = do
    Tuple guards' s' <- runStateT (traverse (StateT <<< goGuard) guards) s
    GuardedExpr guards' <$> g'' s' expr

  -- Apply user's k to get new state+guard, then recursively traverse the guard
  goGuard :: Guard -> s -> m (Tuple Guard s)
  goGuard guard s = do
    Tuple s' guard' <- k s guard
    guard'' <- k' s' guard'
    pure (Tuple guard'' s')

  j'' :: s -> DoNotationElement -> m DoNotationElement
  j'' s e = j s e >>= \(Tuple s' e') -> j' s' e'

  j' :: s -> DoNotationElement -> m DoNotationElement
  j' s (DoNotationValue v)    = DoNotationValue <$> g'' s v
  j' s (DoNotationBind b v)   = DoNotationBind <$> h'' s b <*> g'' s v
  j' s (DoNotationLet ds)     = DoNotationLet <$> traverse (f'' s) ds
  j' s (PositionedDoNotationElement pos com e1) =
    PositionedDoNotationElement pos com <$> j'' s e1

  k'' :: s -> Guard -> m Guard
  k'' s g2 = k s g2 >>= \(Tuple s' g2') -> k' s' g2'

  k' :: s -> Guard -> m Guard
  k' s (ConditionGuard e) = ConditionGuard <$> g'' s e
  k' s (PatternGuard b e) = PatternGuard <$> h'' s b <*> g'' s e

-- -----------------------------------------------------------------------
-- ScopedIdent and everythingWithScope
-- -----------------------------------------------------------------------

data ScopedIdent = LocalIdent Ident | ToplevelIdent Ident

derive instance eqScopedIdent  :: Eq ScopedIdent
derive instance ordScopedIdent :: Ord ScopedIdent

instance showScopedIdent :: Show ScopedIdent where
  show (LocalIdent i)    = "(LocalIdent " <> show i <> ")"
  show (ToplevelIdent i) = "(ToplevelIdent " <> show i <> ")"

inScope :: Ident -> Set ScopedIdent -> Boolean
inScope i s = Set.member (LocalIdent i) s || Set.member (ToplevelIdent i) s

everythingWithScope
  :: forall r. Monoid r
  => (Set ScopedIdent -> Declaration -> r)
  -> (Set ScopedIdent -> Expr -> r)
  -> (Set ScopedIdent -> Binder -> r)
  -> (Set ScopedIdent -> CaseAlternative -> r)
  -> (Set ScopedIdent -> DoNotationElement -> r)
  -> WithScopeResult r
everythingWithScope f g h i j =
  { decl: f''
  , expr: g''
  , binder: h''
  , alt: i''
  , doNotation: \s e -> snd' (j'' s e)
  }
  where
  snd' :: forall a b. Tuple a b -> b
  snd' (Tuple _ b) = b

  f'' :: Set ScopedIdent -> Declaration -> r
  f'' s a = f s a <> f' s a

  f' :: Set ScopedIdent -> Declaration -> r
  f' s (DataBindingGroupDeclaration ds) =
    let s' = Set.union s (Set.fromFoldable (mapMaybe getDeclIdent (NEL.toUnfoldable ds :: Array Declaration) <#> ToplevelIdent))
    in foldMap (f'' s') ds
  f' s (ValueDeclaration (ValueDeclarationData vd)) =
    let s'  = Set.insert (ToplevelIdent vd.valdeclIdent) s
        s'' = Set.union s' (Set.fromFoldable (Array.concatMap localBinderNames vd.valdeclBinders))
    in foldMap (h'' s') vd.valdeclBinders <>
       foldMap (l' s'') vd.valdeclExpression
  f' s (BindingGroupDeclaration ds) =
    let s' = Set.union s (Set.fromFoldable
          (map (\(Tuple (Tuple _ name) _) -> ToplevelIdent name) (NEL.toUnfoldable ds :: Array _)))
    in foldMap (\(Tuple _ (Tuple _ val)) -> g'' s' val) ds
  f' s (TypeClassDeclaration _ _ _ _ _ ds) = foldMap (f'' s) ds
  f' s (TypeInstanceDeclaration _ _ _ _ _ _ _ _ (ExplicitInstance ds)) = foldMap (f'' s) ds
  f' _ _ = mempty

  g'' :: Set ScopedIdent -> Expr -> r
  g'' s a = g s a <> g' s a

  g' :: Set ScopedIdent -> Expr -> r
  g' s (Literal _ l)          = lit g'' s l
  g' s (UnaryMinus _ v1)      = g'' s v1
  g' s (BinaryNoParens op v1 v2) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens v1)            = g'' s v1
  g' s (Accessor _ v1)        = g'' s v1
  g' s (ObjectUpdate obj vs)  = g'' s obj <> foldMap (\(Tuple _ v) -> g'' s v) vs
  g' s (ObjectUpdateNested obj vs) = g'' s obj <> foldMap (g'' s) vs
  g' s (Abs b v1) =
    let s' = Set.union (Set.fromFoldable (localBinderNames b)) s
    in h'' s b <> g'' s' v1
  g' s (App v1 v2)            = g'' s v1 <> g'' s v2
  g' s (VisibleTypeApp v _)   = g'' s v
  g' s (Unused v)             = g'' s v
  g' s (IfThenElse v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case vs alts)         = foldMap (g'' s) vs <> foldMap (i'' s) alts
  g' s (TypedValue _ v1 _)    = g'' s v1
  g' s (Let _ ds v1) =
    let s' = Set.union s (Set.fromFoldable (map LocalIdent (mapMaybe getDeclIdent ds)))
    in foldMap (f'' s') ds <> g'' s' v1
  g' s (Do _ es) =
    fold (mapAccumL (\acc e ->
      let Tuple acc' r = j'' acc e in { accum: acc', value: r }) s es).value
  g' s (Ado _ es v1) =
    let s' = Set.union s (foldMap (\e -> fst (j'' s e)) es)
    in g'' s' v1
  g' s (PositionedValue _ _ v1) = g'' s v1
  g' _ _                      = mempty

  h'' :: Set ScopedIdent -> Binder -> r
  h'' s a = h s a <> h' s a

  h' :: Set ScopedIdent -> Binder -> r
  h' s (LiteralBinder _ l)   = lit h'' s l
  h' s (ConstructorBinder _ _ bs) = foldMap (h'' s) bs
  h' s (BinaryNoParensBinder b1 b2 b3) = h'' s b1 <> h'' s b2 <> h'' s b3
  h' s (ParensInBinder b)    = h'' s b
  h' s (NamedBinder _ name b1) = h'' (Set.insert (LocalIdent name) s) b1
  h' s (PositionedBinder _ _ b1) = h'' s b1
  h' s (TypedBinder _ b1)    = h'' s b1
  h' _ _                     = mempty

  lit :: forall a. (Set ScopedIdent -> a -> r) -> Set ScopedIdent -> Literal a -> r
  lit go s (ArrayLiteral as)  = foldMap (go s) as
  lit go s (ObjectLiteral as) = foldMap (\(Tuple _ v) -> go s v) as
  lit _ _  _                  = mempty

  i'' :: Set ScopedIdent -> CaseAlternative -> r
  i'' s a = i s a <> i' s a

  i' :: Set ScopedIdent -> CaseAlternative -> r
  i' s (CaseAlternative cas) =
    let s' = Set.union s (Set.fromFoldable
          (Array.concatMap localBinderNames cas.caseAlternativeBinders))
    in foldMap (h'' s) cas.caseAlternativeBinders <>
       foldMap (l' s') cas.caseAlternativeResult

  -- Returns (accumulated scope, collected result) for threading through do-blocks
  j'' :: Set ScopedIdent -> DoNotationElement -> Tuple (Set ScopedIdent) r
  j'' s a = let Tuple s' r = j' s a in Tuple s' (j s a <> r)

  j' :: Set ScopedIdent -> DoNotationElement -> Tuple (Set ScopedIdent) r
  j' s (DoNotationValue v) = Tuple s (g'' s v)
  j' s (DoNotationBind b v) =
    let s' = Set.union (Set.fromFoldable (localBinderNames b)) s
    in Tuple s' (h'' s b <> g'' s v)
  j' s (DoNotationLet ds) =
    let s' = Set.union s (Set.fromFoldable (map LocalIdent (mapMaybe getDeclIdent ds)))
    in Tuple s' (foldMap (f'' s') ds)
  j' s (PositionedDoNotationElement _ _ e1) = j'' s e1

  k' :: Set ScopedIdent -> Guard -> Tuple (Set ScopedIdent) r
  k' s (ConditionGuard e) = Tuple s (g'' s e)
  k' s (PatternGuard b e) =
    let s' = Set.union (Set.fromFoldable (localBinderNames b)) s
    in Tuple s' (h'' s b <> g'' s' e)

  l' :: Set ScopedIdent -> GuardedExpr -> r
  l' s (GuardedExpr grds e) = case Array.uncons grds of
    Nothing -> g'' s e
    Just { head: grd, tail: gs } ->
      let (Tuple s' r) = k' s grd
      in r <> l' s' (GuardedExpr gs e)

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent (ValueDeclaration (ValueDeclarationData vd)) = Just vd.valdeclIdent
  getDeclIdent (TypeDeclaration (TypeDeclarationData td))   = Just td.tydeclIdent
  getDeclIdent _                                            = Nothing

  localBinderNames :: Binder -> Array ScopedIdent
  localBinderNames = map LocalIdent <<< binderNames

-- -----------------------------------------------------------------------
-- accumTypes and overTypes
-- -----------------------------------------------------------------------

accumTypes
  :: forall r. Monoid r
  => (SourceType -> r)
  -> EverythingResult r
accumTypes f = everythingOnValues append forDecls forValues forBinders (const mempty) (const mempty)
  where
  forDecls :: Declaration -> r
  forDecls (DataDeclaration _ _ _ args dctors) =
    foldMap (\(Tuple _ mty) -> foldMap f mty) args <>
    foldMap (\(DataConstructorDeclaration dc) -> foldMap (\(Tuple _ ty) -> f ty) dc.dataCtorFields) dctors
  forDecls (ExternDataDeclaration _ _ ty) = f ty
  forDecls (ExternDeclaration _ _ ty)     = f ty
  forDecls (TypeClassDeclaration _ _ args implies _ _) =
    foldMap (\(Tuple _ mty) -> foldMap f mty) args <>
    foldMap (foldMap f <<< getConstraintArgs) implies
  forDecls (TypeInstanceDeclaration _ _ _ _ _ cs _ tys _) =
    foldMap (foldMap f <<< getConstraintArgs) cs <> foldMap f tys
  forDecls (TypeSynonymDeclaration _ _ args ty) =
    foldMap (\(Tuple _ mty) -> foldMap f mty) args <> f ty
  forDecls (KindDeclaration _ _ _ ty) = f ty
  forDecls (TypeDeclaration (TypeDeclarationData td)) = f td.tydeclType
  forDecls _ = mempty

  forValues :: Expr -> r
  forValues (TypeClassDictionary c _ _) = foldMap f (getConstraintArgs c)
  forValues (DeferredDictionary _ tys)  = foldMap f tys
  forValues (TypedValue _ _ ty)         = f ty
  forValues (VisibleTypeApp _ ty)       = f ty
  forValues _                           = mempty

  forBinders :: Binder -> r
  forBinders (TypedBinder ty _) = f ty
  forBinders _                  = mempty

  getConstraintArgs :: SourceConstraint -> Array SourceType
  getConstraintArgs (Constraint c) = c.constraintArgs

overTypes :: (SourceType -> SourceType) -> Expr -> Expr
overTypes f e =
  let result = everywhereOnValues identity applyF identity
  in result.expr e
  where
  applyF :: Expr -> Expr
  applyF (TypedValue checkTy val t) = TypedValue checkTy val (f t)
  applyF (TypeClassDictionary c sco hints) =
    TypeClassDictionary (mapConstraintArgs (map f) c) (updateCtx sco) hints
  applyF other = other

  updateDict :: NamedDict -> NamedDict
  updateDict (TypeClassDictionaryInScope d) =
    TypeClassDictionaryInScope d { tcdInstanceTypes = map f d.tcdInstanceTypes }

  -- Apply updateDict through: Maybe -> Map ClassName -> Map Ident -> Array
  updateScope
    :: Maybe (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
    -> Maybe (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
  updateScope = map (map (map (map updateDict)))

  byNullSourcePos :: QualifiedBy
  byNullSourcePos = BySourcePos (SourcePos { line: 0, column: 0 })

  updateCtx
    :: Map QualifiedBy (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
    -> Map QualifiedBy (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
  updateCtx = Map.alter updateScope byNullSourcePos

defS :: forall m s v. Monad m => s -> v -> m (Tuple s v)
defS s val = pure (Tuple s val)

sndM :: forall f a b c. Functor f => (b -> f c) -> Tuple a b -> f (Tuple a c)
sndM f (Tuple a b) = Tuple a <$> f b
