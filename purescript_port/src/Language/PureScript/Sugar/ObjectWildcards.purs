module Language.PureScript.Sugar.ObjectWildcards
  ( desugarObjectConstructors
  , desugarDecl
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Supply.Class (class MonadSupply, freshIdent')
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (catMaybes)
import Data.Traversable (class Traversable, traverse, for)
import Data.Tuple (Tuple(..), snd, fst)

import Language.PureScript.AST.Declarations
  ( AssocList(..)
  , CaseAlternative
  , Declaration(..)
  , Expr(..)
  , GuardedExpr(..)
  , Module(..)
  , PathNode(..)
  , PathTree(..)
  , ValueDeclarationData(..)
  , WhereProvenance(..)
  , declSourceSpan
  , isAnonymousArgument
  )
import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan(..), nullSourceAnn)
import Language.PureScript.AST.Traversals (everywhereOnValuesTopDownM)
import Language.PureScript.Environment (NameKind(..))
import Language.PureScript.Errors (MultipleErrors, rethrowWithPosition)
import Language.PureScript.Names (Ident, Qualified(..), QualifiedBy(..))
import Language.PureScript.PSString (PSString)

nullSourceSpan :: SourceSpan
nullSourceSpan = SourceSpan
  { name: ""
  , start: SourcePos { line: 0, column: 0 }
  , end:   SourcePos { line: 0, column: 0 }
  }

desugarObjectConstructors
  :: forall m
   . MonadSupply m
  => MonadError MultipleErrors m
  => Module
  -> m Module
desugarObjectConstructors (Module ss coms mn ds exts) =
  Module ss coms mn <$> traverse desugarDecl ds <*> pure exts

desugarDecl :: forall m. MonadSupply m => MonadError MultipleErrors m => Declaration -> m Declaration
desugarDecl d = rethrowWithPosition (declSourceSpan d) (fn d)
  where
  t = everywhereOnValuesTopDownM pure desugarExpr pure
  fn = t.decl

  desugarExpr :: Expr -> m Expr
  desugarExpr (Literal ss (ObjectLiteral ps)) =
    wrapLambdaAssoc (Literal ss <<< ObjectLiteral) ps
  desugarExpr (ObjectUpdateNested obj ps) =
    transformNestedUpdate obj ps
  desugarExpr (Accessor prop u)
    | Just props <- peelAnonAccessorChain u = do
        arg <- freshIdent'
        pure $ Abs (VarBinder nullSourceSpan arg) $
          Array.foldr Accessor (argToExpr arg) (Array.cons prop props)
  desugarExpr (Case args cas) | Array.any isAnonymousArgument args = do
    argIdents <- traverse freshIfAnon args
    let args' = Array.zipWith (\a mi -> case mi of
                                  Nothing -> a
                                  Just i  -> argToExpr i) args argIdents
    pure $ Array.foldr (Abs <<< VarBinder nullSourceSpan) (Case args' cas) (catMaybes argIdents)
  desugarExpr (IfThenElse u t' f) | Array.any isAnonymousArgument [u, t', f] = do
    u' <- freshIfAnon u
    t'' <- freshIfAnon t'
    f' <- freshIfAnon f
    let if_ = IfThenElse
          (fromMaybe u (map argToExpr u'))
          (fromMaybe t' (map argToExpr t''))
          (fromMaybe f (map argToExpr f'))
    pure $ Array.foldr (Abs <<< VarBinder nullSourceSpan) if_ (catMaybes [u', t'', f'])
  desugarExpr e = pure e

  transformNestedUpdate :: Expr -> PathTree Expr -> m Expr
  transformNestedUpdate obj ps = do
    val <- freshIdent'
    let valExpr = argToExpr val
    if isAnonymousArgument obj
      then Abs (VarBinder nullSourceSpan val) <$> wrapLambda (buildUpdates valExpr) ps
      else wrapLambda (buildLet val <<< buildUpdates valExpr) ps
    where
    buildLet val' expr =
      Let FromLet
        [ValueDeclaration (ValueDeclarationData
          { valdeclSourceAnn: nullSourceAnn
          , valdeclIdent: val'
          , valdeclName: Public
          , valdeclBinders: []
          , valdeclExpression: [GuardedExpr [] obj]
          })]
        expr

    buildUpdates :: Expr -> PathTree Expr -> Expr
    buildUpdates v (PathTree (AssocList vs)) =
      ObjectUpdate v (map (goLayer []) vs)
      where
      goLayer :: Array PSString -> Tuple PSString (PathNode Expr) -> Tuple PSString Expr
      goLayer _ (Tuple key (Leaf expr)) = Tuple key expr
      goLayer path (Tuple key (Branch (PathTree (AssocList branch)))) =
        let path' = Array.snoc path key
            updates = map (goLayer path') branch
            accessor = Array.foldl (flip Accessor) v path'
        in Tuple key (ObjectUpdate accessor updates)

  wrapLambda :: forall t. Traversable t => (t Expr -> Expr) -> t Expr -> m Expr
  wrapLambda mkVal ps = do
    args <- traverse processExpr ps
    let idents = catMaybes (Array.fromFoldable (map fst args))
        exprs  = map snd args
    pure $ Array.foldr (Abs <<< VarBinder nullSourceSpan) (mkVal exprs) idents
    where
    processExpr :: Expr -> m (Tuple (Maybe Ident) Expr)
    processExpr e = do
      arg <- freshIfAnon e
      pure (Tuple arg (fromMaybe e (map argToExpr arg)))

  wrapLambdaAssoc :: (Array (Tuple PSString Expr) -> Expr) -> Array (Tuple PSString Expr) -> m Expr
  wrapLambdaAssoc mkVal = wrapLambda (mkVal <<< runAssocList) <<< AssocList

  peelAnonAccessorChain :: Expr -> Maybe (Array PSString)
  peelAnonAccessorChain (Accessor p e) = map (Array.cons p) (peelAnonAccessorChain e)
  peelAnonAccessorChain (PositionedValue _ _ e) = peelAnonAccessorChain e
  peelAnonAccessorChain AnonymousArgument = Just []
  peelAnonAccessorChain _ = Nothing

  freshIfAnon :: Expr -> m (Maybe Ident)
  freshIfAnon u
    | isAnonymousArgument u = Just <$> freshIdent'
    | otherwise = pure Nothing

  argToExpr :: Ident -> Expr
  argToExpr = Var nullSourceSpan <<< Qualified (BySourcePos (SourcePos { line: 0, column: 0 }))

runAssocList :: forall k v. AssocList k v -> Array (Tuple k v)
runAssocList (AssocList xs) = xs
