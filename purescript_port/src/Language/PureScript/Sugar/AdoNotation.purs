module Language.PureScript.Sugar.AdoNotation (desugarAdoModule) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Supply.Class (class MonadSupply, freshIdent')
import Data.Array as Array
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , Declaration(..)
  , DoNotationElement(..)
  , Expr(..)
  , GuardedExpr(..)
  , Module(..)
  , WhereProvenance(..)
  , ValueDeclarationData(..)
  , declSourceSpan
  )
import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan)
import Language.PureScript.Constants.Libs (sApply, sMap, sPure)
import Language.PureScript.Errors
  ( MultipleErrors
  , parU
  , rethrowWithPosition
  )
import Language.PureScript.Names (Ident(..), ModuleName, Qualified(..), QualifiedBy(..), byMaybeModuleName)

desugarAdoModule :: forall m. MonadSupply m => MonadError MultipleErrors m => Module -> m Module
desugarAdoModule (Module ss coms mn ds exts) =
  Module ss coms mn <$> parU ds desugarAdo <*> pure exts

desugarAdo :: forall m. MonadSupply m => MonadError MultipleErrors m => Declaration -> m Declaration
desugarAdo d =
  let ss = declSourceSpan d
  in rethrowWithPosition ss (transformDecl ss d)
  where
  pure' :: SourceSpan -> Maybe ModuleName -> Expr
  pure' ss m = Var ss (Qualified (byMaybeModuleName m) (Ident sPure))

  map' :: SourceSpan -> Maybe ModuleName -> Expr
  map' ss m = Var ss (Qualified (byMaybeModuleName m) (Ident sMap))

  apply' :: SourceSpan -> Maybe ModuleName -> Expr
  apply' ss m = Var ss (Qualified (byMaybeModuleName m) (Ident sApply))

  transformDecl :: SourceSpan -> Declaration -> m Declaration
  transformDecl ss decl = case decl of
    ValueDeclaration (ValueDeclarationData vd) -> do
      gs' <- traverse (transformGuardedExpr ss) vd.valdeclExpression
      pure $ ValueDeclaration (ValueDeclarationData vd { valdeclExpression = gs' })
    _ -> pure decl

  transformGuardedExpr :: SourceSpan -> GuardedExpr -> m GuardedExpr
  transformGuardedExpr ss (GuardedExpr gs e) = GuardedExpr gs <$> replace ss e

  replace :: SourceSpan -> Expr -> m Expr
  replace pos (Ado m els yield) = do
    Tuple func args <- foldM (go pos) (Tuple yield []) (Array.reverse els)
    pure $ case args of
      [] -> App (pure' pos m) func
      _ ->
        let hd = Array.head args
            tl = Array.drop 1 args
        in case hd of
          Nothing -> App (pure' pos m) func
          Just h  ->
            Array.foldl (\a b -> App (App (apply' pos m) a) b)
              (App (App (map' pos m) func) h)
              tl
  replace _ (PositionedValue pos com v) =
    PositionedValue pos com <$> rethrowWithPosition pos (replace pos v)
  replace _ other = pure other

  go :: SourceSpan -> Tuple Expr (Array Expr) -> DoNotationElement -> m (Tuple Expr (Array Expr))
  go _ (Tuple yield args) (DoNotationValue val) =
    pure $ Tuple (Abs NullBinder yield) (Array.cons val args)
  go _ (Tuple yield args) (DoNotationBind (VarBinder ss ident) val) =
    pure $ Tuple (Abs (VarBinder ss ident) yield) (Array.cons val args)
  go ss (Tuple yield args) (DoNotationBind binder val) = do
    ident <- freshIdent'
    let nullPos = SourcePos { line: 0, column: 0 }
        abs' = Abs (VarBinder ss ident)
          (Case [Var ss (Qualified (BySourcePos nullPos) ident)]
            [CaseAlternative
              { caseAlternativeBinders: [binder]
              , caseAlternativeResult: [GuardedExpr [] yield]
              }])
    pure $ Tuple abs' (Array.cons val args)
  go _ (Tuple yield args) (DoNotationLet ds') =
    pure $ Tuple (Let FromLet ds' yield) args
  go _ acc (PositionedDoNotationElement pos com el) =
    rethrowWithPosition pos do
      Tuple yield args <- go pos acc el
      pure $ case Array.uncons args of
        Nothing ->
          Tuple (PositionedValue pos com yield) args
        Just { head: a, tail: as } ->
          Tuple yield (Array.cons (PositionedValue pos com a) as)

foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> Array a -> m b
foldM _ acc [] = pure acc
foldM f acc arr = case Array.uncons arr of
  Nothing -> pure acc
  Just { head: x, tail: xs } -> do
    acc' <- f acc x
    foldM f acc' xs
