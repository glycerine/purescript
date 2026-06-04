module Language.PureScript.Sugar.LetPattern (desugarLetPatternModule) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Binders (Binder)
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , Declaration(..)
  , Expr(..)
  , GuardedExpr(..)
  , Module(..)
  , WhereProvenance
  )
import Language.PureScript.AST.SourcePos (SourceAnn)
import Language.PureScript.AST.Traversals (everywhereOnValues)
import Language.PureScript.Crash (internalError)

desugarLetPatternModule :: Module -> Module
desugarLetPatternModule (Module ss coms mn ds exts) =
  Module ss coms mn (map desugarLetPattern ds) exts

desugarLetPattern :: Declaration -> Declaration
desugarLetPattern decl =
  let t = everywhereOnValues identity replace identity
  in t.decl decl
  where
  replace :: Expr -> Expr
  replace (Let w ds e) = go w (partitionDecls ds) e
  replace other = other

  go :: WhereProvenance
     -> Array (Either (Array Declaration) { ann :: SourceAnn, binder :: Binder, bound :: Expr })
     -> Expr
     -> Expr
  go _ [] e = e
  go w decls e = case Array.uncons decls of
    Nothing -> e
    Just { head: Right { ann: Tuple pos com, binder, bound: boundE }, tail: rest } ->
      PositionedValue pos com $
        Case [boundE]
          [CaseAlternative
            { caseAlternativeBinders: [binder]
            , caseAlternativeResult: [GuardedExpr [] (go w rest e)]
            }]
    Just { head: Left ds, tail: rest } ->
      Let w ds (go w rest e)

partitionDecls
  :: Array Declaration
  -> Array (Either (Array Declaration) { ann :: SourceAnn, binder :: Binder, bound :: Expr })
partitionDecls ds = Array.concatMap f (groupByIsBound ds)
  where
  f group = case Array.head group of
    Just (BoundValueDeclaration _ _ _) -> map (Right <<< extractBound) group
    _ -> [Left group]

  extractBound (BoundValueDeclaration sa binder expr) =
    { ann: sa, binder: binder, bound: expr }
  extractBound _ = internalError "partitionDecls: the impossible happened."

isBoundValueDeclaration :: Declaration -> Boolean
isBoundValueDeclaration (BoundValueDeclaration _ _ _) = true
isBoundValueDeclaration _ = false

groupByIsBound :: Array Declaration -> Array (Array Declaration)
groupByIsBound = go []
  where
  go acc [] = if Array.null acc then [] else [Array.reverse acc]
  go acc arr = case Array.uncons arr of
    Nothing -> if Array.null acc then [] else [Array.reverse acc]
    Just { head: x, tail: xs } ->
      case Array.head acc of
        Nothing ->
          go [x] xs
        Just prev ->
          if isBoundValueDeclaration x == isBoundValueDeclaration prev
            then go (Array.cons x acc) xs
            else Array.cons (Array.reverse acc) (go [x] xs)
