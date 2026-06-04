module Language.PureScript.Sugar.DoNotation (desugarDoModule) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Supply.Class (class MonadSupply, freshIdent')
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Binders (Binder(..), binderNames)
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , Declaration(..)
  , DoNotationElement(..)
  , Expr(..)
  , GuardedExpr(..)
  , Module(..)
  , ValueDeclarationData(..)
  , WhereProvenance(..)
  , declSourceSpan
  )
import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan)
import Language.PureScript.Constants.Libs (sBind, sDiscard)
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , errorMessage
  , rethrowWithPosition
  )
import Language.PureScript.Names (Ident(..), ModuleName, Qualified(..), QualifiedBy(..), byMaybeModuleName)

desugarDoModule :: forall m. MonadSupply m => MonadError MultipleErrors m => Module -> m Module
desugarDoModule (Module ss coms mn ds exts) =
  Module ss coms mn <$> traverse desugarDo ds <*> pure exts

desugarDo :: forall m. MonadSupply m => MonadError MultipleErrors m => Declaration -> m Declaration
desugarDo d =
  let ss = declSourceSpan d
  in rethrowWithPosition ss (transformDecl ss d)
  where
  bind' :: SourceSpan -> Maybe ModuleName -> Expr
  bind' ss m = Var ss (Qualified (byMaybeModuleName m) (Ident sBind))

  discard' :: SourceSpan -> Maybe ModuleName -> Expr
  discard' ss m = Var ss (Qualified (byMaybeModuleName m) (Ident sDiscard))

  transformDecl :: SourceSpan -> Declaration -> m Declaration
  transformDecl ss decl = case decl of
    ValueDeclaration (ValueDeclarationData vd) -> do
      gs' <- traverse (transformGuardedExpr ss) vd.valdeclExpression
      pure $ ValueDeclaration (ValueDeclarationData vd { valdeclExpression = gs' })
    _ -> pure decl

  transformGuardedExpr :: SourceSpan -> GuardedExpr -> m GuardedExpr
  transformGuardedExpr ss (GuardedExpr gs e) = GuardedExpr gs <$> replace ss e

  replace :: SourceSpan -> Expr -> m Expr
  replace pos (Do m els) = go pos m els
  replace _ (PositionedValue pos com v) = PositionedValue pos com <$> rethrowWithPosition pos (replace pos v)
  replace _ other = pure other

  stripPositionedBinder :: Binder -> Tuple (Maybe SourceSpan) Binder
  stripPositionedBinder (PositionedBinder ss _ b) =
    let Tuple ss' b' = stripPositionedBinder b
    in Tuple (ss' <|> Just ss) b'
  stripPositionedBinder b = Tuple Nothing b

  go :: SourceSpan -> Maybe ModuleName -> Array DoNotationElement -> m Expr
  go _ _ [] =
    throwError $ errorMessage $ InternalCompilerError "desugarDo" "Empty do block"
  go _ _ els | Just { head: DoNotationValue val, tail: [] } <- Array.uncons els =
    pure val
  go pos m els | Just { head: DoNotationValue val, tail: rest } <- Array.uncons els = do
    rest' <- go pos m rest
    pure $ App (App (discard' pos m) val) (Abs (VarBinder pos UnusedIdent) rest')
  go _ _ els | Just { head: DoNotationBind _ _, tail: [] } <- Array.uncons els =
    throwError $ errorMessage InvalidDoBind
  go _ _ els
    | Just { head: DoNotationBind b _ } <- Array.uncons els
    , Just ident <- findBindOrDiscard (binderNames b) =
        throwError $ errorMessage $ CannotUseBindWithDo (Ident ident)
  go pos m els | Just { head: DoNotationBind binder val, tail: rest } <- Array.uncons els = do
    rest' <- go pos m rest
    let Tuple mss binder' = stripPositionedBinder binder
        ss = fromMaybe pos mss
    case binder' of
      NullBinder ->
        pure $ App (App (bind' pos m) val) (Abs (VarBinder ss UnusedIdent) rest')
      VarBinder _ ident ->
        pure $ App (App (bind' pos m) val) (Abs (VarBinder ss ident) rest')
      _ -> do
        ident <- freshIdent'
        pure $ App (App (bind' pos m) val) (Abs (VarBinder pos ident)
          (Case [Var pos (Qualified (BySourcePos (SourcePos { line: 0, column: 0 })) ident)]
            [CaseAlternative
              { caseAlternativeBinders: [binder]
              , caseAlternativeResult: [GuardedExpr [] rest']
              }]))
  go _ _ els | Just { head: DoNotationLet _, tail: [] } <- Array.uncons els =
    throwError $ errorMessage InvalidDoLet
  go pos m els | Just { head: DoNotationLet ds', tail: rest } <- Array.uncons els = do
    rest' <- go pos m rest
    pure $ Let FromLet ds' rest'
  go pos m els | Just { head: PositionedDoNotationElement pos' com el, tail: rest } <- Array.uncons els =
    rethrowWithPosition pos' $ PositionedValue pos' com <$> go pos' m (Array.cons el rest)
  go _ _ _ =
    throwError $ errorMessage $ InternalCompilerError "desugarDo" "Unexpected do element"

  findBindOrDiscard :: Array Ident -> Maybe String
  findBindOrDiscard idents = Array.findMap getBindOrDiscard idents
    where
    getBindOrDiscard (Ident name)
      | name == sBind || name == sDiscard = Just name
    getBindOrDiscard _ = Nothing
