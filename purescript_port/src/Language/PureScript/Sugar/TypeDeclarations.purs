module Language.PureScript.Sugar.TypeDeclarations
  ( desugarTypeDeclarationsModule
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (fst)

import Language.PureScript.AST.Declarations
  ( Declaration(..)
  , ErrorMessageHint(..)
  , Expr(..)
  , GuardedExpr(..)
  , KindSignatureFor(..)
  , Module(..)
  , RoleDeclarationData(..)
  , TypeDeclarationData(..)
  , TypeInstanceBody(..)
  , ValueDeclarationData(..)
  , declSourceSpan
  , traverseTypeInstanceBody
  )
import Language.PureScript.AST.SourcePos (nullSourceAnn)
import Language.PureScript.AST.Traversals (everywhereOnValuesTopDownM)
import Language.PureScript.Environment (DataDeclType(..), NameKind(..))
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage'
  , rethrow
  )
import Language.PureScript.Names (Ident, ProperName, TypeName, coerceProperName)

desugarTypeDeclarationsModule
  :: forall m
   . MonadError MultipleErrors m
  => Module
  -> m Module
desugarTypeDeclarationsModule (Module modSS coms name ds exps) =
  rethrow (addHint (ErrorInModule name)) $ do
    checkKindDeclarations ds
    checkRoleDeclarations Nothing ds
    Module modSS coms name <$> desugarTypeDeclarations ds <*> pure exps

  where

  desugarTypeDeclarations :: Array Declaration -> m (Array Declaration)
  desugarTypeDeclarations decls = case Array.uncons decls of
    Nothing -> pure []
    Just { head: TypeDeclaration (TypeDeclarationData td), tail: rest } ->
      case Array.uncons rest of
        Nothing -> throwError (errorMessage' (fst td.tydeclSourceAnn) (OrphanTypeDeclaration td.tydeclIdent))
        Just { head: d } -> do
          Tuple3 n nameKind val <- fromValueDeclaration td.tydeclIdent d
          let newDecl = ValueDeclaration (ValueDeclarationData
                { valdeclSourceAnn: td.tydeclSourceAnn
                , valdeclIdent: td.tydeclIdent
                , valdeclName: nameKind
                , valdeclBinders: []
                , valdeclExpression: [GuardedExpr [] (TypedValue true val td.tydeclType)]
                })
          desugarTypeDeclarations (Array.cons newDecl (Array.drop 1 rest))
    Just { head: ValueDeclaration (ValueDeclarationData vd), tail: rest } ->
      let t = everywhereOnValuesTopDownM pure go pure
          f' = traverse (\(GuardedExpr g e) -> GuardedExpr g <$> t.expr e)
      in do
          result' <- f' vd.valdeclExpression
          rest' <- desugarTypeDeclarations rest
          pure $ Array.cons (ValueDeclaration (ValueDeclarationData vd { valdeclExpression = result' })) rest'
      where
      go (Let w ds' val') = Let w <$> desugarTypeDeclarations ds' <*> pure val'
      go other = pure other
    Just { head: TypeInstanceDeclaration sa na ch idx nm deps cls args body, tail: rest } -> do
      body' <- traverseTypeInstanceBody desugarTypeDeclarations body
      rest' <- desugarTypeDeclarations rest
      pure $ Array.cons (TypeInstanceDeclaration sa na ch idx nm deps cls args body') rest'
    Just { head: d, tail: rest } -> do
      rest' <- desugarTypeDeclarations rest
      pure $ Array.cons d rest'

  fromValueDeclaration :: Ident -> Declaration -> m (Tuple3 Ident NameKind Expr)
  fromValueDeclaration name' (ValueDeclaration (ValueDeclarationData vd))
    | name' == vd.valdeclIdent
    , [GuardedExpr [] val] <- vd.valdeclExpression =
        pure (Tuple3 vd.valdeclIdent vd.valdeclName val)
  fromValueDeclaration name' d =
    throwError (errorMessage' (declSourceSpan d) (OrphanTypeDeclaration name'))

  checkKindDeclarations :: Array Declaration -> m Unit
  checkKindDeclarations decls = case Array.uncons decls of
    Nothing -> pure unit
    Just { head: KindDeclaration sa kindFor name' _, tail: rest } ->
      case Array.uncons rest of
        Just { head: d } ->
          if matchesKindDeclaration kindFor name' d
            then checkKindDeclarations rest
            else throwError (errorMessage' (fst sa) (OrphanKindDeclaration name'))
        Nothing -> throwError (errorMessage' (fst sa) (OrphanKindDeclaration name'))
    Just { tail: rest } -> checkKindDeclarations rest
    where
    matchesKindDeclaration kindFor name' (DataDeclaration _ dt name'' _ _) =
      kindFor == DataSig && name' == name'' ||
      kindFor == NewtypeSig && name' == name'' && dt == Newtype
    matchesKindDeclaration kindFor name' (TypeSynonymDeclaration _ name'' _ _) =
      kindFor == TypeSynonymSig && name' == name''
    matchesKindDeclaration kindFor name' (TypeClassDeclaration _ name'' _ _ _ _) =
      kindFor == ClassSig && name' == coerceProperName name''
    matchesKindDeclaration _ _ _ = false

  checkRoleDeclarations :: Maybe Declaration -> Array Declaration -> m Unit
  checkRoleDeclarations prev decls = case Array.uncons decls of
    Nothing -> pure unit
    Just { head: RoleDeclaration (RoleDeclarationData rd), tail: rest } ->
      case prev of
        Nothing ->
          throwError (errorMessage' (fst rd.rdeclSourceAnn) (OrphanRoleDeclaration rd.rdeclIdent))
        Just (RoleDeclaration (RoleDeclarationData prevRd))
          | prevRd.rdeclIdent == rd.rdeclIdent ->
              throwError (errorMessage' (fst rd.rdeclSourceAnn) (DuplicateRoleDeclaration rd.rdeclIdent))
        Just d ->
          if matchesRoleDeclaration rd.rdeclIdent d
            then if isSupportedForRole d
              then checkRoleDeclarations (Just (RoleDeclaration (RoleDeclarationData rd))) rest
              else throwError (errorMessage' (fst rd.rdeclSourceAnn) UnsupportedRoleDeclaration)
            else throwError (errorMessage' (fst rd.rdeclSourceAnn) (OrphanRoleDeclaration rd.rdeclIdent))
    Just { head: d, tail: rest } ->
      checkRoleDeclarations (Just d) rest
    where
    matchesRoleDeclaration name' (DataDeclaration _ _ pn _ _) = name' == pn
    matchesRoleDeclaration name' (ExternDataDeclaration _ pn _) = name' == pn
    matchesRoleDeclaration name' (TypeSynonymDeclaration _ pn _ _) = name' == pn
    matchesRoleDeclaration name' (TypeClassDeclaration _ pn _ _ _ _) = name' == coerceProperName pn
    matchesRoleDeclaration _ _ = false

    isSupportedForRole (DataDeclaration _ _ _ _ _) = true
    isSupportedForRole (ExternDataDeclaration _ _ _) = true
    isSupportedForRole _ = false

-- Simple 3-tuple helper (PureScript doesn't have built-in 3-tuples)
data Tuple3 a b c = Tuple3 a b c
