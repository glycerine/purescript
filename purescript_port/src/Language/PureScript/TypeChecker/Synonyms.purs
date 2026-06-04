module Language.PureScript.TypeChecker.Synonyms
  ( SynonymMap
  , KindMap
  , replaceAllTypeSynonyms
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State.Class (class MonadState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Environment (Environment(..), TypeKind)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), errorMessage')
import Language.PureScript.Names (ProperName, Qualified, TypeName)
import Language.PureScript.TypeChecker.Monad (CheckState, getEnv)
import Language.PureScript.Types
  ( SourceType
  , Type(..)
  , completeBinderList
  , everywhereOnTypesTopDownM
  , getAnnForType
  , replaceAllTypeVars
  )

type SynonymMap = Map (Qualified (ProperName TypeName)) (Tuple (Array (Tuple String (Maybe SourceType))) SourceType)

type KindMap = Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)

replaceAllTypeSynonyms'
  :: SynonymMap
  -> KindMap
  -> SourceType
  -> Either MultipleErrors SourceType
replaceAllTypeSynonyms' syns kinds = everywhereOnTypesTopDownM try
  where
  try :: SourceType -> Either MultipleErrors SourceType
  try t = fromMaybe t <$> go (fst (getAnnForType t)) 0 [] [] t

  go :: SourceSpan -> Int -> Array SourceType -> Array SourceType -> SourceType -> Either MultipleErrors (Maybe SourceType)
  go ss c kargs args (TypeConstructor _ ctor) =
    case Map.lookup ctor syns of
      Just (Tuple synArgs body) | c == Array.length synArgs ->
        let kindArgs = lookupKindArgs ctor
        in if Array.length kargs == Array.length kindArgs
           then let repl = replaceAllTypeVars
                             (Array.zipWith (\(Tuple n _) a -> Tuple n a) synArgs args
                              <> Array.zip kindArgs kargs) body
                in map Just (try repl)
           else pure Nothing
      Just (Tuple synArgs _) | Array.length synArgs > c ->
        throwError (errorMessage' ss (PartiallyAppliedSynonym ctor))
      _ -> pure Nothing
  go ss c kargs args (TypeApp _ f arg) = go ss (c + 1) kargs (Array.cons arg args) f
  go ss c kargs args (KindApp _ f arg) = go ss c (Array.cons arg kargs) args f
  go _ _ _ _ _ = pure Nothing

  lookupKindArgs :: Qualified (ProperName TypeName) -> Array String
  lookupKindArgs ctor = fromMaybe [] do
    Tuple kindTy _ <- Map.lookup ctor kinds
    Tuple binders _ <- completeBinderList kindTy
    pure (map (\(Tuple _ (Tuple n _)) -> n) binders)

replaceAllTypeSynonyms
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => SourceType
  -> m SourceType
replaceAllTypeSynonyms d = do
  env <- getEnv
  let Environment e = env
  case replaceAllTypeSynonyms' e.typeSynonyms e.types d of
    Left err -> throwError err
    Right t  -> pure t
