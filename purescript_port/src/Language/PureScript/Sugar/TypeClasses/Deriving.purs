-- | Stub: deriveInstances is elaborated during typechecking.
-- | Full implementation requires TypeChecker.checkNewtype (Phase 4).
module Language.PureScript.Sugar.TypeClasses.Deriving (deriveInstances) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Supply.Class (class MonadSupply)
import Language.PureScript.AST.Declarations (Declaration(..), Module(..))
import Language.PureScript.Errors (MultipleErrors)

deriveInstances
  :: forall m
   . MonadError MultipleErrors m
  => MonadSupply m
  => Module
  -> m Module
deriveInstances = pure
