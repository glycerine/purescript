-- | Common subexpression elimination for CoreFn (stub).
module Language.PureScript.CoreFn.CSE
  ( optimizeCommonSubexpressions
  ) where

import Prelude

import Control.Monad.Supply.Class (class MonadSupply)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind)
import Language.PureScript.Names (ModuleName)

-- | Optimize common subexpressions in CoreFn bindings (stub — returns bindings unchanged).
optimizeCommonSubexpressions
  :: forall m
   . MonadSupply m
  => ModuleName
  -> Array (Bind Ann)
  -> m (Array (Bind Ann))
optimizeCommonSubexpressions _mn = pure
