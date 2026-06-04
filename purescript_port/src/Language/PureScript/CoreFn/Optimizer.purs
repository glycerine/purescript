-- | CoreFn optimization pass (stub).
module Language.PureScript.CoreFn.Optimizer
  ( optimizeCoreFn
  ) where

import Prelude

import Control.Monad.Supply.Class (class MonadSupply)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.CSE (optimizeCommonSubexpressions)
import Language.PureScript.CoreFn.Laziness (applyLazinessTransform)
import Language.PureScript.CoreFn.Module (Module(..))

-- | CoreFn optimization pass (applies laziness and CSE transforms).
optimizeCoreFn :: forall m. MonadSupply m => Module Ann -> m (Module Ann)
optimizeCoreFn (Module m) = do
  let mn = m.moduleName
  let decls = applyLazinessTransform mn m.moduleDecls
  decls' <- optimizeCommonSubexpressions mn decls
  pure (Module m { moduleDecls = decls' })
