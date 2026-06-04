-- | CoreImp optimizer (stub).
module Language.PureScript.CoreImp.Optimizer
  ( optimize
  ) where

import Prelude

import Control.Monad.Supply.Class (class MonadSupply)
import Language.PureScript.CoreImp.AST (AST)

-- | Optimize CoreImp AST (stub — returns AST unchanged).
optimize :: forall m. MonadSupply m => Array AST -> m (Array AST)
optimize = pure
