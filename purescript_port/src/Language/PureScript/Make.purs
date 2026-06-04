-- | Top-level Make API — wires together the full compiler pipeline.
module Language.PureScript.Make
  ( make
  , rebuildModule
  , rebuildModuleWithCoreFn
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.State (runStateT)
import Control.Monad.Writer.Class (class MonadWriter)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.Declarations (Module(..), getModuleName, importPrim)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Externs
  ( ExternsFile
  , applyExternsFileToEnvironment
  , moduleToExternsFile
  )
import Language.PureScript.CoreFn.Module (Module) as CoreFn
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.Renamer (renameInModule)
import Language.PureScript.Sugar
  ( Env
  , collapseBindingGroups
  , createBindingGroups
  , desugar
  , desugarCaseGuards
  , externsEnv
  , primEnv
  )
import Language.PureScript.TypeChecker (typeCheckModule)
import Language.PureScript.TypeChecker.Monad (CheckState(..), emptyCheckState)
import Language.PureScript.CoreFn.Desugar (moduleToCoreFn)
import Language.PureScript.CoreFn.Optimizer (optimizeCoreFn)
import Language.PureScript.Linter.Imports (UsedImports)
import Control.Monad.Supply (runSupplyT)

-- | Rebuild a single module against a given set of externs.
rebuildModule
  :: forall m
   . MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Array ExternsFile
  -> Module
  -> m ExternsFile
rebuildModule externs m = do
  let env = foldl (\e ef -> applyExternsFileToEnvironment ef e) initEnvironment externs
      withPrim = importPrim m
      mn = getModuleName m
  -- Build the name resolution Env from externs
  exEnv <- foldl
    (\accM ef -> do
      acc <- accM
      externsEnv acc ef)
    (pure primEnv)
    externs
  -- Run everything inside a shared SupplyT for fresh name supply
  Tuple (Tuple checked checkSt) _nextVar <- runSupplyT 0 do
    -- Stage 1: Desugar (name resolution, operators, TC class elaboration, etc.)
    Tuple desugared _ <- runStateT
      (desugar externs withPrim)
      (Tuple exEnv (Map.empty :: UsedImports))
    let modulesExports = map snd exEnv
    -- Stage 2: Type check
    Tuple checked checkSt <- runStateT
      (typeCheckModule modulesExports desugared)
      (emptyCheckState env)
    pure (Tuple checked checkSt)
  let CheckState cs = checkSt
      checkEnv = cs.checkEnv
      Module ss coms mn' decls exps = checked
  -- Stage 3: Desugar case guards (after type checking)
  Tuple deguarded _ <- runSupplyT 0 (desugarCaseGuards decls)
  -- Stage 4: Re-group binding groups
  regrouped <- createBindingGroups mn (collapseBindingGroups deguarded)
  let mod' = Module ss coms mn' regrouped exps
  -- Stage 5: CoreFn conversion
  let corefn = moduleToCoreFn checkEnv mod'
  -- Stage 6: Optimize CoreFn
  Tuple optimized _ <- runSupplyT 0 (optimizeCoreFn corefn)
  -- Stage 7: Rename
  let Tuple renamedIdents renamed = renameInModule optimized
  -- Stage 8: Build externs
  pure (moduleToExternsFile mod' checkEnv renamedIdents)

-- | Like rebuildModule, but also returns the optimized CoreFn module for inspection/comparison.
rebuildModuleWithCoreFn
  :: forall m
   . MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Array ExternsFile
  -> Module
  -> m (Tuple ExternsFile (CoreFn.Module Ann))
rebuildModuleWithCoreFn externs m = do
  let env = foldl (\e ef -> applyExternsFileToEnvironment ef e) initEnvironment externs
      withPrim = importPrim m
      mn = getModuleName m
  exEnv <- foldl
    (\accM ef -> do
      acc <- accM
      externsEnv acc ef)
    (pure primEnv)
    externs
  Tuple (Tuple checked checkSt) _nextVar <- runSupplyT 0 do
    Tuple desugared _ <- runStateT
      (desugar externs withPrim)
      (Tuple exEnv (Map.empty :: UsedImports))
    let modulesExports = map snd exEnv
    Tuple checked checkSt <- runStateT
      (typeCheckModule modulesExports desugared)
      (emptyCheckState env)
    pure (Tuple checked checkSt)
  let CheckState cs = checkSt
      checkEnv = cs.checkEnv
      Module ss coms mn' decls exps = checked
  Tuple deguarded _ <- runSupplyT 0 (desugarCaseGuards decls)
  regrouped <- createBindingGroups mn (collapseBindingGroups deguarded)
  let mod' = Module ss coms mn' regrouped exps
      corefn = moduleToCoreFn checkEnv mod'
  Tuple optimized _ <- runSupplyT 0 (optimizeCoreFn corefn)
  let Tuple renamedIdents renamed = renameInModule optimized
  pure (Tuple (moduleToExternsFile mod' checkEnv renamedIdents) renamed)

-- | Build a set of modules in dependency order.
make
  :: forall m
   . MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Array Module
  -> m (Array ExternsFile)
make = Array.foldM
  (\externs m -> do
    ef <- rebuildModule externs m
    pure (Array.snoc externs ef))
  []
