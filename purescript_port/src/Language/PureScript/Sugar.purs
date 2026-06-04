-- | Desugaring passes orchestrator.
module Language.PureScript.Sugar
  ( module Language.PureScript.Sugar.BindingGroups
  , module Language.PureScript.Sugar.CaseDeclarations
  , module Language.PureScript.Sugar.DoNotation
  , module Language.PureScript.Sugar.AdoNotation
  , module Language.PureScript.Sugar.LetPattern
  , module Language.PureScript.Sugar.Names
  , module Language.PureScript.Sugar.ObjectWildcards
  , module Language.PureScript.Sugar.Operators
  , module Language.PureScript.Sugar.TypeClasses
  , module Language.PureScript.Sugar.TypeClasses.Deriving
  , module Language.PureScript.Sugar.TypeDeclarations
  , module Language.PureScript.Sugar.Names.Env
  , desugar
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Supply.Class (class MonadSupply)
import Control.Monad.Writer.Class (class MonadWriter)
import Data.Tuple (Tuple)

import Language.PureScript.AST.Declarations (Module)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Linter.Imports (UsedImports)
import Language.PureScript.Sugar.AdoNotation
import Language.PureScript.Sugar.BindingGroups
import Language.PureScript.Sugar.CaseDeclarations
import Language.PureScript.Sugar.DoNotation
import Language.PureScript.Sugar.LetPattern
import Language.PureScript.Sugar.Names
import Language.PureScript.Sugar.Names.Env
import Language.PureScript.Sugar.ObjectWildcards
import Language.PureScript.Sugar.Operators
import Language.PureScript.Sugar.TypeClasses
import Language.PureScript.Sugar.TypeClasses.Deriving
import Language.PureScript.Sugar.TypeDeclarations

-- | The full desugaring pipeline.
-- | Passes proceed in order:
-- | 1. desugarSignedLiterals (pure)
-- | 2. desugarObjectConstructors
-- | 3. desugarDoModule
-- | 4. desugarAdoModule
-- | 5. desugarLetPatternModule (pure)
-- | 6. desugarCasesModule
-- | 7. desugarTypeDeclarationsModule
-- | 8. desugarImports (name resolution)
-- | 9. rebracket externs
-- | 10. checkFixityExports
-- | 11. deriveInstances
-- | 12. desugarTypeClasses externs
-- | 13. createBindingGroupsModule
desugar
  :: forall m
   . MonadSupply m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => MonadState (Tuple Env UsedImports) m
  => Array ExternsFile
  -> Module
  -> m Module
desugar externs m = do
  m1  <- desugarObjectConstructors (desugarSignedLiterals m)
  m2  <- desugarDoModule m1
  m3  <- desugarAdoModule m2
  m4  <- desugarCasesModule (desugarLetPatternModule m3)
  m5  <- desugarTypeDeclarationsModule m4
  m6  <- desugarImports m5
  m7  <- rebracket externs m6
  m8  <- checkFixityExports m7
  m9  <- deriveInstances m8
  m10 <- desugarTypeClasses externs m9
  createBindingGroupsModule m10
