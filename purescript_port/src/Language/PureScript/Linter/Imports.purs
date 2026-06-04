module Language.PureScript.Linter.Imports
  ( UsedImports
  ) where

import Data.Map (Map)
import Language.PureScript.Names (ModuleName, Name, Qualified)

type UsedImports = Map ModuleName (Array (Qualified Name))
