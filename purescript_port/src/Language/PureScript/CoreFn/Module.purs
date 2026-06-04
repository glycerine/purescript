-- | The CoreFn module representation.
module Language.PureScript.CoreFn.Module
  ( Module(..)
  ) where

import Prelude

import Data.Map (Map)
import Data.Tuple (Tuple)

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Expr (Bind)
import Language.PureScript.Names (Ident, ModuleName)

data Module a = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: Array Comment
  , moduleName :: ModuleName
  , modulePath :: String
  , moduleImports :: Array (Tuple a ModuleName)
  , moduleExports :: Array Ident
  , moduleReExports :: Map ModuleName (Array Ident)
  , moduleForeign :: Array Ident
  , moduleDecls :: Array (Bind a)
  }

derive instance functorModule :: Functor Module

instance showModule :: Show a => Show (Module a) where
  show (Module m) = "(Module " <> show m.moduleName <> ")"
