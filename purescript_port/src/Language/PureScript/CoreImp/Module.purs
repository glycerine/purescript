-- | CoreImp module representation.
module Language.PureScript.CoreImp.Module
  ( Module(..)
  , Import(..)
  , Export(..)
  ) where

import Prelude

import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)

import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreImp.AST (AST)
import Language.PureScript.PSString (PSString)

data Module = Module
  { modHeader :: Array Comment
  , modImports :: Array Import
  , modBody :: Array AST
  , modExports :: Array Export
  }

data Import = Import String PSString

data Export = Export (NonEmptyList String) (Maybe PSString)
