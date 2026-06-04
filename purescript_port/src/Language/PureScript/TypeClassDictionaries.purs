module Language.PureScript.TypeClassDictionaries
  ( TypeClassDictionaryInScope(..)
  , NamedDict
  , superclassName
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Names (ClassName, Ident, ProperName, Qualified, runProperName, disqualify)
import Language.PureScript.Types (SourceConstraint, SourceType)

data TypeClassDictionaryInScope v = TypeClassDictionaryInScope
  { tcdChain         :: Maybe ChainId
  , tcdIndex         :: Int
  , tcdValue         :: v
  , tcdPath          :: Array (Tuple (Qualified (ProperName ClassName)) Int)
  , tcdClassName     :: Qualified (ProperName ClassName)
  , tcdForAll        :: Array (Tuple String SourceType)
  , tcdInstanceKinds :: Array SourceType
  , tcdInstanceTypes :: Array SourceType
  , tcdDependencies  :: Maybe (Array SourceConstraint)
  , tcdDescription   :: Maybe SourceType
  }

derive instance functorTCD :: Functor TypeClassDictionaryInScope

instance showTCD :: Show v => Show (TypeClassDictionaryInScope v) where
  show (TypeClassDictionaryInScope d) =
    "(TypeClassDictionaryInScope { tcdClassName: " <> show d.tcdClassName <> " })"

type NamedDict = TypeClassDictionaryInScope (Qualified Ident)

superclassName :: Qualified (ProperName ClassName) -> Int -> String
superclassName pn index = runProperName (disqualify pn) <> show index
