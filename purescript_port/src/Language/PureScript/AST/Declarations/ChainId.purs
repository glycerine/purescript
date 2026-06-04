module Language.PureScript.AST.Declarations.ChainId
  ( ChainId
  , mkChainId
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Language.PureScript.AST.SourcePos (SourcePos(..))

newtype ChainId = ChainId (Tuple String SourcePos)

derive instance eqChainId :: Eq ChainId
derive instance ordChainId :: Ord ChainId

instance showChainId :: Show ChainId where
  show (ChainId (Tuple name pos)) = "(ChainId " <> show name <> " " <> show pos <> ")"

mkChainId :: String -> SourcePos -> ChainId
mkChainId fileName startPos = ChainId (Tuple fileName startPos)
