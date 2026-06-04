module Language.PureScript.CST.Traversals
  ( everythingOnSeparated
  ) where

import Prelude

import Data.Array (uncons) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Language.PureScript.CST.Types (Separated(..))

everythingOnSeparated :: forall a r. (r -> r -> r) -> (a -> r) -> Separated a -> r
everythingOnSeparated op k (Separated { sepHead: hd, sepTail: tl }) = go hd tl
  where
  go a arr = case Array.uncons arr of
    Nothing -> k a
    Just { head: b, tail: bs } -> k a `op` go (snd b) bs
