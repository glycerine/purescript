module Control.Monad.Supply.Class
  ( class MonadSupply
  , fresh
  , peek
  , freshIdent
  , freshIdent'
  ) where

import Prelude

import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Data.Maybe (Maybe(..))
import Language.PureScript.Names (Ident(..))

class Monad m <= MonadSupply m where
  fresh :: m Int
  peek  :: m Int

instance monadSupplyStateT :: MonadSupply m => MonadSupply (StateT s m) where
  fresh = lift fresh
  peek  = lift peek

instance monadSupplyWriterT :: (Monoid w, MonadSupply m) => MonadSupply (WriterT w m) where
  fresh = lift fresh
  peek  = lift peek

freshIdent :: forall m. MonadSupply m => String -> m Ident
freshIdent name = map (\n -> GenIdent (Just name) n) fresh

freshIdent' :: forall m. MonadSupply m => m Ident
freshIdent' = map (\n -> GenIdent Nothing n) fresh
