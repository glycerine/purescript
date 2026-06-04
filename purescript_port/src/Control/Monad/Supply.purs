module Control.Monad.Supply
  ( SupplyT(..)
  , Supply
  , runSupplyT
  , runSupply
  , evalSupplyT
  , evalSupply
  , freshIdent
  , freshIdent'
  ) where

import Prelude

import Control.Monad.Supply.Class (class MonadSupply, fresh)
import Data.Maybe (Maybe(..))
import Language.PureScript.Names (Ident(..))
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter, tell, listen, pass)
import Control.Monad.State (StateT, get, put, runStateT, evalStateT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)

newtype SupplyT m a = SupplyT (StateT Int m a)

type Supply = SupplyT Identity

instance functorSupplyT :: Functor m => Functor (SupplyT m) where
  map f (SupplyT s) = SupplyT (map f s)

instance applySupplyT :: Monad m => Apply (SupplyT m) where
  apply (SupplyT f) (SupplyT x) = SupplyT (apply f x)

instance applicativeSupplyT :: Monad m => Applicative (SupplyT m) where
  pure x = SupplyT (pure x)

instance bindSupplyT :: Monad m => Bind (SupplyT m) where
  bind (SupplyT s) f = SupplyT (bind s (\a -> let SupplyT s' = f a in s'))

instance monadSupplyT :: Monad m => Monad (SupplyT m)

instance monadTransSupplyT :: MonadTrans SupplyT where
  lift m = SupplyT (lift m)

instance monadSupplyInstance :: Monad m => MonadSupply (SupplyT m) where
  fresh = SupplyT do
    n <- get
    put (n + 1)
    pure n
  peek = SupplyT get

instance monadThrowSupplyT :: MonadThrow e m => MonadThrow e (SupplyT m) where
  throwError e = SupplyT (throwError e)

instance monadErrorSupplyT :: MonadError e m => MonadError e (SupplyT m) where
  catchError (SupplyT s) f = SupplyT (catchError s (\e -> let SupplyT s' = f e in s'))

instance monadTellSupplyT :: (Monad m, MonadTell w m) => MonadTell w (SupplyT m) where
  tell w = SupplyT (tell w)

instance monadWriterSupplyT :: (Monad m, MonadWriter w m) => MonadWriter w (SupplyT m) where
  listen (SupplyT s) = SupplyT (listen s)
  pass (SupplyT s)   = SupplyT (pass s)

runSupplyT :: forall m a. Int -> SupplyT m a -> m (Tuple a Int)
runSupplyT n (SupplyT s) = runStateT s n

runSupply :: forall a. Int -> Supply a -> Tuple a Int
runSupply n s = unwrap (runSupplyT n s)

evalSupplyT :: forall m a. Monad m => Int -> SupplyT m a -> m a
evalSupplyT n (SupplyT s) = evalStateT s n

evalSupply :: forall a. Int -> Supply a -> a
evalSupply n s = unwrap (evalSupplyT n s)

freshIdent :: forall m. MonadSupply m => String -> m Ident
freshIdent name = GenIdent (Just name) <$> fresh

freshIdent' :: forall m. MonadSupply m => m Ident
freshIdent' = GenIdent Nothing <$> fresh
