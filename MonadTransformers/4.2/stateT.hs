module StateT where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader


evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT st = fmap fst . runStateT st


execStateT :: Monad m => StateT s m a -> s -> m s
execStateT st = fmap snd . runStateT st


readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT r = StateT $ \st -> do
  a <- runReaderT r st
  return (a, st)
