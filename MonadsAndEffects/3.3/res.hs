module Res where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except


tickCollatz :: StateT Integer IO Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n


type RiiEsSiT m = ReaderT (Integer, Integer) (ExceptT String (StateT Integer m))


runRiiEsSiT :: ReaderT (Integer, Integer) (ExceptT String (StateT Integer m)) a
                -> (Integer, Integer)
                -> Integer
                -> m (Either String a, Integer)
runRiiEsSiT m e = runStateT $ runExceptT $ runReaderT m e


go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go s = do
  (lower, upper) <- ask
  lift $ lift s
  n <- lift $ lift get
  when (n >= upper) (lift $ throwE "Upper bound")
  when (n <= lower) (lift $ throwE "Lower bound")
