{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module LogMTL where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Class


data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

type Logg = LoggT Identity


instance Monad m => Functor (LoggT m) where
  fmap = liftM


instance Monad m => Applicative (LoggT m) where
  pure = return

  (<*>) = ap


instance Monad m => Monad (LoggT m) where
  return a = LoggT $ return $ Logged "" a

  ma >>= k = LoggT $ do
    (Logged s a)  <- runLoggT ma
    (Logged s' b) <- runLoggT (k a)
    return $ Logged (s ++ s') b


instance MonadTrans LoggT where
  -- lift :: Monad m => m a -> t m a
  lift ma = LoggT $ ma >>= (return . Logged "")


instance MonadState s m => MonadState s (LoggT m) where
  -- get :: m s
  get = lift $ get

  -- put :: s -> m ()
  put = lift . put

  -- state :: (s -> (a, s)) -> m a
  state = lift . state


instance MonadReader r m => MonadReader r (LoggT m) where
  -- ask :: m r
  ask = lift ask

  -- local :: (r -> r) -> m a -> m a
  local f ma = LoggT $ local f (runLoggT ma)

  -- reader :: (r -> a) -> m a
  reader = lift . reader


class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a


instance Monad m => MonadLogg (LoggT m) where
  w2log = write2log
  logg = LoggT . return


instance MonadLogg m => MonadLogg (StateT s m) where
  -- w2log :: String -> m ()
  -- w2log :: String -> StateT s m ()
  w2log l = StateT $ \s -> fmap (\_ -> ((), s)) (w2log l)

  -- logg :: Logged a -> m a
  -- logg :: Logged a -> StateT s m a
  logg l = StateT $ \s -> fmap (\a -> (a, s)) (logg l)


instance MonadLogg m => MonadLogg (ReaderT r m) where
  -- w2log :: String -> m ()
  -- w2log :: String -> ReaderT r m ()
  w2log l = ReaderT $ \_ -> w2log l

  -- logg :: Logged a -> m a
  -- logg :: Logged a -> ReaderT r m a
  logg l = ReaderT $ \_ -> logg l


write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()


runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT


mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT


logSt' :: LoggT (State Integer) Integer
logSt' = do
  modify (+1)
  a <- get
  write2log $ show $ a * 10
  put 42
  return $ a * 100


logRdr :: LoggT (Reader [(Int,String)]) ()
logRdr = do
  Just x <- asks $ lookup 2
  write2log x
  Just y <- local ((3,"Jim"):) $ asks $ lookup 3
  write2log y


logSt'' :: LoggT (State Integer) Integer
logSt'' = do
  x <- logg $ Logged "BEGIN " 1
  modify (+x)
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100


rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer
rdrStLog = do
  x <- logg $ Logged "BEGIN " 1
  y <- ask
  modify (+ (x+y))
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100
