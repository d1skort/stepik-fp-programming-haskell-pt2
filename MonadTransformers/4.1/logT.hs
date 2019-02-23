module LogT where

import Control.Applicative (liftA2)
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

type Logg = LoggT Identity


instance Functor m => Functor (LoggT m) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f m = LoggT $ fmap (\(Logged s a) -> Logged s (f a)) (runLoggT m)


instance Applicative m => Applicative (LoggT m) where
  -- pure :: a -> m a
  pure = LoggT . pure . Logged ""

  -- <*> :: m (a -> b) -> m a -> m b
  mf <*> ma = LoggT $ liftA2 f (runLoggT mf) (runLoggT ma) where
    f (Logged s g) (Logged s' a) = Logged (s ++ s') (g a)


instance Monad m => Monad (LoggT m) where
  -- >>= :: m a -> (a -> m b) -> m b
  ma >>= k = LoggT $ do
    (Logged s a)  <- runLoggT ma
    (Logged s' b) <- runLoggT (k a)
    return $ Logged (s ++ s') b

  -- fail :: String -> m a
  fail = LoggT . fail


instance MonadTrans LoggT where
  -- lift :: Monad m => m a -> t m a
  lift ma = LoggT $ ma >>= (return . Logged "")


logTst :: LoggT Identity Integer
logTst = do
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42


write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()


runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT


logTst' :: Logg Integer
logTst' = do
  write2log "AAA"
  write2log "BBB"
  return 42


stLog :: StateT Integer Logg Integer
stLog = do
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100


logSt :: LoggT (State Integer) Integer
logSt = do
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100
