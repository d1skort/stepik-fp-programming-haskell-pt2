module LogT where

import Control.Applicative (liftA2)
import Control.Monad.Identity


data Logged a = Logged String a deriving (Eq, Show)


newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }


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
