{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Coroutine where
import Control.Monad (ap, liftM)
import Control.Monad.Writer
import Control.Monad.Trans.Class


newtype CoroutineT m a = CoroutineT { runCoroutineT :: m (Either (CoroutineT m a) a) }


instance Monad m => Functor (CoroutineT m) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap = liftM


instance Monad m => Applicative (CoroutineT m) where
    -- pure :: a -> m a
    pure = return

    -- <*> :: m (a -> b) -> m a -> m b
    (<*>) = ap


instance Monad m => Monad (CoroutineT m) where
    -- return :: x -> m a
    return = CoroutineT . return . Right

    -- >>= :: m a -> (a -> m b) -> m b
    (CoroutineT ma) >>= k = CoroutineT $ do
        inner <- ma
        case inner of
            Right a -> runCoroutineT $ k a
            Left ca -> return $ Left $ ca >>= k


instance MonadTrans CoroutineT where
    -- lift :: Monad m => m a -> t m a
    lift ma = CoroutineT $ Right <$> ma


instance MonadWriter w m => MonadWriter w (CoroutineT m) where
    -- tell :: w -> m ()
    tell = lift . tell

    -- listen :: m a -> m (a, w)
    listen = undefined

    -- pass :: m (a, w -> w) -> m a
    pass = undefined


runCoroutines :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines c1 c2 = do
    innerC1 <- runCoroutineT c1
    case innerC1 of
        Right _ -> runSingle c2
        Left ca -> runCoroutines c2 ca


runSingle :: Monad m => CoroutineT m () -> m ()
runSingle (CoroutineT c) = do
    inner <- c
    case inner of
        Right _ -> return ()
        Left ca -> runSingle ca


yield :: Monad m => CoroutineT m ()
yield = CoroutineT $ return $ Left $ return ()


coroutine3, coroutine4 :: CoroutineT (Writer String) ()
coroutine3 = do
  tell "1"
  yield
  yield
  tell "2"

coroutine4 = do
  tell "a"
  yield
  tell "b"
  yield
  tell "c"
  yield
  tell "d"
  yield