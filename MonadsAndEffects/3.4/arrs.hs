module Arrs where

import Control.Monad.Trans.Class


newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }


arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T $ \e1 e2 -> return $ f e1 e2

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T $ \e1 e2 e3 -> return $ f e1 e2 e3


instance Functor m => Functor (Arr2T e1 e2 m) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f arr2 = Arr2T $ fmap (fmap (fmap f)) $ getArr2T arr2


instance Functor m => Functor (Arr3T e1 e2 e3 m) where
  fmap f arr3 = Arr3T $ fmap (fmap (fmap (fmap f))) $ getArr3T arr3


instance Applicative m => Applicative (Arr2T e1 e2 m) where
  -- pure :: a -> f a
  pure a = Arr2T $ \_ _ -> pure a

  -- <*> :: f (a -> b) -> f a -> f b
  ff <*> fa = Arr2T $ \e1 e2 -> getArr2T ff e1 e2 <*> getArr2T fa e1 e2


instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
  pure a = Arr3T $ \_ _ _ -> pure a

  ff <*> fa = Arr3T $ \e1 e2 e3 -> getArr3T ff e1 e2 e3 <*> getArr3T fa e1 e2 e3


instance Monad m => Monad (Arr2T e1 e2 m) where
  -- >>= :: m a -> (a -> m b) -> m b
  ma >>= k = Arr2T $ \e1 e2 -> do
    a <- getArr2T ma e1 e2
    getArr2T (k a) e1 e2


instance Monad m => Monad (Arr3T e1 e2 e3 m) where
  ma >>= k = Arr3T $ \e1 e2 e3 -> do
    a <- getArr3T ma e1 e2 e3
    getArr3T (k a) e1 e2 e3

  -- fail :: String -> m a
  fail err = Arr3T $ \_ _ _ -> fail err


instance MonadTrans (Arr2T e1 e2) where
  -- lift :: Monad m => m a -> t m a
  lift m = Arr2T $ \_ _ -> m


asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \e1 e2 -> return $ f e1 e2
