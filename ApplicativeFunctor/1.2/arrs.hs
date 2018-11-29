module Arrs where


newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }

newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }


instance Functor (Arr2 e1 e2) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f fa = pure f <*> fa


instance Applicative (Arr2 e1 e2) where
  -- pure :: a -> f a
  pure a = Arr2 (\e1 e2 -> a)

  -- <*> :: f (a -> b) -> f a -> f b
  (Arr2 ff) <*> (Arr2 fa) = Arr2 (\e1 e2 -> ff e1 e2 $ fa e1 e2)


instance Functor (Arr3 e1 e2 e3) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f fa = pure f <*> fa


instance Applicative (Arr3 e1 e2 e3) where
  -- pure :: a -> f a
  pure a = Arr3 (\e1 e2 e3 -> a)

  -- <*> f (a -> b) -> f a -> f b
  (Arr3 ff) <*> (Arr3 fa) = Arr3 (\e1 e2 e3 -> ff e1 e2 e3 $ fa e1 e2 e3)
