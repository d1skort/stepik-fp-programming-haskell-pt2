module Arrs where


newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }

newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }


instance Functor (Arr2 e1 e2) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f = Arr2 . fmap (fmap f) . getArr2


instance Functor (Arr3 e1 e2 e3) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f = Arr3 . fmap (fmap (fmap f)) . getArr3
