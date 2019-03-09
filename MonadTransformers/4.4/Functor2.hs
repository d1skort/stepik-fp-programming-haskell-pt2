{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Functor2 where


class Functor' c e | c -> e where
  fmap' :: (e -> e) -> c -> c


instance Functor' (Maybe e) e where
  fmap' _ Nothing  = Nothing
  fmap' f (Just x) = Just $ f x


instance Functor' [e] e where
  fmap' = map
