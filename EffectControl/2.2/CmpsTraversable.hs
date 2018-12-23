{-# LANGUAGE TypeOperators #-}
module CmpsTraversable where


infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)


instance (Functor f, Functor g) => Functor (f |.| g) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Cmps x) = Cmps $ fmap (fmap f) x


instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (Cmps x) = foldMap (foldMap f) x


instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Cmps x) = Cmps <$> traverse (traverse f) x

  -- sequenceA :: Applicative h => t (h a) -> h (t a)
  sequenceA (Cmps x) = Cmps <$> (sequenceA $ sequenceA <$> x)
