module TripleTraversable where


data Triple a = Tr a a a deriving (Eq, Show)


instance Functor Triple where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Tr x y z) = (Tr (f x) (f y) (f z))


instance Foldable Triple where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (Tr x y z) = f x $ f y $ f z ini


instance Traversable Triple where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Tr x y z) = Tr <$> (f x) <*> (f y) <*> (f z)

  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA (Tr x y z) = Tr <$> x <*> y <*> z
