module OddC where


data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)


instance Functor OddC where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Un a) = Un $ f a
  fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)


instance Foldable OddC where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (Un a) = f a
  foldMap f (Bi a b c) = mappend (f a) (mappend (f b) (foldMap f c))

  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (Un a) = f a ini
  foldr f ini (Bi a b c) = f a (f b (foldr f ini c))


instance Traversable OddC where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Un a) = Un <$> f a
  traverse f (Bi a b c) = Bi <$> f a <*> f b <*> traverse f c

  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA (Un a) = Un <$> a
  sequenceA (Bi a b c) = Bi <$> a <*> b <*> sequenceA c
