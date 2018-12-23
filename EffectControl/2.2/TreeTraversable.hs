module TreeTraversable where


data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)


instance Functor Tree where
  -- fmap :: (a -> b) -> f a -> f b
  fmap _ Nil = Nil
  fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)


instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap _ Nil = mempty
  foldMap f (Branch l a r) = mappend (foldMap f r) (mappend (foldMap f l) (f a))


instance Traversable Tree where
  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA Nil = pure Nil
  sequenceA (Branch l a r) = Branch <$> (sequenceA l) <*> a <*> (sequenceA r)

  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse _ Nil = pure Nil
  traverse f (Branch l a r) = Branch <$> (traverse f l) <*> (f a) <*> (traverse f r)
