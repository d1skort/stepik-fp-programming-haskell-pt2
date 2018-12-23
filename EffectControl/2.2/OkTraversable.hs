module OkTraversable where


data Result a = Ok a | Error String deriving (Eq, Show)


instance Functor Result where
  -- fmap :: (a -> b) -> f a -> f b
  fmap _ (Error s) = Error s
  fmap f (Ok a) = Ok (f a)


instance Foldable Result where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr _ ini (Error s) = ini
  foldr f ini (Ok a) = f a ini

  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap _ (Error _) = mempty
  foldMap f (Ok a) = f a

instance Traversable Result where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse _ (Error s) = pure (Error s)
  traverse f (Ok a) = Ok <$> f a

  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA (Error s) = pure (Error s)
  sequenceA (Ok a) = Ok <$> a
