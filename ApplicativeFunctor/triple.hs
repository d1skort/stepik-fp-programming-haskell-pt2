module Triple where


data Triple a = Tr a a a deriving (Show, Eq)


instance Functor Triple where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = (<*>) . pure


instance Applicative Triple where
  -- pure :: a -> f a
  pure a = Tr a a a

  -- (<*>) :: f (a -> b) -> f a -> f b
  (Tr f g h) <*> (Tr a b c) = Tr (f a) (g b) (h c)
