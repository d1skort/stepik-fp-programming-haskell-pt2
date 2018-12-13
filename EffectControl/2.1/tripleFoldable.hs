module TripleFoldable where


data Triple a = Tr a a a deriving (Eq, Show)


instance Foldable Triple where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (Tr a b c) = f a $ f b $ f c ini

  -- foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f ini (Tr a b c) = f (f (f ini a) b) c
