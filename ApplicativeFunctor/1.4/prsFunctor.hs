module PrsFunctor where


newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }


instance Functor Prs where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f p = Prs fun where
    fun s = (\(a, s') -> (f a, s')) <$> runPrs p s


anyChr :: Prs Char
anyChr = Prs f where
  f [] = Nothing
  f (x:xs) = Just (x, xs)
