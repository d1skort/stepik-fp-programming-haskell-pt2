module PrsApplicative where


newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }


instance Functor Prs where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Prs p) = Prs fun where
    fun s = (\(a, s') -> (f a, s')) <$> p s



instance Applicative Prs where
  -- pure :: a -> f a
  pure a = Prs $ \s -> Just (a, s)

  -- (<*>) :: f (a -> b) -> f a -> f b
  (Prs ff) <*> fa = Prs $ \s -> case ff s of
                                  Nothing -> Nothing
                                  Just (f, s') -> runPrs (f <$> fa) s'


anyChr :: Prs Char
anyChr = Prs f where
  f [] = Nothing
  f (x:xs) = Just (x, xs)
