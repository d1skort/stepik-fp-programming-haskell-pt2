module PrsAlternative where


import Control.Applicative


newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }


instance Functor Prs where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f p = Prs $ \s -> (\(a, s') -> (f a, s')) <$> runPrs p s


instance Applicative Prs where
  -- pure :: a -> f a
  pure a = Prs $ \s -> Just (a, s)

  -- <*> :: f (a -> b) -> f a -> f b
  (Prs ff) <*> fa = Prs $ \s -> case ff s of
                                  Nothing -> Nothing
                                  Just (f, s') -> runPrs (f <$> fa) s'


instance Alternative Prs where
  -- empty :: f a
  empty = Prs $ const Nothing

  -- (<|>) :: f a -> f a -> f a
  (Prs p) <|> (Prs q) = Prs $ (<|>) <$> p <*> q
