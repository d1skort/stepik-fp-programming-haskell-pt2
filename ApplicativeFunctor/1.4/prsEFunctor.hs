module PrsEFunctor where


newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }


instance Functor PrsE where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f prs = PrsE fun where
    fun s = fmap (\(a, rest) -> (f a, rest)) $ runPrsE prs s


instance Applicative PrsE where
  -- pure :: a -> f a
  pure x = PrsE $ \s -> Right (x, s)

  -- <*> :: f (a -> b) -> f a -> f b
  (PrsE ff) <*> prs = PrsE fun where
    fun s = case ff s of
              Left err      -> Left err
              Right (f, s2) -> runPrsE (fmap f prs) s2
