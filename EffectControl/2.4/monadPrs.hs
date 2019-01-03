module MonadPrs where


newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }


instance Functor PrsE where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f p = PrsE fun where
    fun s = fmap (\(a, s') -> (f a, s')) $ runPrsE p s


instance Applicative PrsE where
  -- pure :: a -> f a
  pure a = PrsE fun where
    fun s = Right (a, s)

  -- <*> :: f (a -> b) -> f a -> f b
  (PrsE ff) <*> p = PrsE fun where
    fun s = do
      (f, s') <- ff s
      runPrsE (f <$> p) s'


instance Monad PrsE where
  -- >>= :: m a -> (a -> m b) -> m b
  (PrsE p) >>= f = PrsE fun where
    fun s = do
      (a, s') <- p s
      runPrsE (f a) s'
