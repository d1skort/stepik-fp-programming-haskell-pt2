{-# LANGUAGE TypeOperators #-}
module ApplicativeCmps where

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)


instance (Functor f, Functor g) => Functor (f |.| g) where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> f (g a) -> f (g b)
  fmap f (Cmps x) = Cmps $ fmap (fmap f) x


instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  -- pure :: a -> f a
  -- pure :: a -> (f (g a))
  pure = Cmps . pure . pure

  ff <*> fa = undefined


unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap getCmps . getCmps


unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap (fmap getCmps) . unCmps3
