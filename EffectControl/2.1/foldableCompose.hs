{-# LANGUAGE TypeOperators #-}
module FoldableCompose where


infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)


instance (Foldable f, Foldable g) => Foldable (f |.| g) where
   -- foldr :: (a -> b -> b) -> b -> t a -> b
   foldr f ini (Cmps x) = foldr (\a b -> foldr f b a) ini x
