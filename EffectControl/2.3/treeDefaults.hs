module TreeDefaults where

import Data.Traversable (foldMapDefault)


data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)


instance Functor Tree where
  -- fmap :: (a -> b) -> f a -> f b
  fmap _ Nil = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)


instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap = foldMapDefault


instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse _ Nil = pure Nil
  traverse f (Branch l x r) = fun <$> (traverse f l) <*> (traverse f r) <*> (f x) where
    fun ll rr xx = Branch ll xx rr
