module TreeSum where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Identity
import MonadReadError
import Data.Foldable
import Data.Monoid


data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show


instance Foldable Tree where
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini (Leaf a)     = f a ini
    foldr f ini (Fork l a r) = foldr f (f a (foldr f ini r)) l


treeSum :: Tree String -> Either ReadError Integer
treeSum = fmap getSum . execWriterT . traverse_ (tryRead >=> tell . Sum)