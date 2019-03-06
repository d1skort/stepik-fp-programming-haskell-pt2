module ParseTree where

import TryRead
import Data.Foldable (traverse_)
import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)


data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Eq, Show)


instance Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (Leaf a) = f a ini
  foldr f ini (Fork l a r) = foldr f (f a (foldr f ini r)) l


treeSum :: Tree String -> (Maybe ReadError, Integer)
treeSum t = let (err, s) = runWriter . runExceptT $ traverse_ go t
            in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

    go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
    go s = tryRead s >>= (lift . tell . Sum)
