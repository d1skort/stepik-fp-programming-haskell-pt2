module CountTree where

import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Monoid


data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Eq, Show)


numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1) where
  go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
  go (Leaf _) = do
    i <- get
    _ <- put $ i + 1
    _ <- lift $ tell 1
    return $ Leaf i
  go (Fork left _ right) = do
    l <- go left
    i <- get
    _ <- put $ i + 1
    r <- go right
    return (Fork l i r)
