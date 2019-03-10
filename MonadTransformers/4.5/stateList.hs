{-# LANGUAGE FlexibleContexts #-}
module StateList where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable


limited :: (MonadError Int m, MonadState s m) => (s -> Bool) -> [State s a] -> m [a]
limited p fs = traverse limit1 (zip [0..] fs) where
  limit1 (i, f) = do
    a <- state (runState f)
    stateIsBad <- gets (not . p)
    when stateIsBad $ throwError i
    return a


runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s


runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s


run1 :: ExceptT Int (State s) [a] -> s -> (Either Int [a], s)
run1 m s = runState (runExceptT m) s


run2 :: StateT s (Except Int) [a] -> s -> Either Int ([a], s)
run2 m s = runExcept $ runStateT m s
