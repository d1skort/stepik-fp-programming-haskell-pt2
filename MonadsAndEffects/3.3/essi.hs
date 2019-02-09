module Essi where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except


tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n


type EsSi = ExceptT String (State Integer)


runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi = runState . runExceptT


go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go lower upper s = do
  n <- lift get
  let next = execState s n
  lift $ put next
  when (next >= upper) (throwE "Upper bound")
  when (next <= lower) (throwE "Lower bound")


go2 :: Integer -> Integer -> State Integer Integer -> EsSi ()
go2 lower upper s = do
  lift s
  n <- lift get
  when (n >= upper) (throwE "Upper bound")
  when (n <= lower) (throwE "Lower bound")
