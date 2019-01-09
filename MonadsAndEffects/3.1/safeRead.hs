module SafeRead where

import Control.Monad.Trans.Except


data ReadError = EmptyInput | NoParse String deriving Show

data SumError = SumError Int ReadError  deriving Show


tryRead :: Read a => String -> Except ReadError a
tryRead [] = throwE EmptyInput
tryRead s  = f $ reads s where
  f [(n, "")] = return n
  f [(n, xs)] = throwE $ NoParse s
  f []        = throwE $ NoParse s


trySum :: [String] -> Except SumError Integer
trySum = fmap sum . traverse (\(idx, s) -> withExcept (SumError idx) $ tryRead s) . zip [1..]
