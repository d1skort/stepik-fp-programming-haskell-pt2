module TryRead where

import Control.Monad.Trans.Except


data ReadError = EmptyInput | NoParse String deriving (Eq, Show)


tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead [] = throwE EmptyInput
tryRead s = f $ reads s where
  f [(n, "")] = return n
  f [(n, xs)] = throwE $ NoParse s
  f []        = throwE $ NoParse s
