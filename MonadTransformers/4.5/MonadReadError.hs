{-# LANGUAGE FlexibleContexts #-}
module MonadReadError where

import Control.Monad.Except


data ReadError = EmptyInput | NoParse String deriving Show

tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead [] = throwError EmptyInput
tryRead s  = f $ reads s where
  f [(n, "")] = return n
  f [(n, xs)] = throwError $ NoParse s
  f []        = throwError $ NoParse s
