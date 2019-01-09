module SafeIndex where

import Control.Monad.Trans.Except


data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex deriving (Eq, Show)


(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) xs n = go xs n where
  go [] _    = throwE $ ErrIndexTooLarge n
  go (x:_) 0 = return x
  go (x:xs) n' | n' < 0 = throwE ErrNegativeIndex
               | otherwise = go xs (n' - 1)
