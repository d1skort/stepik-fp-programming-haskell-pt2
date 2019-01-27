module FailCont where

import Control.Monad (ap, liftM)
import Control.Monad.Trans.Except


data ReadError = EmptyInput | NoParse String deriving (Eq, Show)


newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }


instance Functor (FailCont r e) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = liftM


instance Applicative (FailCont r e) where
  -- pure :: a -> f a
  pure = return

  -- <*> :: f (a -> b) -> f a -> f b
  (<*>) = ap


instance Monad (FailCont r e) where
  -- return :: a -> m a
  -- return :: a -> FailCont r e a
  return x = FailCont $ \ok _ -> ok x

  -- >>= :: m a -> (a -> m b) -> m b
  -- >>= :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
  -- >>= :: ((a -> r) -> (e -> r) -> r) -> (a -> ((b -> r) -> (e -> r) -> r)) -> (b -> r) -> (e -> r) -> r
  FailCont v >>= k = FailCont $ \ok err -> v (\a -> runFailCont (k a) ok err) err


tryRead :: Read a => String -> Except ReadError a
tryRead [] = throwE EmptyInput
tryRead s  = f $ reads s where
  f [(n, "")] = return n
  f [(n, xs)] = throwE $ NoParse s
  f []        = throwE $ NoParse s


toFailCont :: Except e a -> FailCont r e a
toFailCont e = case runExcept e of
  Left e  -> FailCont $ \_ err -> err e
  Right a -> return a


evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont c = runFailCont c Right Left


add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y


addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2
