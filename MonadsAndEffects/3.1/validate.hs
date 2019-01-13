module Validate where

import Control.Monad.Trans.Except


newtype Validate e a = Validate { getValidate :: Either [e] a }

data ReadError = EmptyInput | NoParse String deriving (Eq, Show)

data SumError = SumError Int ReadError deriving (Eq, Show)


instance Functor (Validate e) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = (<*>) . pure


instance Applicative (Validate e) where
  -- pure :: a -> f a
  pure = Validate . Right

  -- <*> :: f (a -> b) -> f a -> f b
  ff <*> fa = f (getValidate ff) (getValidate fa) where
    f (Left e) (Left e') = Validate $ Left $ e ++ e'
    f fun a              = Validate $ fun <*> a


collectE :: Except e a -> Validate e a
collectE e = f $ runExcept e where
  f (Left e)  = Validate $ Left [e]
  f (Right a) = Validate $ Right a


tryRead :: Read a => String -> Except ReadError a
tryRead [] = throwE EmptyInput
tryRead s  = f $ reads s where
  f [(n, "")] = return n
  f [(n, xs)] = throwE $ NoParse s
  f []        = throwE $ NoParse s


validateSum :: [String] -> Validate SumError Integer
validateSum = fmap sum . traverse (\(idx, s) -> collectE $ withExcept (SumError idx) $ tryRead s) . zip [1..]
