module TransformError where

import Control.Monad.Trans.Except


data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex deriving (Eq, Show)

newtype SimpleError = Simple { getSimple :: String } deriving (Eq, Show)


instance Monoid SimpleError where
  -- mempty :: a
  mempty = Simple mempty

  -- mappend :: a -> a -> a
  mappend (Simple a) (Simple b) = Simple $ a `mappend` b

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge n) = Simple $ "[index (" ++ show n ++ ") is too large]"
lie2se ErrNegativeIndex     = Simple "[negative index]"
