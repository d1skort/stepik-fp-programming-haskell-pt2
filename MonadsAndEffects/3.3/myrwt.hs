module MyRWT where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Char (toUpper)


type MyRWT m = ReaderT [String] (WriterT String m)


runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt = runWriterT . runReaderT rwt


myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks


myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell


myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift


logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2


logFirstAndRetSecondSafe :: MyRWT Maybe String
logFirstAndRetSecondSafe = do
  xs <- myAsk
  case xs of
    (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
    _ -> myLift Nothing


myWithReader :: (r' -> r)
                -> ReaderT r (WriterT String m) a
                -> ReaderT r' (WriterT String m) a
myWithReader f r = ReaderT $ \e -> runReaderT r (f e)


veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  e1 <- myWithReader (filter $ even . length) logFirstAndRetSecondSafe
  myTell ","
  e2 <- myWithReader (filter $ odd . length) logFirstAndRetSecondSafe
  return (e1, e2)
