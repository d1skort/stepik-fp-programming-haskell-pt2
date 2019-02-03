module MyRw where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Char (toUpper)


type MyRW = ReaderT [String] (Writer String)


myAsks :: ([String] -> a) -> MyRW a
myAsks = asks


myTell :: String -> MyRW ()
myTell = lift . tell


logFirstAndRetSecond :: MyRW String
logFirstAndRetSecond = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2


runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter (runReaderT rw e)
