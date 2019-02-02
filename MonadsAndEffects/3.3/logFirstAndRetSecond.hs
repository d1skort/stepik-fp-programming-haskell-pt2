module LogFirstAndRetSecond where


import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans (lift)
import Data.Char (toUpper)


logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
  e2 <- lift $ asks (map toUpper . head . tail)
  e1 <- lift $ asks head
  tell e1
  return e2
