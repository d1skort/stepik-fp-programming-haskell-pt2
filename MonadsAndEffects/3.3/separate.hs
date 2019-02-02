module Separate where

import Control.Monad (when)
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)


separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate _ _ [] = return []
separate p1 p2 (x:xs) = do
  when (p1 x) (tell [x])
  when (p2 x) (lift $ tell [x])
  xs' <- separate p1 p2 xs
  if (p1 x || p2 x) then
    return xs'
  else
    return (x:xs')
