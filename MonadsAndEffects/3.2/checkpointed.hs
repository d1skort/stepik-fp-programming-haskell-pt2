module Checkpointed where

import Control.Monad.Cont


type Checkpointed a = (a -> Cont a a) -> Cont a a


addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2
  let x3 = x2 + 10
  checkpoint x3
  let x4 = x3 + 10
  return x4


runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed = undefined
