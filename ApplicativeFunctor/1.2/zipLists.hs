module ZipLists where

import Control.Applicative (ZipList(ZipList), getZipList)


(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = map

(>*<) :: [(a -> b)] -> [a] -> [b]
(>*<) fs = getZipList . (<*>) (ZipList fs) . ZipList
