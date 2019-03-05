module Game where


import Control.Monad.Trans.Except


data Tile = Floor | Chasm | Snake deriving (Eq, Show)
data DeathReason = Fallen | Poisoned deriving (Eq, Show)

type Point = (Integer, Integer)
type GameMap = Point -> Tile


up :: Point -> Point
up (x, y) = (x, y - 1)


down :: Point -> Point
down (x, y) = (x, y + 1)


left :: Point -> Point
left (x, y) = (x - 1, y)


right :: Point -> Point
right (x, y) = (x + 1, y)


check :: GameMap -> Point -> Either DeathReason Point
check m p = f $ m p where
  f Floor = Right p
  f Chasm = Left Fallen
  f Snake = Left Poisoned


moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves _ 0 p = [Right p]
moves m i p = runExceptT $ do
  p' <- ExceptT $ [check m (up p), check m (down p), check m (left p), check m (right p)]
  ExceptT $ moves m (i - 1) p'


waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie dr m i p = foldr f 0 (moves m i p) where
  f (Left dr') c | dr' == dr = c + 1
                 | otherwise = c
  f _ c = c


map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm
