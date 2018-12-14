module TreeFoldable where


data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
newtype PreOrder a = PreO (Tree a) deriving (Eq, Show)
newtype PostOrder a = PostO (Tree a) deriving (Eq, Show)
newtype LevelOder a = LevelO (Tree a) deriving (Eq, Show)


tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)


instance Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini Nil = ini
  foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l


instance Foldable PreOrder where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini r) l)


instance Foldable PostOrder where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l x r)) = undefined


instance Foldable LevelOder where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (LevelO Nil) = ini
  foldr f ini (LevelO (Branch l x r)) = undefined
