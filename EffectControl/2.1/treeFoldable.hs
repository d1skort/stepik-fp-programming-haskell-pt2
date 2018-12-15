module TreeFoldable where


data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)


tree = Branch (Branch (Branch Nil 0 Nil) 2 (Branch Nil 3 Nil)) 6 (Branch (Branch Nil 7 Nil) 8 (Branch Nil 9 Nil))
--       6
--     /   \
--    2     8
--  /  \   / \
-- 0   3  7   9

instance Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini Nil = ini
  foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l


instance Foldable Preorder where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))


instance Foldable Postorder where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)


instance Foldable Levelorder where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (LevelO Nil) = ini
  foldr f ini (LevelO tree) = foldl (\res a -> f a res) ini (fun (tree : []) []) where
    fun [] res = res
    fun ((Branch l x r) : ys) res = fun (append r (append l ys)) (x : res)
    append Nil xs = xs
    append tree xs = xs ++ [tree]
