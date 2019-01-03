module OddC where

data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)


tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')


concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) c = Bi a b c
concat3OC (Un a) (Bi b b' b'') c = Bi a b (concat3OC (Un b') b'' c)
concat3OC (Bi a a' a'') b c = Bi a a' (concat3OC a'' b c)


concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi a b c) = concat3OC a b (concatOC c)


instance Functor OddC where
  fmap f (Un a) = Un (f a)
  fmap f (Bi a b c) = Bi (f a) (f b) (f <$> c)


instance Applicative OddC where
  pure = Un

  ff <*> fa = do
    f <- ff
    a <- fa
    return (f a)

instance Monad OddC where
  (Un a) >>= f = f a
  (Bi a b c) >>= f = concat3OC (f a) (f b) (c >>= f)
