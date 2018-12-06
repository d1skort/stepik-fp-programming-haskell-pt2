{-# LANGUAGE TypeOperators #-}
module SomeTypes where


infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)


type A = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (42, ('a', True))

b :: B t
b = Cmps (True, id, Right 42)

c :: C
c = Cmps f where
  f :: Bool -> Integer -> Integer
  f _ _ = 42
