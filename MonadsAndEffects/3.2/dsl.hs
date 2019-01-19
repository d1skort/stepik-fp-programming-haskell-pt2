module Dsl where


decode :: (Integer -> c) -> c
decode f = f 0

as :: Integer -> (Integer -> r) -> r
as n c = c n

a :: Integer -> (Integer -> r) -> r
a = as

number = id

one :: Integer -> (Integer -> r) -> r
one n c = c (n + 1)

two :: Integer -> (Integer -> r) -> r
two n c = c (n + 2)

three :: Integer -> (Integer -> r) -> r
three n c = c (n + 3)

seventeen :: Integer -> (Integer -> r) -> r
seventeen n c = c (n + 17)

twenty :: Integer -> (Integer -> r) -> r
twenty n c = c (n + 20)

hundred :: Integer -> (Integer -> r) -> r
hundred n c = c (n * 100)

thousand :: Integer -> (Integer -> r) -> r
thousand n c = c (n * 1000)
