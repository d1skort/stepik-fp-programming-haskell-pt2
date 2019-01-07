module ParserEP where

import Control.Applicative


newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p = snd . runPrsEP p 0


satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP f where
  f n "" = (n + 1, Left $ "pos " ++ show (n + 1) ++ ": unexpected end of input")
  f n (c:cs) | pr c      = (n + 1, Right (c, cs))
             | otherwise = (n + 1, Left $ "pos " ++ show (n + 1) ++ ": unexpected " ++ [c])


instance Functor PrsEP where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f p = PrsEP fun where
    fun n s = fmap (fmap (\(a, s') -> (f a, s'))) $ runPrsEP p n s


instance Applicative PrsEP where
  -- pure :: a -> f a
  pure a = PrsEP f where
    f n s = (n, Right (a, s))

  -- <*> :: f (a -> b) -> f a -> f b
  pf <*> pa = PrsEP f where
    f n s = g $ runPrsEP pf n s
    g (n, Left s) = (n, Left s)
    g (n, Right (f, s')) = runPrsEP (f <$> pa) n s'


instance Alternative PrsEP where
  -- empty :: f a
  empty = PrsEP f where
    f n _ = (n, Left $ "pos " ++ show n ++ ": empty alternative")

  -- <|> :: f a -> f a -> f a
  pa <|> pb = PrsEP f where
    f n s = g (runPrsEP pa n s) (runPrsEP pb n s)
    g r@(_, Right _) (_, _) = r
    g (_, Left _) r@(_, Right _) = r
    g r1@(n, Left _) r2@(n', Left _) | n >= n' = r1
                                     | otherwise = r2
