module PrsAlternative where


import Control.Applicative
import Data.Char


newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }


instance Functor Prs where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f p = Prs $ \s -> (\(a, s') -> (f a, s')) <$> runPrs p s


instance Applicative Prs where
  -- pure :: a -> f a
  pure a = Prs $ \s -> Just (a, s)

  -- <*> :: f (a -> b) -> f a -> f b
  (Prs ff) <*> fa = Prs $ \s -> case ff s of
                                  Nothing -> Nothing
                                  Just (f, s') -> runPrs (f <$> fa) s'


instance Alternative Prs where
  -- empty :: f a
  empty = Prs $ const Nothing

  -- (<|>) :: f a -> f a -> f a
  (Prs p) <|> (Prs q) = Prs $ (<|>) <$> p <*> q


satisfy :: (Char -> Bool) -> Prs Char
satisfy pr = Prs fun where
  fun [] = Nothing
  fun (x:xs) | pr x = Just (x, xs)
             | otherwise = Nothing


char :: Char -> Prs Char
char ch = satisfy (== ch)


digit :: Prs Char
digit = satisfy isDigit


many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many p


nat :: Prs Int
nat = read <$> many1 digit


mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat
