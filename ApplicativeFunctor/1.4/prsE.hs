module PrsE where


newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }


satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
  f [] = Left "unexpected end of input"
  f (x:xs) | pr x      = Right (x, xs)
           | otherwise = Left $ "unexpected " ++ [x]


charE :: Char -> PrsE Char
charE c = satisfyE (== c)
