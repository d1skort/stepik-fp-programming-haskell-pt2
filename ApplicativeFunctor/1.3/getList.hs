module GetList where


import Text.Parsec


getList :: Parsec String u [String]
getList = (many1 digit) `sepBy` (char ';')
