module IgnoreBraces where


import Text.Parsec


ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces f g h = f *> h <* g
