module MkEndo where


import Data.Monoid


mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo
