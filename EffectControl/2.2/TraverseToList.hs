module TraverseToList where


import Control.Applicative


traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (liftA2 (:) . f) (pure [])
