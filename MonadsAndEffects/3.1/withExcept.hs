module WithExcept where


newtype Except e a = Except { runExcept :: Either e a } deriving Show


except :: Either e a -> Except e a
except = Except


withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept _ (Except (Right a)) = except $ Right a
withExcept f (Except (Left e))  = except $ Left $ f e
