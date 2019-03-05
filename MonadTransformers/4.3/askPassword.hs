module AskPassword where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)


newtype PwdError = PwdError String

instance Monoid PwdError where
  -- mempty :: a
  mempty = PwdError ""

  -- mappend :: a -> a -> a
  (PwdError x) `mappend` (PwdError y) = PwdError $ x `mappend` y

type PwdErrorMonad = ExceptT PwdError IO


askPassword :: PwdErrorMonad ()
askPassword = do
  liftIO $ putStrLn "Enter you new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."


getValidPassword :: PwdErrorMonad String
getValidPassword = do
  s <- liftIO getLine
  catchE (validatePassword s) reportError


reportError :: PwdError -> PwdErrorMonad String
reportError x@(PwdError e) = do
  liftIO $ putStrLn e
  throwE x


validatePassword :: String -> PwdErrorMonad String
validatePassword s | length s <= 8 = throwE $ PwdError "Incorrect input: password is too short!"
                   | not (any isNumber s) = throwE $ PwdError "Incorrect input: password must contain some digits!"
                   | not (any isPunctuation s) = throwE $ PwdError "Incorrect input: password must contain some punctuation!"
                   | otherwise = return s
