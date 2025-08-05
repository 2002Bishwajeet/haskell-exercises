import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Char (isDigit)
import System.Console.Haskeline (putHistory)
import Text.Read (readMaybe)

foo :: MaybeT IO String
foo = do
  liftIO $ putStrLn "Enter a String"
  input <- liftIO getLine
  if null input
    then MaybeT $ return Nothing
    else return input

bar :: String -> Maybe Int
bar s = if all isDigit s then Just (read s) else Nothing

getNumber :: MaybeT IO Int
getNumber = do
  liftIO $ putStrLn "Enter a number:"
  input <- liftIO getLine
  if all isDigit input
    then return $ read input
    else MaybeT $ return Nothing

getCredentials :: MaybeT IO (String, String)
getCredentials = do
  liftIO $ putStrLn "Username"
  username <- liftIO getLine
  if null username
    then MaybeT (return Nothing)
    else do
      liftIO $ putStrLn "Password"
      password <- liftIO getLine
      if null password
        then MaybeT (return Nothing)
        else return (username, password)

readInt :: MaybeT IO Int
readInt = do
  liftIO $ putStrLn "Enter Something bitch"
  input <- liftIO getLine
  MaybeT (return (readMaybe input))

readTwoNumbers :: MaybeT IO Int
readTwoNumbers = do
  liftIO $ putStrLn "Enter a number"
  input1 <- liftIO getLine
  liftIO $ putStrLn "Enter another number"
  input2 <- liftIO getLine
  n1 <- MaybeT (return (readMaybe input1))
  n2 <- MaybeT (return (readMaybe input2))
  return (n1 + n2)
