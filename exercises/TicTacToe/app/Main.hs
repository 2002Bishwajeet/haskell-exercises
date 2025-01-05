{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (intercalate)
import Text.Read (readMaybe)

data Token = X | O | E deriving (Show, Eq)

type Board = [[Token]]

emptyBoard :: Int -> Board
emptyBoard 0 = [[]]
-- emptyBoard n = replicate n (replicate n E)
emptyBoard n = replicate n $ replicate n E -- if you add $ ( ) is redudant since it does composititon

isFull :: Board -> Bool
isFull = all (notElem E)

showToken :: Token -> String
showToken X = "X"
showToken O = "O"
showToken E = "_"

showRow :: [Token] -> String
-- intercalate :: [a] -> [[a]] -> [a]
showRow ts = intercalate " " (map showToken ts)

showBoard :: Board -> String
showBoard n = intercalate "\n" (map showRow n)

updateListM :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
updateListM = undefined

mapAtM :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
mapAtM _ _ [] = Nothing
mapAtM 0 f (x : xs) = fmap (: xs) (f x)
mapAtM i f (x : xs) = fmap (x :) (mapAtM (i - 1) f xs)

setToken :: (Int, Int) -> Token -> Board -> Maybe Board
setToken (x, y) t = mapAtM y $ mapAtM x $ \case
  E -> Just t
  _ -> Nothing

winner :: Board -> Token
winner = undefined

main :: IO ()
main = run (emptyBoard 3) X
  where
    run :: Board -> Token -> IO ()
    run b t = do
      putStrLn $ showBoard b ++ "\n"
      putStr $ "Where to place " ++ show t ++ "? "
      line <- getLine
      case readMaybe line of
        Nothing -> do
          putStrLn "Invalid input... Try again!"
          run b t
        Just pos -> do
          putStrLn ""
          case setToken pos t b of
            Nothing -> do
              putStrLn "Invalid position... Try again!"
              run b t
            Just b' -> do
              case winner b' of
                E | isFull b' -> do
                  putStrLn $ showBoard b' ++ "\n"
                  putStrLn "Game over. Everyone looses."
                E -> do
                  run b' (flipTok t)
                t' -> do
                  putStrLn $ showBoard b' ++ "\n"
                  putStrLn $ show t' ++ " is the winner!"

    flipTok :: Token -> Token
    flipTok X = O
    flipTok O = X
    flipTok E = E
