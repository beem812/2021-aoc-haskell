{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Day4.Day4Refactor where

import Data.List

data Five a = Five a a a a a deriving (Foldable, Functor, Traversable, Show)

newtype Board a = Board (Five (Five a)) deriving (Show, Functor, Foldable)

markBoard :: Eq a => a -> Board (Maybe a) -> Board (Maybe a)
markBoard pick = fmap erase
  where
    erase (Just y) | pick == y = Nothing
    erase y = y

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

getDay4File :: IO [String]
getDay4File = do
  string <- readFile "src/Day4/day4.txt"
  return $ lines string

getChoices :: [String] -> [Int]
getChoices fileLines = map read $ wordsWhen (== ',') $ head fileLines

lineToNumbers :: String -> [Int]
lineToNumbers str = map read $ words str

buildBoards :: [String] -> [[[Int]]]
buildBoards [] = [] :: [[[Int]]]
buildBoards fLines = map lineToNumbers (take 5 fLines) : buildBoards (drop 5 fLines)

parseBoards :: [String] -> [[[Int]]]
parseBoards fLines =
  let strippedLines = filter (/= "") $ drop 2 fLines
   in buildBoards strippedLines