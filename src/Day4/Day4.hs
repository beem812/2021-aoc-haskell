{-# LANGUAGE OverloadedStrings #-}

module Day4.Day4 where

import Data.List

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

listIsFullyMarked :: Bool -> [Int] -> Bool
listIsFullyMarked winner list = winner || (-5) == sum list

checkWinner :: [[Int]] -> [[Int]] -> [[Int]]
checkWinner acc board =
  let columnsAndRows = transpose board ++ board
   in if foldl listIsFullyMarked False columnsAndRows then board else acc

checkAllWinners :: [[[Int]]] -> [[Int]] -> [[[Int]]]
checkAllWinners acc board =
  let columnsAndRows = transpose board ++ board
   in if foldl listIsFullyMarked False columnsAndRows then board : acc else acc

markRow :: Int -> [Int] -> [Int]
markRow pick = map (\x -> if x == pick then (-1) else x)

recordPickOnBoard :: Int -> [[Int]] -> [[Int]]
recordPickOnBoard pick = map (markRow pick)

recordPickOnBoards :: Int -> [[[Int]]] -> [[[Int]]]
recordPickOnBoards pick = map (recordPickOnBoard pick)

makePicks :: [Int] -> [[[Int]]] -> (Int, [[Int]])
makePicks [] boards = (0, [[27]])
makePicks (pick : nextPicks) boards =
  let updatedBoards = recordPickOnBoards pick boards
      possibleWinner = foldl checkWinner [] updatedBoards
   in if not $ null possibleWinner then (pick, possibleWinner) else makePicks nextPicks updatedBoards

isWinner :: [[[Int]]] -> [[Int]] -> Bool
isWinner [] board = False
isWinner winners board = board `elem` winners

notWinner :: [[[Int]]] -> [[Int]] -> Bool
notWinner winners board = not $ isWinner winners board

makePicksRemoveWinner :: [Int] -> [[[Int]]] -> (Int, [[Int]])
makePicksRemoveWinner [] boards = (0, [[27]])
makePicksRemoveWinner (pick : nextPicks) boards = do
  let updatedBoards = recordPickOnBoards pick boards
      possibleWinners = foldl checkAllWinners [] updatedBoards
  let winnersRemoved = filter (notWinner possibleWinners) updatedBoards
  if null winnersRemoved && 1 == length possibleWinners then (pick, head possibleWinners) else makePicksRemoveWinner nextPicks winnersRemoved

calculateScore :: Int -> [[Int]] -> Int
calculateScore pick board =
  let list = filter (> 0) $ concat board
   in pick * sum list

reportDay4 :: IO ()
reportDay4 = do
  fileLines <- getDay4File
  let choices = getChoices fileLines
      (pick, board) = makePicks choices $ parseBoards fileLines
      (lastPick, worstBoard) = makePicksRemoveWinner choices $ parseBoards fileLines
  let score = calculateScore lastPick worstBoard
  _ <- print $ show score
  return ()
