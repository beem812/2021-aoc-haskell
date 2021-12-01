module Lib where

getFile :: IO String
getFile = readFile "day1/day1.txt"

readInts :: String -> [Int]
readInts str = map read $ words str

readAndPrint = do
  file <- getFile
  let ints = readInts file
  _ <- print ints
  return ()