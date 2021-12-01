{-# LANGUAGE NamedFieldPuns #-}

module Day1 (getDepthIncreaseCount, getWindowDepthIncreaseCount) where

getFile :: IO String
getFile = readFile "src/day1/day1.txt"

readInts :: String -> [Int]
readInts str = map read $ words str

readAndPrint :: IO ()
readAndPrint = do
  file <- getFile
  let ints = readInts file
  _ <- print ints
  return ()

data Accumulator = Accumulator
  { lastSeen :: Int,
    count :: Int
  }

countIncreases :: [Int] -> Int
countIncreases ints = count $ foldl accumulateDepthIncrease (Accumulator maxBound 0) ints

accumulateDepthIncrease :: Accumulator -> Int -> Accumulator
accumulateDepthIncrease Accumulator {lastSeen, count} next =
  Accumulator
    { count = if next > lastSeen then count + 1 else count,
      lastSeen = next
    }

getDepthIncreaseCount :: IO ()
getDepthIncreaseCount = do
  file <- getFile
  let depths = readInts file
  let increases = countIncreases depths
  _ <- print $ "total increases: " ++ show increases
  return ()

getWindows :: [Int] -> [Int]
getWindows lst =
  if length lst < 3
    then []
    else sum (take 3 lst) : getWindows (drop 1 lst)

getWindowDepthIncreaseCount :: IO ()
getWindowDepthIncreaseCount = do
  file <- getFile
  let depths = readInts file
  let windows = getWindows depths
  let increases = countIncreases windows
  _ <- print $ "total window increases: " ++ show increases
  return ()