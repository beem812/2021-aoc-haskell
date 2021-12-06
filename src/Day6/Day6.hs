module Day6.Day6 where

import Data.IntMap (insertWith)
import qualified Data.Map as M
import Day4.Day4 (wordsWhen)

getDay6File :: IO [Int]
getDay6File = do
  string <- readFile "src/Day6/day6.txt"
  return $ map read $ wordsWhen (== ',') string

buildFishMap :: M.Map Int Int -> [Int] -> M.Map Int Int
buildFishMap = foldl (\m x -> M.insertWith (+) x 1 m)

advanceFishGroup :: M.Map Int Int -> Int -> Int -> M.Map Int Int
advanceFishGroup m 0 value = M.insert 8 value $ M.insertWith (+) 6 value m
advanceFishGroup m period value = M.insertWith (+) (period - 1) value m

advanceGestation :: M.Map Int Int -> M.Map Int Int
advanceGestation = M.foldlWithKey advanceFishGroup M.empty

advanceDays :: Int -> M.Map Int Int -> M.Map Int Int
advanceDays 0 m = m
advanceDays days m = advanceDays (days - 1) $ advanceGestation m

countFish :: M.Map Int Int -> Int
countFish = sum

reportDay6 :: IO ()
reportDay6 = do
  fish <- getDay6File
  let fishMap = buildFishMap M.empty fish
      newFish = advanceDays 256 fishMap
      fishCount = length newFish
  _ <- print $ countFish newFish
  return ()