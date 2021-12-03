module Day3.Day3 where

import Data.Bits (Bits (xor))
import Data.Char (digitToInt)

type Index = Int

getDay3File :: IO String
getDay3File = readFile "src/Day3/day3.txt"

getNumberStrings :: IO [String]
getNumberStrings = do
  words <$> getDay3File

numberStringsToInts :: [String] -> [[Int]]
numberStringsToInts = map (map digitToInt)

accumulateOccurance :: Int -> Int -> Int
accumulateOccurance i 0 = i - 1
accumulateOccurance i 1 = i + 1
accumulateOccurance i _ = i

recordBitFrequencyFull :: [Int] -> [Int] -> [Int]
recordBitFrequencyFull = zipWith accumulateOccurance

recordBitFrequencyByIndex :: Index -> Int -> [Int] -> Int
recordBitFrequencyByIndex idx acc [] = acc
recordBitFrequencyByIndex idx acc list = accumulateOccurance acc (list !! idx)

toMostCommonBit :: Int -> Int
toMostCommonBit x = if x >= 0 then 1 else 0

toLeastCommonBit :: Int -> Int
toLeastCommonBit x = if x >= 0 then 0 else 1

getGammaValueBin :: [Int] -> [Int]
getGammaValueBin = map toMostCommonBit

convert :: [Int] -> Int
-- Array must be ordered such that the first digit is the least significant
convert [] = 0
convert (x : xs) = x + 2 * convert xs

filterOnDigit :: Index -> Int -> [Int] -> Bool
filterOnDigit i target list = list !! i == target

sharedPowerRatingCalc :: (Int -> Int) -> Index -> [[Int]] -> [Int]
sharedPowerRatingCalc _ _ [] = []
sharedPowerRatingCalc _ _ [x] = x
sharedPowerRatingCalc freqToBitConverter idx xs =
  let frequentDigit = freqToBitConverter $ foldl (recordBitFrequencyByIndex idx) 0 xs
      filtered = filter (filterOnDigit idx frequentDigit) xs
   in sharedPowerRatingCalc freqToBitConverter (idx + 1) filtered

oxygenRating :: [[Int]] -> [Int]
oxygenRating = sharedPowerRatingCalc toMostCommonBit 0

scrubberRating :: [[Int]] -> [Int]
scrubberRating = sharedPowerRatingCalc toLeastCommonBit 0

reportDay3Part1 :: IO ()
reportDay3Part1 = do
  binInts <- numberStringsToInts <$> getNumberStrings
  let strLength = length $ head binInts
      freqArr = foldl recordBitFrequencyFull (replicate strLength 0) binInts
      epsilon = convert $ reverse $ map toLeastCommonBit freqArr
      gamma = convert $ reverse $ getGammaValueBin freqArr
  _ <- print $ "Answer: " ++ show (epsilon * gamma)
  return ()

reportDay3Part2 :: IO ()
reportDay3Part2 = do
  binInts <- numberStringsToInts <$> getNumberStrings
  let oxRating = convert $ reverse $ oxygenRating binInts
      scrubRat = convert $ reverse $ scrubberRating binInts
  _ <- print $ oxRating * scrubRat
  return ()