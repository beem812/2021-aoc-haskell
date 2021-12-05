{-# LANGUAGE DeriveGeneric #-}

module Day5.Day5 where

import Data.Map
import GHC.Exception (fromCallSiteList)
import GHC.Generics (Generic)

data LineSegment a = LineSegment a a a a deriving (Generic, Show)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

getDay5File :: IO [String]
getDay5File = do
  string <- readFile "src/Day5/day5.txt"
  return $ lines string

parseLineSegments :: [String] -> [LineSegment Int]
parseLineSegments [] = []
parseLineSegments (x : xs) =
  let (first : _ : second : _) = words x
      (x1 : y1 : x2 : y2 : _) = Prelude.map read $ wordsWhen (== ',') (first ++ "," ++ second)
   in LineSegment x1 y1 x2 y2 : parseLineSegments xs

shortenSegment :: (Eq a, Num a, Ord a) => LineSegment a -> LineSegment a
shortenSegment (LineSegment x1 y1 x2 y2) | x1 == x2 = if y1 > y2 then LineSegment x1 (y1 - 1) x2 y2 else LineSegment x1 (y1 + 1) x2 y2
shortenSegment (LineSegment x1 y1 x2 y2) | y1 == y2 = if x1 > x2 then LineSegment (x1 - 1) y1 x2 y2 else LineSegment (x1 + 1) y1 x2 y2
shortenSegment (LineSegment x1 y1 x2 y2) | y1 > y2 && x1 > x2 = LineSegment (x1 - 1) (y1 - 1) x2 y2
shortenSegment (LineSegment x1 y1 x2 y2) | y1 < y2 && x1 < x2 = LineSegment (x1 + 1) (y1 + 1) x2 y2
shortenSegment (LineSegment x1 y1 x2 y2) | y1 > y2 && x1 < x2 = LineSegment (x1 + 1) (y1 - 1) x2 y2
shortenSegment (LineSegment x1 y1 x2 y2) | y1 < y2 && x1 > x2 = LineSegment (x1 - 1) (y1 + 1) x2 y2
shortenSegment segment = segment

foldSegment :: (Eq a, Ord a, Num a) => ((a, a) -> b -> b) -> b -> LineSegment a -> b
foldSegment f acc (LineSegment x1 y1 x2 y2) | x1 == x2 && y1 == y2 = f (x1, y1) acc
foldSegment f acc (LineSegment x1 y1 x2 y2) = foldSegment f (f (x1, y1) acc) (shortenSegment (LineSegment x1 y1 x2 y2))

updateCloudMap :: (Ord a, Num a) => (a, a) -> Map (a, a) a -> Map (a, a) a
updateCloudMap coord = insertWith (+) coord 1

filterDiagonalLines :: (Eq a) => [LineSegment a] -> [LineSegment a]
filterDiagonalLines = Prelude.filter nonVertical
  where
    nonVertical (LineSegment x1 y1 x2 y2) | x1 /= x2 && y1 /= y2 = False
    nonVertical _ = True

driver :: IO ()
driver = do
  lines <- getDay5File
  let segments = parseLineSegments lines
  let cloudMap = empty :: Map (Int, Int) Int
  let newMap = Prelude.foldl (foldSegment updateCloudMap) cloudMap segments
  _ <- print $ Data.Map.foldl (\acc value -> if value > 1 then acc + 1 else acc) 0 newMap
  return ()