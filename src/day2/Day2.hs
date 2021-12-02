module Day2 where

(|>) :: a -> (a -> b) -> b
(|>) x y = y x

type Horizontal = Int

type Depth = Int

type Aim = Int

getFile :: IO String
getFile = readFile "src/day2/day2.txt"

zipCommands :: [String] -> [(String, Int)]
zipCommands (x : y : xs) = (x, read y) : zipCommands xs
zipCommands _ = []

processCommandPart1 :: (Horizontal, Depth) -> (String, Int) -> (Horizontal, Depth)
processCommandPart1 (h, d) ("forward", x) = (h + x, d)
processCommandPart1 (h, d) ("down", x) = (h, d + x)
processCommandPart1 (h, d) ("up", x) = (h, d - x)
processCommandPart1 acc _ = acc

processCommandPart2 :: (Horizontal, Depth, Aim) -> (String, Int) -> (Horizontal, Depth, Aim)
processCommandPart2 (h, d, a) ("forward", x) = (h + x, d + (x * a), a)
processCommandPart2 (h, d, a) ("down", x) = (h, d, a + x)
processCommandPart2 (h, d, a) ("up", x) = (h, d, a - x)
processCommandPart2 acc _ = acc

reportAnswerPart1 :: IO ()
reportAnswerPart1 = do
  str <- getFile
  let (h, d) = words str |> zipCommands |> foldl processCommandPart1 (0, 0)
  _ <- print $ "Horizontal position: " ++ show h ++ " Depth: " ++ show d
  _ <- print $ "Answer: " ++ show (h * d)
  return ()

reportAnswerPart2 :: IO ()
reportAnswerPart2 = do
  str <- getFile
  let (h, d, a) = words str |> zipCommands |> foldl processCommandPart2 (0, 0, 0)
  _ <- print $ "Horizontal position: " ++ show h ++ " Depth: " ++ show d ++ " Aim: " ++ show a
  _ <- print $ "Answer: " ++ show (h * d)
  return ()