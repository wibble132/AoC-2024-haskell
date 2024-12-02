module Day02 (part1, part2) where

import Data.List.Split (splitOn)

part1 :: String -> String
part1 = show . length . filter id . map isSafe . parseInput

part2 :: String -> String
part2 = show . length . filter id . map isAlmostSafe . parseInput

newtype Report = Levels [Int] deriving (Show)

parseInput :: String -> [Report]
parseInput = map (Levels . map read . splitOn " ") . lines

isSafe :: Report -> Bool
isSafe (Levels (a : b : rest)) = inner a b rest
  where
    inner :: Int -> Int -> [Int] -> Bool
    -- Must be at most 3 apart
    inner last2 last1 _ | abs (last2 - last1) >= 4 = False
    -- Must be monotonically increasing or decreasing
    inner last2 last1 (next : xs) | last2 < last1 && last1 < next = inner last1 next xs
    inner last2 last1 (next : xs) | last2 > last1 && last1 > next = inner last1 next xs
    -- End of list, pass (Monotonic & distance checked above)
    inner _ _ [] = True
    -- Fail
    inner _ _ _ = False
isSafe _ = False

isAlmostSafe :: Report -> Bool
isAlmostSafe r | isSafe r = True
isAlmostSafe (Levels levels) = inner [] levels
  where
    inner :: [Int] -> [Int] -> Bool
    inner a (x : xs) = isSafe (Levels $ a ++ xs) || inner (a ++ [x]) xs
    inner a [] = isSafe $ Levels a