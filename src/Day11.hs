module Day11 (part1, part2, readInput) where

import Data.List.Split (splitOn)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS

part1 :: [Integer] -> String
part1 = show . blink 25

part2 :: [Integer] -> String
part2 = show . blink' 75

readInput :: String -> [Integer]
readInput = map read . splitOn " "

blink :: Int -> [Integer] -> Integer
blink stepCount = sum . map (step stepCount)
  where
    step :: Int -> Integer -> Integer
    step 0 _ = 1
    step i stone = sum $ map (step (i - 1)) $ blinkStone stone

blinkStone :: Integer -> [Integer]
blinkStone 0 = [1]
blinkStone x
  | even lenStr = [read h1, read h2]
  | otherwise = [x * 2024]
  where
    numStr = show x
    lenStr = length numStr
    halfLen = lenStr `div` 2
    (h1, h2) = splitAt halfLen numStr

blink' :: Int -> [Integer] -> Int
blink' stepCount = MS.size . iterateN stepCount step . MS.fromList
  where
    step :: MultiSet Integer -> MultiSet Integer
    step = MS.concatMap blinkStone

iterateN :: Int -> (a -> a) -> (a -> a)
iterateN n f = (!! n) . iterate f