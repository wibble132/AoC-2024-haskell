{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE LambdaCase #-}
module Day11 (part1, part2, readInput) where

import Data.List.Split (splitOn)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import Utils (iterateN)

part1 :: [Integer] -> String
part1 = show . blink 25

part2 :: [Integer] -> String
part2 = show . blink' 75 . map fromInteger

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

-- For part 2, changed from the naiive solution
-- - Used a MultiSet to keep track of the stones (don't care about the position, just the number of each at each step)
--    - This made it possible to run. Did part 2 in 110ms, compared to 95ms for the above with part 1
-- - Changed Integer to Int
--    - This was used cautiously at first since I didn't know the expected scale of the results
--    - Surprisingly doesn't impact the performance of the result, still 110ms
-- - Did splitting stone calculation using `logBase 10` instead of Strings
--    - Needs a little more thought to avoid errors, but it is a more direct way to achieve the result
--    - Significant improvement, down to 40ms
blink' :: Int -> [Int] -> Int
blink' stepCount = MS.size . iterateN stepCount step . MS.fromList
  where
    step :: MultiSet Int -> MultiSet Int
    step = MS.concatMap blinkStone'

blinkStone' :: Int -> [Int]
blinkStone' 0 = [1]
blinkStone' x
  | even numDigits = [h1, h2]
  | otherwise = [x * 2024]
  where
    numDigits :: Int
    numDigits = (1 +) . floor . logBase 10 $ (fromIntegral x :: Double)
    halfLen = numDigits `div` 2
    (h1, h2) = x `divMod` (10 ^ halfLen)
