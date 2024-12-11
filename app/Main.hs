module Main where

import Day01 (part1, part2, readInput)
import Day02 (part1, part2)
import Day03 (part1, part2)
import Day04 (part1, part2)
import Day05 (part1, part2)
import Day06 (part1, part2)
import Day07 (part1, part2, readInput)
import Day08 (part1, part2, readInput)
import Day09 (part1, part2, readInput1, readInput2)
import Day10 (part1, part2, readInput)
import Day11 (part1, part2, readInput)

import Criterion.Main (bench, bgroup, defaultMain, whnf)

-- main :: IO ()
-- main = day dayNum testNum
--   where
--     -- The day to run
--     dayNum = 11
--     -- Which test to run. 0 for the full input (dayN.txt), other number for a test (e.g. day3-e2.txt)
--     testNum = 0

main :: IO ()
main = do
  input <- getInput 11
  let !parsedInput = Day11.readInput input
  defaultMain
    [ bgroup
        "day 11"
        [ bench "parse" $ whnf Day11.readInput input,
          bench "Part 1" $ whnf Day11.part1 parsedInput,
          bench "Part 2" $ whnf Day11.part2 parsedInput
        ]
    ]

day :: Int -> Int -> IO ()
day d i = do
  putStrLn $ "Day " ++ show d
  input <- if i == 0 then getInput d else getExample d i
  putStrLn $ getDayPart d 1 input
  putStrLn $ getDayPart d 2 input

getDayPart :: Int -> Int -> (String -> String)
getDayPart d p = case (d, p) of
  (1, 1) -> show . Day01.part1 . Day01.readInput
  (1, 2) -> show . Day01.part2 . Day01.readInput
  (2, 1) -> Day02.part1
  (2, 2) -> Day02.part2
  (3, 1) -> Day03.part1
  (3, 2) -> Day03.part2
  (4, 1) -> Day04.part1
  (4, 2) -> Day04.part2
  (5, 1) -> Day05.part1
  (5, 2) -> Day05.part2
  (6, 1) -> Day06.part1
  (6, 2) -> Day06.part2
  (7, 1) -> show . Day07.part1 . Day07.readInput
  (7, 2) -> show . Day07.part2 . Day07.readInput
  (8, 1) -> show . Day08.part1 . Day08.readInput
  (8, 2) -> show . Day08.part2 . Day08.readInput
  (9, 1) -> show . Day09.part1 . Day09.readInput1
  (9, 2) -> Day09.part2 . Day09.readInput2
  (10, 1) -> Day10.part1 . Day10.readInput
  (10, 2) -> Day10.part2 . Day10.readInput
  (11, 1) -> Day11.part1 . Day11.readInput
  (11, 2) -> Day11.part2 . Day11.readInput
  _ -> error "Unknown day part"

getInput :: Int -> IO String
getInput d = readFile ("app/data/day" ++ show d ++ ".txt")

getExample :: Int -> Int -> IO String
getExample d exNum = readFile ("app/data/day" ++ show d ++ "-e" ++ show exNum ++ ".txt")

getExample' :: Int -> Int -> IO String
getExample' = ((readFile . ("app/data/day" ++)) .) . (. (("-e" ++) . (++ ".txt") . show)) . (++) . show