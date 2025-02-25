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
import Day12 (part1, part2, readInput)
import Day13 (part1, part2, readInput)
import Day14 (part1, part2, readInput)
import Day15 (part1, part2, readInput1, readInput2)
import Day16 (part1, part2, readInput)
import Day17 (part1, part2, readInput)
import Day18 (part1, part2, readInput)
import Day19 (part1, part2, readInput)
import Day20 (part1, part2, readInput)
import Day21 (part1, part2, readInput)
import Day22 (part1, part2, readInput)
import Day23 (part1, part2, readInput)
import Day24 (part1, part2, readInput)
import Day25 (part1, part2, readInput)

import Criterion.Main (bench, bgroup, defaultMain, whnf)

-- main :: IO ()
-- main = day dayNum testNum
--   where
--     -- The day to run
--     dayNum = 3
--     -- Which test to run. 0 for the full input (dayN.txt), other number for a test (e.g. 2 for day3-e2.txt)
--     testNum = 0

main :: IO ()
main = do
  !input <- getInput 3
  -- let !parsedInput = Day03.readInput input
  defaultMain
    [ bgroup
        "day 3"
        [ 
          -- bench "parse" $ whnf Day03.readInput input,
          bench "Part 1" $ whnf Day03.part1 input,
          bench "Part 2" $ whnf Day03.part2 input
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
  (12, 1) -> Day12.part1 . Day12.readInput
  (12, 2) -> Day12.part2 . Day12.readInput
  (13, 1) -> Day13.part1 . Day13.readInput
  (13, 2) -> Day13.part2 . Day13.readInput
  (14, 1) -> Day14.part1 . Day14.readInput
  (14, 2) -> Day14.part2 . Day14.readInput
  (15, 1) -> Day15.part1 . Day15.readInput1
  (15, 2) -> Day15.part2 . Day15.readInput2
  (16, 1) -> Day16.part1 . Day16.readInput
  (16, 2) -> Day16.part2 . Day16.readInput
  (17, 1) -> Day17.part1 . Day17.readInput
  (17, 2) -> Day17.part2 . Day17.readInput
  (18, 1) -> Day18.part1 . Day18.readInput
  (18, 2) -> Day18.part2 . Day18.readInput
  (19, 1) -> Day19.part1 . Day19.readInput
  (19, 2) -> Day19.part2 . Day19.readInput
  (20, 1) -> Day20.part1 . Day20.readInput
  (20, 2) -> Day20.part2 . Day20.readInput
  (21, 1) -> Day21.part1 . Day21.readInput
  (21, 2) -> Day21.part2 . Day21.readInput
  (22, 1) -> Day22.part1 . Day22.readInput
  (22, 2) -> Day22.part2 . Day22.readInput
  (23, 1) -> Day23.part1 . Day23.readInput
  (23, 2) -> Day23.part2 . Day23.readInput
  (24, 1) -> Day24.part1 . Day24.readInput
  (24, 2) -> Day24.part2 . Day24.readInput
  (25, 1) -> Day25.part1 . Day25.readInput
  (25, 2) -> Day25.part2 . Day25.readInput
  _ -> error "Unknown day part"

getInput :: Int -> IO String
getInput d = readFile ("app/data/day" ++ show d ++ ".txt")

getExample :: Int -> Int -> IO String
getExample d exNum = readFile ("app/data/day" ++ show d ++ "-e" ++ show exNum ++ ".txt")

getExample' :: Int -> Int -> IO String
getExample' = ((readFile . ("app/data/day" ++)) .) . (. (("-e" ++) . (++ ".txt") . show)) . (++) . show