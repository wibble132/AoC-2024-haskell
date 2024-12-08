module Main where

import Day01 (part1, part2, readInput)
import Day02 (part1, part2)
import Day03 (part1, part2)
import Day04 (part1, part2)
import Day05 (part1, part2)
import Day06 (part1, part2)
import Day07 (part1, part2, readInput)

import Criterion.Main

-- main :: IO ()
-- main = day dayNum testNum
--   where
--     -- The day to run
--     dayNum = 7
--     -- Which test to run. 0 for the full input (dayN.txt), other number for a test (e.g. day3-e2.txt)
--     testNum = 0

main :: IO ()
main = do
  input <- getInput 7
  let parsedInput = Day07.readInput input
  defaultMain [
    bgroup "day 7" [
      bench "parse" $ whnf Day07.readInput input,
      bench "Part 1" $ whnf Day07.part1 parsedInput,
      bench "Part 2" $ whnf Day07.part2 parsedInput
    ]]


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
  _ -> error "Unknown day part"

getInput :: Int -> IO String
getInput d = readFile ("app/data/day" ++ show d ++ ".txt")

getExample :: Int -> Int -> IO String
getExample d exNum = readFile ("app/data/day" ++ show d ++ "-e" ++ show exNum ++ ".txt")

getExample' :: Int -> Int -> IO String
getExample'= ((readFile . ("app/data/day" ++)) .) . (. (("-e" ++) . (++ ".txt") . show)) . (++) . show