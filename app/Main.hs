module Main where

import Day01 (part1, part2)
import Day02 (part1, part2)

main :: IO ()
main = day dayNum testNum
  where 
    -- The day to run
    dayNum = 2
    -- Which test to run. 0 for the full input (dayN.txt), other number for a test (e.g. day3-e2.txt)
    testNum = 0

day :: Int -> Int -> IO ()
day d i = do
  putStrLn $ "Day " ++ show d
  input <- if i == 0 then getInput d else getExample d i
  putStrLn $ getDayPart d 1 input
  putStrLn $ getDayPart d 2 input

getDayPart :: Int -> Int -> (String -> String)
getDayPart d p = case (d,p) of
  (1,1) -> Day01.part1
  (1,2) -> Day01.part2
  (2,1) -> Day02.part1
  (2,2) -> Day02.part2
  _ -> error "Unknown day part"

getInput :: Int -> IO String
getInput d = readFile ("app/data/day" ++ show d ++ ".txt")

getExample :: Int -> Int -> IO String
getExample d exNum = readFile ("app/data/day" ++ show d ++ "-e" ++ show exNum ++ ".txt")
