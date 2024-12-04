module Day01 (part1, part2) where

import Data.List (sort, transpose)
import Data.List.Split (splitOn)

part1 :: String -> String
part1 = show . sum . map (\(a, b) -> abs (a - b)) . toPairs . transpose . map sort . readinput

part2 :: String -> String
part2 = show . similarityScore . toLists . readinput

readinput :: String -> [[Integer]]
readinput = transpose . map ((map read :: [String] -> [Integer]) . splitOn "   ") . lines

similarityScore :: ([Integer], [Integer]) -> Integer
similarityScore (x : xs, rhs) = x * count (x ==) rhs + similarityScore (xs, rhs)
similarityScore ([], _) = 0

toPairs :: [[a]] -> [(a, a)]
toPairs ([x, y] : c) = (x, y) : toPairs c
toPairs [] = []
toPairs _ = error "Bad input"

toLists :: [[a]] -> ([a], [a])
toLists [x, y] = (x, y)
toLists _ = error "Bad input"

count :: (a -> Bool) -> [a] -> Integer
count p = toInteger . length . filter p