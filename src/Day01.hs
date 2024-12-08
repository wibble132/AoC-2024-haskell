module Day01 (part1, part2, readInput) where

import Control.Arrow (first)
import Data.Bifunctor (bimap)
import Data.List (sort, stripPrefix)
import Data.Maybe (fromJust)

part1 :: ([Int], [Int]) -> Int
part1 = sum . map (uncurry ((abs .) . (-))) . uncurry zip . both sort

part2 :: ([Int], [Int]) -> Int
part2 = similarityScore

readInput :: String -> ([Int], [Int])
readInput = unzip . map (bimap read read . fromJust . stripInfix "   ") . lines

similarityScore :: ([Int], [Int]) -> Int
similarityScore (x : xs, rhs) = x * count (x ==) rhs + similarityScore (xs, rhs)
similarityScore ([], _) = 0

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

stripInfix :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
stripInfix needle haystack | Just rest <- stripPrefix needle haystack = Just ([], rest)
stripInfix _ [] = Nothing
stripInfix needle (x : xs) = first (x :) <$> stripInfix needle xs

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f
