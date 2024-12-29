module Day25 (part1, part2, readInput) where

import Data.Either (partitionEithers)
import Data.List (transpose, uncons)
import Data.List.Split (splitOn)

part1 :: Input -> String
part1 = show . uncurry (flip validPairs)

part2 :: Input -> String
part2 = const ""

readInput :: String -> Input
readInput = partitionEithers . map readLockOrKey . splitOn "\n\n"

------ PARSING ------

type Input = ([Lock], [Key])

data Lock = Lock [Int] deriving (Show)

data Key = Key [Int] deriving (Show)

readLockOrKey :: String -> Either Lock Key
readLockOrKey s = case top of
  Just ("#####", _) -> Left $ Lock values
  Just (".....", _) -> Right $ Key values
  _ -> error "Bad Input"
  where
    ls = lines s
    top = uncons ls

    values = map (subtract 1) . map sum . transpose . map (map (fromEnum . (== '#'))) $ ls

------ PART 1 ------

validPairs :: [Key] -> [Lock] -> Int
validPairs ks ls = length . filter (uncurry doesFit) $ [(k, l) | k <- ks, l <- ls]

doesFit :: Key -> Lock -> Bool
doesFit (Key k) (Lock l) = all (<= 5) (zipWith (+) k l)

------ PART 2 ------
