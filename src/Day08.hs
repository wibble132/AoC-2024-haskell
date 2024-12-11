module Day08 (part1, part2, readInput) where

import Data.Bifunctor (bimap)
import Data.Ix (inRange)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

part1 :: Input -> Int
part1 = length . antiNodePositions antiNodesForBeacon1

--  $ Input (M.fromList [('a', [(4, 3), (5, 5), (8, 4)]), ('A', [(6,7)])]) (9, 9)

part2 :: Input -> Int
part2 = length . antiNodePositions antiNodesForBeacon2

data Input = Input
  { beacons :: Map Char [(Int, Int)],
    maxBounds :: (Int, Int)
  }
  deriving (Show)

readInput :: String -> Input
readInput s = Input beacons maxBounds
  where
    positions = withPositions . lines $ s
    beaconPositions = filter ((/= '.') . snd) positions
    beacons = foldl' (\acc (pos, beacon) -> M.insertWith (++) beacon [pos] acc) M.empty beaconPositions
    -- This does iterate through all the positions to get this, but I don't think I care
    maxBounds = fst . last $ positions

withPositions :: [[a]] -> [((Int, Int), a)]
withPositions = concat . zipWith (\i_y ys -> zipWith (\i_x x -> ((i_x, i_y), x)) [0 ..] ys) [0 ..]

antiNodePositions :: ((Int, Int) -> [(Int, Int)] -> [(Int, Int)]) -> Input -> Set (Int, Int)
antiNodePositions f (Input beacons maxBounds) =
  --   S.filter (`notElem` concat (M.elems beacons))
  S.filter (((0, 0), maxBounds) `inRange`) $
    M.foldl' (\acc x -> acc `S.union` S.fromList (f maxBounds x)) S.empty beacons

antiNodesForBeacon1 :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
antiNodesForBeacon1 _ beaconPos =
  concatMap (\(a, b) -> [moveStep a b, moveStep b a]) (distinctPairs beaconPos)

antiNodesForBeacon2 :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
antiNodesForBeacon2 maxBounds beaconPos =
  concatMap (\(a, b) -> allStepsInBounds maxBounds a b ++ allStepsInBounds maxBounds b a) (distinctPairs beaconPos)

moveStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveStep from to = bimap ((2 * fst to) -) ((2 * snd to) -) from

allStepsInBounds :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
allStepsInBounds maxBounds from to = (from :) . map snd . iterateMaybe (moveStepInBounds maxBounds) $ (from, next)
  where
    diff = bimap (fst to -) (snd to -) from
    divisor = uncurry gcd diff
    step = bimap (`div` divisor) (`div` divisor) diff
    next = bimap (fst from +) (snd from +) step

moveStepInBounds :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Maybe ((Int, Int), (Int, Int))
moveStepInBounds maxBounds (from, to)
  | inRange ((0, 0), maxBounds) nextPos = Just (to, nextPos)
  | otherwise = Nothing
  where
    nextPos = moveStep from to

iterateMaybe :: (b -> Maybe b) -> b -> [b]
iterateMaybe f x
  | Just y <- f x = y : iterateMaybe f y
  | otherwise = []

distinctPairs :: [a] -> [(a, a)]
distinctPairs (x : xs) = map (x,) xs ++ distinctPairs xs
distinctPairs [] = []
