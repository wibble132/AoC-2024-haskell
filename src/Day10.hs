module Day10 (part1, part2, readInput) where

import Control.Arrow (second)
import Data.Array (Array)
import Data.Array qualified as A
import Data.Char (digitToInt)
import Data.List (partition)
import Data.Set (Set)
import Data.Set qualified as S

part1 :: TopoMap -> String
part1 = show . searchArea1 (== 0) ((==) . (1 +)) (== 9)

part2 :: TopoMap -> String
part2 = show . searchArea2 (== 0) ((==) . (1 +)) (== 9)

type Position = (Int, Int)

type TopoMap = Array Position Int

readInput :: String -> TopoMap
readInput s = A.array ((0, 0), maxBounds) wPos
  where
    wPos = map (second readDigit) . withPositions . lines $ s
    maxBounds = fst . last $ wPos

readDigit :: Char -> Int
readDigit '.' = -1
readDigit c = digitToInt c

withPositions :: [[a]] -> [((Int, Int), a)]
withPositions = concat . zipWith (\i_y ys -> zipWith (\i_x x -> ((i_x, i_y), x)) [0 ..] ys) [0 ..]

searchArea1 ::
  -- Is this somewhere to start from
  (Int -> Bool) ->
  -- Is this step valid
  (Int -> Int -> Bool) ->
  -- Is this a finish
  (Int -> Bool) ->
  -- The map
  TopoMap ->
  -- Sum of trailhead scores
  Int
searchArea1 isStart isStepValid isFinish area =
  sum . map (S.size . searchFrom) . filter (isStart . snd) $ A.assocs area
  where
    searchFrom :: ((Int, Int), Int) -> Set Position
    searchFrom (p, v) = S.fromList (map fst ends) `S.union` S.unions (map searchFrom continues)
      where
        nextSteps = filter (isStepValid v . snd) . map ((,) <*> (area A.!)) . filter (A.bounds area `A.inRange`) . getNeighbours $ p
        (ends, continues) = partition (isFinish . snd) nextSteps

searchArea2 ::
  -- Is this somewhere to start from
  (Int -> Bool) ->
  -- Is this step valid
  (Int -> Int -> Bool) ->
  -- Is this a finish
  (Int -> Bool) ->
  -- The map
  TopoMap ->
  -- Number of valid paths
  Int
searchArea2 isStart isStepValid isFinish area =
  sum . map searchFrom . filter (isStart . snd) $ A.assocs area
  where
    searchFrom :: ((Int, Int), Int) -> Int
    searchFrom (p, v) = length ends + sum (map searchFrom continues)
      where
        nextSteps = filter (isStepValid v . snd) . map ((,) <*> (area A.!)) . filter (A.bounds area `A.inRange`) . getNeighbours $ p
        (ends, continues) = partition (isFinish . snd) nextSteps

getNeighbours :: Position -> [Position]
getNeighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1)]