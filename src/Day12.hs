module Day12 (part1, part2, readInput) where

import Control.Arrow (Arrow (first), second)
import Control.Monad (join, liftM2)
import Data.Array (Array)
import Data.Array qualified as A
import Data.Bifunctor (bimap)
import Data.Set (Set)
import Data.Set qualified as S
-- import Debug.Trace (trace)
trace :: String -> a -> a
trace = const id

part1 :: Input -> String
part1 = show . totalPrice

part2 :: Input -> String
part2 = show . totalPriceDiscounted
-- part2 = unlines . map show . totalPriceDiscounted

type Position = (Int, Int)

type Input = Array (Int, Int) Char

readInput :: String -> Input
readInput s = A.array ((0, 0), maxBounds) wPos
  where
    wPos = withPositions . lines $ s
    maxBounds = fst . last $ wPos

withPositions :: [[a]] -> [((Int, Int), a)]
withPositions = concat . zipWith (\i_y ys -> zipWith (\i_x x -> ((i_x, i_y), x)) [0 ..] ys) [0 ..]

totalPrice :: Input -> Int
totalPrice a = sum . map (\x -> perimeter x * area x) $ regions
  where
    regions :: [[Position]]
    regions = getRegions a

    ps :: Array Position Int
    ps = perimeterContributions a

    perimeter :: [Position] -> Int
    perimeter = sum . map (ps A.!)
    area :: [Position] -> Int
    area = length

getRegions :: Input -> [[Position]]
getRegions a = foldl' step [] (A.indices a)
  where
    step :: [[Position]] -> Position -> [[Position]]
    step ps p | any (p `elem`) ps = ps
    step ps p = findRegionFrom a p : ps

findRegionFrom :: Input -> Position -> [Position]
findRegionFrom a p = step [p] p
  where
    bounds = A.bounds a

    step :: [Position] -> Position -> [Position]
    step searched next = foldl' step nextNeighbours validNeighbours
      where
        c = a A.! next
        validNeighbours =
          -- Must have not already visited
          filter (`notElem` searched)
            -- Must be same character as at `next`
            . filter ((c ==) . (a A.!))
            $ nbours
        nbours = neighbours bounds next

        -- Inefficient, but should be fine for the scale of the areas
        nextNeighbours = validNeighbours ++ searched

perimeterContributions :: (Eq a) => Array Position a -> Array Position Int
perimeterContributions area = A.listArray bounds (zipWith (+) innerWallCount outerWallCount)
  where
    bounds = A.bounds area

    -- Perimeter contributions from walls between regions
    innerWallCount :: [Int]
    innerWallCount =
      map length
        . map (\(ns, c) -> filter ((c /=) . (area A.!)) ns)
        . map (first $ neighbours bounds)
        $ A.assocs area

    outerWallCount :: [Int]
    outerWallCount =
      map
        (\x -> matchesCorner x (fst bounds) + matchesCorner x (snd bounds))
        $ A.indices area
    matchesCorner :: Position -> Position -> Int
    matchesCorner (x, y) (cx, cy) = fromEnum (x == cx) + fromEnum (y == cy)

neighbours :: (Position, Position) -> Position -> [Position]
neighbours bounds (x, y) =
  filter (A.inRange bounds) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

totalPriceDiscounted :: Input -> Int
totalPriceDiscounted a = sum . map (\x -> (sideCount x * area x)) $ regions
  where
    regions :: [[Position]]
    regions = getRegions a

    area :: [Position] -> Int
    area = length

sideCount :: [Position] -> Int
sideCount r = sum $ map (trace "" $ sideContribution $ S.fromList r) r

sideContribution :: Set Position -> Position -> Int
sideContribution r p = length $ filter (uncurry isCorner) ops
  where
    ops :: [(Int -> Int, Int -> Int)]
    ops = map (bimap ($ 1) ($ 1)) $ join (liftM2 (,)) [(+), flip (-)]

    isCorner :: (Int -> Int) -> (Int -> Int) -> Bool
    isCorner fX fY = trace (show p ++ " " ++ show (px, pxy, py) ++ " " ++ show res) res
      where
        px = first fX p
        py = second fY p
        pxy = first fX py

        (a, b, c) = (pxy `S.member` r, px `S.member` r, py `S.member` r)
        res = (not a && (b == c)) || (a && not b && not c)