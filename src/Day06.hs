module Day06 (part1, part2) where

import Data.List (elemIndex, uncons)
import Data.Maybe (fromJust)
import Data.Set qualified as Set (Set, empty, filter, insert, map, member)
import Data.Tuple (swap)
import GHC.Arr qualified as Arr (Array, bounds, inRange, ixmap, listArray, (!), (//))
import Prelude hiding (Left, Right)

part1 :: String -> String
part1 = show . length . Set.map fst . fst . uncurry searchArea . parseInput

part2 :: String -> String
part2 s = show $ length blockers
  where
    (a, startPos) = parseInput s
    initialWalk = Set.filter (/= startPos) . Set.map fst . fst $ searchArea a startPos
    blockers = Set.filter (\pos -> snd $ searchArea (a Arr.// [(pos, Obstruction)]) startPos) initialWalk

type Area = Arr.Array Pos AreaSpace

type Pos = (Int, Int)

data AreaSpace = Obstruction | Space deriving (Show, Enum, Eq)

parseInput :: String -> (Area, Pos)
parseInput s = (transposedArr, startPos)
  where
    maxBounds = (subtract 1 . length . fst . fromJust . uncons . lines $ s, subtract 1 . length . lines $ s)
    mappedList = map (\x -> if x == '#' then Obstruction else Space) . filter (/= '\n') $ s
    arr = Arr.listArray ((0, 0), maxBounds) mappedList
    -- I want the indices to be (x = horizontal,y = vertical) with (0,0) in the top left
    -- Just transpose this
    transposedArr = Arr.ixmap ((0, 0), swap maxBounds) swap arr

    startIndex = fromJust . elemIndex '^' $ s
    -- +2 is here due to maxBounds being inclusive and we have also have \n at end of each line
    startPos = swap $ startIndex `divMod` (+ 2) (snd maxBounds)

data Dir
  = Up
  | Right
  | Down
  | Left
  deriving (Show, Eq, Enum, Ord)

turnRight :: Dir -> Dir
turnRight = toEnum . (`mod` 4) . (1 +) . fromEnum

searchArea :: Area -> Pos -> (Set.Set (Pos, Dir), Bool)
searchArea area startPos = searchFrom startPos Up Set.empty
  where
    searchFrom :: Pos -> Dir -> Set.Set (Pos, Dir) -> (Set.Set (Pos, Dir), Bool)
    searchFrom p d visited
      -- \| trace ("Visiting " ++ show (p, d)) False = undefined
      | (p, d) `Set.member` visited = (visited, True)
      | leavesArea area next = (nextVisited, False)
      | isBlocked area next = searchFrom p (turnRight d) visited
      | otherwise = searchFrom next d nextVisited
      where
        next = forward p d
        nextVisited = Set.insert (p, d) visited

forward :: Pos -> Dir -> Pos
forward (x, y) Up = (x, y - 1)
forward (x, y) Right = (x + 1, y)
forward (x, y) Down = (x, y + 1)
forward (x, y) Left = (x - 1, y)

isBlocked :: Area -> Pos -> Bool
isBlocked a p
  | a Arr.! p == Obstruction = True
  | otherwise = False

leavesArea :: Area -> Pos -> Bool
leavesArea a p
  | not $ Arr.inRange (Arr.bounds a) p = True
  | otherwise = False
