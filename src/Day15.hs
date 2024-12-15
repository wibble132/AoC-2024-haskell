module Day15 (part1, part2, readInput1, readInput2) where

import Control.Arrow (Arrow (first, second))
import Control.Monad (foldM)
import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A
import Data.List (find, sortOn, transpose)
import Data.List.Split (chunk, splitOn)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Utils (Position, toArray)
import Prelude hiding (Left, Right)

part1 :: Input -> String
part1 = show . sum . gpsCoordinates . fst . uncurry (uncurry makeMoves)

-- part1 = showGrid . uncurry placeRobot . uncurry (uncurry makeMoves2)

-- 1443193 too high
-- 1422600 too low
-- 1432898
part2 :: Input -> String
part2 = show . sum . gpsCoordinates . fst . uncurry (uncurry makeMoves2)

-- part2 = showGrid . uncurry placeRobot . uncurry (uncurry makeMoves2) . second (take 3315)

readInput1 :: String -> Input
readInput1 = first readGrid . second (map charToDir . concat . lines) . takePair . splitOn "\n\n"

readInput2 :: String -> Input
readInput2 = readInput1 . widen

------ PARSING ------

type Input = ((Grid, Position), Moves)

type Moves = [Dir]

data Dir = Up | Right | Down | Left deriving (Show)

charToDir :: Char -> Dir
charToDir '^' = Up
charToDir '>' = Right
charToDir 'v' = Down
charToDir '<' = Left
charToDir c = error $ "Bad dir " ++ [c]

takePair :: [a] -> (a, a)
takePair [x, y] = (x, y)
takePair xs = error $ "Cannot take pair from list of length " ++ show (length xs)

type Grid = Array Position Tile

data Tile
  = Wall
  | Space
  | Box
  | Robot
  | WideBoxLeft
  | WideBoxRight
  deriving (Show, Eq, Ord)

readGrid :: String -> (Grid, Position)
readGrid = extractRobot . toArray . map (map charToTile) . lines

charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile 'O' = Box
charToTile '.' = Space
charToTile '@' = Robot
charToTile '[' = WideBoxLeft
charToTile ']' = WideBoxRight
charToTile c = error $ "Invalid tile: " ++ [c]

extractRobot :: Grid -> (Grid, Position)
extractRobot = (\(g, p) -> (g A.// [(p, Space)], p)) . ((,) <*> findRobot)

findRobot :: Grid -> Position
findRobot = fst . fromJust . find ((== Robot) . snd) . A.assocs

-- Used for debugging
_placeRobot :: Grid -> Position -> Grid
_placeRobot g p
  | (Just Space) <- g A.!? p = g A.// [(p, Robot)]
  | otherwise = trace (concat ["Cannot place robot at ", show p, " since there is a ", show (g A.!? p), " in that position."]) g A.// [(p, Robot)]

-- Used for debugging
_showGrid :: Grid -> String
_showGrid g = unlines . transpose . chunk lineLength . map tileToChar . A.elems $ g
  where
    lineLength = (+ 1) . snd . snd . A.bounds $ g

    tileToChar Space = '.'
    tileToChar Wall = '#'
    tileToChar Box = 'O'
    tileToChar Robot = '@'
    tileToChar WideBoxLeft = '['
    tileToChar WideBoxRight = ']'

------ PART 1 ------

posDir :: Dir -> Position -> Position
posDir Up = second (subtract 1)
posDir Right = first (+ 1)
posDir Down = second (+ 1)
posDir Left = first (subtract 1)

-- Pushes multiple boxes by swapping the first box with an empty space at the end of a line of boxes
-- Reduces the number of changes to the array. Completes Part1 in 9.6ms
tryMove :: Grid -> Position -> Dir -> (Grid, Position)
tryMove g p d
  --   | trace (concat ["Grid is:\n", showGrid (placeRobot g p), "\nHave position ", show p, " and direction ", show d, " and space is ", show emptySpace]) False = undefined
  | g A.! (posDir d p) == Space = (g, posDir d p)
  | otherwise = case emptySpace of
      Nothing -> (g, p)
      Just p' -> (g A.// [(posDir d p, Space), (p', Box)], posDir d p)
  where
    emptySpace = lookForSpace p
    lookForSpace p' = case g A.!? nextPos of
      Just Space -> Just nextPos
      Nothing -> Nothing
      Just Wall -> Nothing
      _ -> lookForSpace nextPos
      where
        nextPos = posDir d p'

makeMoves :: Grid -> Position -> [Dir] -> (Grid, Position)
makeMoves g p = foldl' (\(g', p') d -> (tryMove g' p' d)) (g, p)

gpsCoordinates :: Grid -> [Int]
gpsCoordinates = map (\(x, y) -> x + 100 * y) . map fst . filter (isBoxLeft . snd) . A.assocs

isBoxLeft :: Tile -> Bool
isBoxLeft b = b == Box || b == WideBoxLeft

------ PARSING 2 ------

widen :: String -> String
widen ('#' : xs) = '#' : '#' : widen xs
widen ('O' : xs) = '[' : ']' : widen xs
widen ('.' : xs) = '.' : '.' : widen xs
widen ('@' : xs) = '@' : '.' : widen xs
widen (c : xs) = c : widen xs
widen [] = []

------ PART 2 ------

-- Pushes multiple boxes by moving each box individually. Capable of working with boxes larger than a single tile
-- Each box reports the updates needed to move it (set positions it currently is to be empty, and the next positions to be occupied)
-- Need some care to ensure multiple moves in the same step don't overwrite a new block's position with another blocks empty
--   See the `sortOn` here which ensures all `Space` in a step is written before any `Block`
tryMove2 :: Grid -> Position -> Dir -> (Grid, Position)
tryMove2 g p d
  --  | trace (concat ["Grid is:\n", showGrid (placeRobot g p), "\nHave position ", show p, " and direction ", show d]) False = undefined
  | otherwise = case tryStep (posDir d p) of
      Nothing -> (g, p)
      Just x -> (g A.// (sortOn snd x), posDir d p)
  where
    tryStep :: Position -> Maybe [(Position, Tile)]
    tryStep nextPos = do
      box <- g A.!? nextPos
      case box of
        Space -> pure []
        Wall -> Nothing
        Box -> pushBox [nextPos] [Box]
        WideBoxLeft -> pushBox (boxPositions nextPos WideBoxLeft) wideBox
        WideBoxRight -> pushBox (boxPositions nextPos WideBoxRight) wideBox
        Robot -> error "Can't push a robot."

    pushBox :: [Position] -> [Tile] -> Maybe [(Position, Tile)]
    pushBox curPositions tiles = do
      let nextPositions = map (posDir d) curPositions
      -- Set next positions to be this block, and previous positions to be empty
      let updates = zipWith (,) nextPositions tiles ++ zipWith (,) curPositions (repeat Space)
      let alsoPush = filter (`notElem` curPositions) nextPositions
      foldM (\acc p' -> (acc ++) <$> tryStep p') updates alsoPush

wideBox :: [Tile]
wideBox = [WideBoxLeft, WideBoxRight]

boxPositions :: Position -> Tile -> [Position]
boxPositions p' Box = [p']
boxPositions p'@(x, y) WideBoxLeft = [p', (x + 1, y)]
boxPositions p'@(x, y) WideBoxRight = [(x - 1, y), p']
boxPositions _ _ = error "Not a box"

makeMoves2 :: Grid -> Position -> [Dir] -> (Grid, Position)
makeMoves2 g p = foldl' (\(g', p') d -> (tryMove2 g' p' d)) (g, p)