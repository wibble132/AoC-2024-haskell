module Day21 (part1, part2, readInput) where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.MemoTrie (memo3)
import Debug.Trace (trace)
import Utils (Direction, Position, windows)
import Utils qualified as D

part1 :: Input -> String
part1 =
  show
    . sum
    . map
      ( uncurry (*)
          . bimap (solve 2) (numericValue)
          . join (,)
      )

part2 :: Input -> String
part2 =
  show
    . sum
    . map
      ( uncurry (*)
          . bimap (solve 25) (numericValue)
          . join (,)
      )

readInput :: String -> Input
readInput = map (map digitToInt . init) . lines

------ PARSING ------

type Input = [Code]

type Code = [Int]

data Button = Dir Direction | Apply

instance Show Button where
  show :: Button -> String
  show (Dir D.Up) = "^"
  show (Dir D.Down) = "v"
  show (Dir D.Left) = "<"
  show (Dir D.Right) = ">"
  show Apply = "A"

-- Used for debugging
_buttonsShow :: [Button] -> String
_buttonsShow = concatMap show

-- Used for debugging
_traceButtons :: [Button] -> [Button]
_traceButtons x = trace (_buttonsShow x) x

------ PART 1 ------

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)
toPair _ = error "Invalid pair"

codeToPos :: Code -> [Position]
codeToPos = (++ [(0, 0)]) . map go
  where
    go :: Int -> Position
    go 0 = (-1, -0)
    go 1 = (-2, -1)
    go 2 = (-1, -1)
    go 3 = (-0, -1)
    go 4 = (-2, -2)
    go 5 = (-1, -2)
    go 6 = (-0, -2)
    go 7 = (-2, -3)
    go 8 = (-1, -3)
    go 9 = (-0, -3)
    go _ = error "Invalid code"

buttonToPos :: Button -> Position
buttonToPos (Dir d) = dirToPos d
buttonToPos Apply = (0, 0)

dirToPos :: Direction -> Position
dirToPos D.Up = (-1, 0)
dirToPos D.Right = (0, 1)
dirToPos D.Down = (-1, 1)
dirToPos D.Left = (-2, 1)

solve :: Int -> Code -> Int
solve depth =
  sum
    . map (uncurry (stepsToReach depth))
    . map toPair
    . windows 2
    . (buttonToPos Apply :)
    . codeToPos
  where
    stepsToReach ::
      -- Max Depth
      Int ->
      -- Starting and Ending positions
      Position ->
      Position ->
      -- Number of inputs to press on outermost keypad
      Int
    stepsToReach = memo3 stepsToReachInner
    stepsToReachInner maxDepth p0 p1
      | p0 == p1 = 1 -- Apply
      | not (canHoriFirst || canVertFirst) = error "oops something wrong"
      | maxDepth == 0 = abs (fst p0 - fst p1) + abs (snd p0 - snd p1) + 1
      | otherwise = recursiveSolve
      where
        verts = replicate (snd p0 - snd p1) D.Up ++ replicate (snd p1 - snd p0) D.Down
        horis = replicate (fst p1 - fst p0) D.Right ++ replicate (fst p0 - fst p1) D.Left

        vertFirst = map Dir (verts ++ horis) ++ [Apply]
        horiFirst = map Dir (horis ++ verts) ++ [Apply]

        badPos = (-2, 0)
        canVertFirst = snd p0 /= snd p1 && (fst p0, snd p1) /= badPos
        canHoriFirst = fst p0 /= fst p1 && (fst p1, snd p0) /= badPos

        doRecurse :: [Button] -> Int
        doRecurse =
          sum
            -- Then apply recursively
            . map (uncurry (stepsToReach (maxDepth - 1)) . toPair)
            . windows 2
            -- Change to Positions
            . map buttonToPos
            -- Start at apply button
            . (Apply :)
        -- . (trace =<< buttonsShow)

        recursiveSolve
          | not canHoriFirst = doRecurse vertFirst
          | not canVertFirst = doRecurse horiFirst
          | otherwise = min (doRecurse vertFirst) (doRecurse horiFirst)

numericValue :: Code -> Int
numericValue = read . concatMap show

------ PART 2 ------
-- Just modified the p1 code to be faster