module Day09 (part1, part2, readInput1, readInput2) where

import Control.Monad (liftM2)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.List (find)
import Data.Vector (Vector)
import Data.Vector qualified as V

-- import Debug.Trace (trace)
trace :: String -> a -> a
trace _ a = a 

part1 :: [Block] -> Integer
part1 bs = result
  where
    orderBlocks :: [Block] -> [Block] -> [Int]
    orderBlocks xs@(Gap : _) (Gap : ys) = orderBlocks xs ys
    orderBlocks (Gap : xs) (FileBlock y : ys) = y : orderBlocks xs ys
    orderBlocks (FileBlock x : xs) ys = x : orderBlocks xs ys
    orderBlocks [] _ = []
    orderBlocks _ [] = undefined

    blockCount :: Int
    blockCount = length . filter (/= Gap) $ bs

    orderedBlocks :: [Integer]
    orderedBlocks = map toInteger . take blockCount $ orderBlocks bs (reverse bs)

    result :: Integer
    result = foldl' (\tot (i, b) -> tot + i * b) 0 . zip [0 ..] $ orderedBlocks

data Block = Gap | FileBlock {fbId :: Int} deriving (Show, Eq)

readInput1 :: [Char] -> [Block]
readInput1 s = inner s 0
  where
    inner :: [Char] -> Int -> [Block]
    inner (x : y : xs) i = rep x (FileBlock i) ++ rep y Gap ++ inner xs (i + 1)
    inner [x] i = rep x (FileBlock i)
    inner [] _ = []

    rep d = replicate (digitToInt d)

part2 :: ([File], [Space]) -> String
part2 = show . checksum2 . uncurry order2 -- (fs, ss) = show fs ++ ['\n'] ++ show ss

data File = File {fId :: Int, fPos :: Int, fLength :: Int} deriving (Show)

data Space = Space {sPos :: Int, sLength :: Int} deriving (Show)

readInput2 :: String -> ([File], [Space])
readInput2 s = bimap makeFiles makeSpaces $ alternating withPositions
  where
    nums = map digitToInt s

    positions :: [Int]
    positions = getPositions nums -- reverse $ foldl' (\a b -> case a of [] -> [0]; (x : _) -> x + b : a) [] nums
    withPositions :: [(Int, Int)] -- [(position, length)]
    withPositions = trace (show nums) $ trace (show positions) $ positions `zip` nums

    makeFiles :: [(Int, Int)] -> [File]
    makeFiles = zipWith (\i (pos, len) -> File i pos len) [0 ..]
    makeSpaces :: [(Int, Int)] -> [Space]
    makeSpaces = map (uncurry Space)

getPositions :: [Int] -> [Int]
getPositions = inner 0
  where
    inner :: Int -> [Int] -> [Int]
    inner prev (x : xs) = prev : inner (prev + x) xs
    inner prev [] = [prev]

alternating :: [a] -> ([a], [a])
alternating (x : y : xs) = bimap (x :) (y :) $ alternating xs
alternating [x] = ([x], [])
alternating [] = ([], [])

order2 :: [File] -> [Space] -> [File]
order2 files sps = newFiles
  where
    revFiles = reverse files

    newFiles = snd $ foldl' doStep (V.fromList sps, []) revFiles

    doStep :: (Vector Space, [File]) -> File -> (Vector Space, [File])
    doStep (spaces, fs) nextFile = (nextSpaces, movedFile : fs)
      where
        (nextSpaces, movedFile) = step (squashSpaces spaces) nextFile

    step :: Vector Space -> File -> (Vector Space, File)
    step !spaces file
      | trace ("Step: " ++ show (V.map (liftM2 (,) sPos sLength) spaces)) False = undefined
      | Just (idx, space) <- fittingSpace =
          trace
            ("Moving " ++ show file ++ " to " ++ show idx ++ ", " ++ show space)
            (spaces V.// [(idx, space `reduceBy` fLength file)], file `moveTo` sPos space)
      | otherwise = (spaces, file)
      where
        -- The first space before this file that is larger than the file
        fittingSpace =
          find (\(_, s) -> sLength s >= fLength file)
            . zip [0 ..]
            . takeWhile (\s -> sPos s < fPos file)
            . V.toList
            $ spaces

moveTo :: File -> Int -> File
moveTo (File fId _ fLength) newPos
  | trace ("Moving to " ++ show newPos) False = undefined
  | otherwise = File fId newPos fLength

reduceBy :: Space -> Int -> Space
reduceBy (Space sPos sLength) i = Space (sPos + i) (sLength - i)

squashSpaces :: Vector Space -> Vector Space
squashSpaces = V.fromList . inner . V.toList
  where
    inner :: [Space] -> [Space]
    -- Remove spaces of length 0
    inner (a : xs) | sLength a == 0 = inner xs
    -- Merge adjacent spaces
    inner (a : b : xs) | sPos b == sPos a + sLength a = Space (sPos a) (sLength a + sLength b) : inner xs
    -- Otherwise continue iterating
    inner (a : xs) = a : inner xs
    -- Finished
    inner [] = []

checksum2 :: [File] -> Int
checksum2 = foldl' (\acc file -> acc + checksumFile file) 0

-- Can be improved, but this should work
checksumFile :: File -> Int
checksumFile file = fId file * sum (take (fLength file) [fPos file ..])
