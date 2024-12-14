module Day14 (part1, part2, readInput) where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow ((&&&)), (>>^))
import Control.Monad (liftM2)
import Data.Bifunctor (bimap)
import Data.Functor.Identity (Identity)
import Data.List (findIndex)
import Data.List.Split (chunk)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import Text.Parsec (ParsecT, char, digit, parse, sepEndBy, string)
import Text.Parsec.Char (newline)
import Text.Parsec.Prim (many)
import Utils (both, each, iterateN, (.:))

part1 :: Input -> String
part1 = show . product . countQuadrants bounds . roam bounds 100
  where
    -- Example input
    -- bounds :: (Int, Int) = (11, 7)
    -- Real input
    bounds :: (Int, Int) = (101, 103)

part2 :: Input -> String
part2 = show . searchForTree bounds
  where
    -- Example input
    -- bounds :: (Int, Int) = (11, 7)
    -- Real input
    bounds :: (Int, Int) = (101, 103)

------ PARSING ------

type Position = (Int, Int)

type Velocity = (Int, Int)

type Input = ([Position], [Velocity])

readInput :: String -> Input
readInput = (\case (Left e) -> error (show e); (Right x) -> x) . parse input "input"

input :: ParsecT String () Identity Input
input = unzip <$> liftM2 (,) buttonA buttonB `sepEndBy` newline

buttonA :: ParsecT String u Identity (Int, Int)
buttonA = numbers (string "p=") (string ",") (pure ())

buttonB :: ParsecT String u Identity (Int, Int)
buttonB = numbers (string " v=") (string ",") (pure ())

numbers ::
  ParsecT String u Identity a1 ->
  ParsecT String u Identity a2 ->
  ParsecT String u Identity a3 ->
  ParsecT String u Identity (Int, Int)
numbers prefix sep suffix = liftM2 (,) (prefix >> number) (sep >> number <* suffix)

number :: ParsecT String u Identity Int
number = read <$> (many (char '-' <|> digit))

------ PART 1 ------

roam :: (Int, Int) -> Int -> Input -> [Position]
roam bounds iterCount (ps, vs) = iterateN iterCount (roamStep bounds vs) ps

roamStep ::
  -- Bounds of the room
  (Int, Int) ->
  -- Velocities
  [Velocity] ->
  -- Initial positions
  [Position] ->
  -- Positions after one step
  [Position]
roamStep bounds = map (each (flip mod) bounds) .: zipWith (\x y -> uncurry bimap (both (+) x) y)

countQuadrants :: (Int, Int) -> [Position] -> [Int]
countQuadrants bounds ps = map (\f -> length . filter f $ ps) filters
  where
    xSplit = (`div` 2) . fst $ bounds
    ySplit = (`div` 2) . snd $ bounds

    isLeft, isRight, isTop, isBottom :: Position -> Bool
    isLeft = (< xSplit) . fst
    isRight = (> xSplit) . fst
    isTop = (< ySplit) . snd
    isBottom = (> ySplit) . snd

    filters :: [Position -> Bool]
    filters = [hori `andA` vert | hori <- [isLeft, isRight], vert <- [isTop, isBottom]]

andA :: (Arrow a) => a b Bool -> a b Bool -> a b Bool
andA f g = (f &&& g) >>^ uncurry (&&)

------ PART 2 ------

roaming :: (Int, Int) -> Input -> [[Position]]
roaming bounds (ps, vs) = iterate (roamStep bounds vs) ps

searchForTree :: (Int, Int) -> Input -> Maybe Int
searchForTree bounds xs = findIndex isTree $ zip [10000 ..] (drop 10000 . take 20000 $ roaming bounds xs)
  where

    -- Found a couple of approaches here work. 
    --  -- First can print out the images at each step, notice a couple of patterns and find when they intersect (e.g. with Chinese remainder th'm)
    --  -- Other method I tried is finding a step that minimises variance of the positions (I check the variance of x and y coordinates independently)
    --     -- Most of the time the x variance is over 800, every 101 steps it is down to about 375
    --  -- Another method (I didn't try this at first since I didn't expect it to work...) is to find steps when all the positions are unique. 
    -- I've commented out some of these, left it with the variance check
    isTree :: (Int, [Position]) -> Bool
    isTree (i, ps) =
      -- \| (i) == 8149 && trace (concat [show (i, v), "\n", drawPositions bounds ps, "\n\n\n"]) False = undefined
      -- S.size set == length ps
      vx < 500 && vy < 500
      where
        set = S.fromList ps

        vx = var (map fst ps)
        vy = var (map snd ps)

var :: (Real a) => [a] -> Double
var xs = v
  where
    len = toRational $ length xs
    mean = (toRational $ sum xs) / len
    v :: Double =
      fromRational
        . (/ (len - 1))
        . sum
        . map (\x -> (toRational x - mean) ^ (2 :: Int))
        $ xs

drawPositions :: (Int, Int) -> [Position] -> String
drawPositions (bx, by) ps = unlines . chunk bx $ [if (i, j) `elem` ps then 'H' else ' ' | j <- [0 .. by - 1], i <- [0 .. bx - 1]]
