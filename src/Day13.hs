module Day13 (part1, part2, readInput) where

import Control.Monad (join, liftM2, liftM3)
import Data.Bifunctor (bimap)
import Data.Functor.Identity (Identity)
import Data.Maybe (mapMaybe)
import Text.Parsec (ParsecT, newline, parse, sepBy, string)
import Text.Parsec.Char (digit)
import Text.Parsec.Prim (many)

part1 :: Input -> String
part1 = show . sum . map score . filter isReasonable . mapMaybe (findButtonCounts)

part2 :: Input -> String
part2 = show . sum . map score . mapMaybe (findButtonCounts) . map (mapPrize (+ 10000000000000))

data ClawMachine = ClawMachine
  { cwButtonA :: (Integer, Integer),
    cwButtonB :: (Integer, Integer),
    cwPrize :: (Integer, Integer)
  }
  deriving (Show)

------ PARSING ------

type Input = [ClawMachine]

readInput :: String -> Input
readInput = (\case (Left e) -> error (show e); (Right x) -> x) . parse input "input"

input :: ParsecT String () Identity Input
input = liftM3 ClawMachine buttonA buttonB prize `sepBy` newline

buttonA :: ParsecT String u Identity (Integer, Integer)
buttonA = numbers (string "Button A: X+") (string ", Y+") (newline)

buttonB :: ParsecT String u Identity (Integer, Integer)
buttonB = numbers (string "Button B: X+") (string ", Y+") (newline)

prize :: ParsecT String u Identity (Integer, Integer)
prize = numbers (string "Prize: X=") (string ", Y=") (newline)

numbers ::
  ParsecT String u Identity a1 ->
  ParsecT String u Identity a2 ->
  ParsecT String u Identity a3 ->
  ParsecT String u Identity (Integer, Integer)
numbers prefix sep suffix = liftM2 (,) (prefix >> number) (sep >> number <* suffix)

number :: ParsecT String u Identity Integer
number = read <$> (many digit)

------ PART 1 ------

isReasonable :: (Integer, Integer) -> Bool
isReasonable = uncurry (&&) . both (<= 100)

score :: (Integer, Integer) -> Integer
score (a, b) = 3 * a + b

findButtonCounts :: ClawMachine -> Maybe (Integer, Integer)
findButtonCounts m
  | mDet == 0 = error "Not needed lmao"
  | check == (False, False) = Nothing
  | otherwise = Just (aInv, bInv)
  where
    (ma1, ma2) = cwButtonA m
    (mb1, mb2) = cwButtonB m
    mDet = ma1 * mb2 - ma2 * mb1

    -- If det /= 0, solve uniquely
    (pa, pb) = both fromIntegral $ cwPrize m

    aInv :: Integer
    aInv = (mb2 * pa - mb1 * pb) `div` mDet
    bInv :: Integer
    bInv = (ma1 * pb - ma2 * pa) `div` mDet

    -- Ensure that the solution is correct (if not integer soln, then rounding will break it)
    check =
      ( ma1 * aInv + mb1 * bInv == pa,
        ma2 * aInv + mb2 * bInv == pb
      )

both :: (a -> b) -> (a, a) -> (b, b)
both = join bimap

------ PART 2 ------

mapPrize :: (Integer -> Integer) -> ClawMachine -> ClawMachine
mapPrize f m = ClawMachine (cwButtonA m) (cwButtonB m) (both f . cwPrize $ m)