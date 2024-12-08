{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- Have an old solution in `canSolve` left here

module Day07 (part1, part2, readInput) where

import Control.Monad (foldM)
import Data.Functor.Identity (Identity)
import Data.Maybe (mapMaybe)
import Text.Parsec (ParseError, char, digit, many1, newline, parse, sepBy, sepEndBy, space)
import Text.Parsec.Prim (ParsecT)

part1 :: [Equation] -> Int
-- part1 = sum . map result . filter (`canSolve` [(*), (+)])
part1 = sum . map result . filter (`canSolve2` [unAdd, unMul])

part2 :: [Equation] -> Int
part2 =
  sum
    . map result
    -- Turns out it is faster if I do the part1 checks first (eliminates 376 of 850 entries)
    -- . filter ((`canSolve` [(*), (+)]) `orA` (`canSolve` [concatInt, (*), (+)]))
    .filter (`canSolve2` [unAdd, unMul, unConcat])

data Equation = Equation {result :: Int, inputs :: [Int]} deriving (Show)

readInput :: String -> [Equation]
readInput s = case parseInput s of
  Left err -> error (show err)
  Right res -> res

parseInput :: String -> Either ParseError [Equation]
parseInput = parse (parseLine `sepEndBy` newline) "input"

parseLine :: ParsecT String u Identity Equation
parseLine = do
  result <- int
  _ <- char ':'
  _ <- space
  inputs <- int `sepBy` char ' '
  return $ Equation result inputs

int :: ParsecT String u Identity Int
int = read <$> many1 digit

type Operator = (Int -> Int -> Int)

canSolve :: Equation -> [Operator] -> Bool
canSolve (Equation res (in1 : inps)) ops = step in1 inps
  where
    step :: Int -> [Int] -> Bool
    -- All operators increase, so can short-circuit an overflow
    step tot _ | tot > res = False
    -- Branch for all possible next-operators
    step tot (x : xs) = any (\op -> step (op tot x) xs) ops
    -- Finished inputs, want the correct result
    step tot [] = tot == res
canSolve (Equation _ []) _ = False

type UnOperator = Int -> Int -> Maybe Int

canSolve2 :: Equation -> [UnOperator] -> Bool
canSolve2 (Equation _ []) _ = False
canSolve2 (Equation res (x : xs)) ops = x `elem` foldM step res xs
  where
    step :: Int -> Int -> [Int]
    step tot next = mapMaybe (\op -> op tot next) ops

unAdd :: Int -> Int -> Maybe Int
unAdd a b
  | a < b = Nothing
  | otherwise = Just $ a - b

unMul :: Int -> Int -> Maybe Int
unMul a b
  | a `mod` b /= 0 = Nothing
  | otherwise = Just $ a `div` b

unConcat :: Int -> Int -> Maybe Int
unConcat a b
  | a == b = Just 0
  | aTail == bStr = Just $ read aStart
  | otherwise = Nothing
  where
    aStr = show a
    bStr = show b
    aLen = length aStr
    bLen = length bStr
    (aStart, aTail) = splitAt (aLen - bLen) aStr
