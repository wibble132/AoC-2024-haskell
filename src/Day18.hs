module Day18 (part1, part2, readInput) where

import Algorithm.Search (dijkstra)
import Data.Functor.Identity (Identity)
import Data.Ix qualified as Ix
import Data.List (inits, find)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as S
import Text.Parsec (ParsecT, char, newline, parse, sepEndBy)
import Utils (Position, fromParseResult, number)

gridBounds :: (Position, Position)
gridBounds =
  -- Example
  -- ((0, 0), (6, 6))

  -- Real
  ((0, 0), (70, 70))

part1 :: Input -> String
part1 = show . (fst <$>) . findPath gridBounds . S.fromList . take 1024

part2 :: Input -> String
part2 = show . (last <$>) . find (isNothing . findPath gridBounds . S.fromList) . drop 1024 . inits

readInput :: String -> Input
readInput = fromParseResult . parse (input `sepEndBy` newline) "Input"

------ PARSING ------

type Input = [Position]

input :: ParsecT String u Identity Position
input = do
  n1 <- number
  _ <- char ','
  n2 <- number
  pure (n1, n2)

------ PART 1 ------

findPath :: (Position, Position) -> Set Position -> Maybe (Int, [Position])
findPath bounds points = dijkstra neighbours (const (const 1)) (== (snd bounds)) (fst bounds)
  where
    neighbours :: Position -> [Position]
    neighbours (x, y) =
      filter (`S.notMember` points)
        . filter (Ix.inRange bounds)
        $ [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

------ PART 2 ------