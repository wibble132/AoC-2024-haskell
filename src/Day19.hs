{-# LANGUAGE TypeFamilies #-}

module Day19 (part1, part2, readInput) where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (second))
import Data.Functor.Identity (Identity)
import Data.List (isPrefixOf, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Text.Parsec (ParsecT, char, many1, newline, parse, sepBy, sepEndBy, string)
import Utils (fromParseResult)
import Data.Maybe (fromMaybe)

part1 :: Input -> String
part1 (ts, ps) = show . length . filter (canBeMade4 ts) $ ps

part2 :: Input -> String
part2 (ts, ps) = show . sum . map (canBeMadeCounts ts) $ ps

readInput :: String -> Input
readInput = fromParseResult . parse input "Input"

------ PARSING ------

type Input = ([Towel], [Pattern])

type Towel = [Colour]

type Pattern = [Colour]

data Colour = White | Blue | Black | Red | Green deriving (Show, Eq, Enum)

input :: ParsecT String u Identity Input
input = do
  a <- available
  _ <- newline >> newline
  p <- patt
  pure (a, p)

available :: ParsecT String u Identity [Towel]
available = many1 colour `sepBy` string ", "

patt :: ParsecT String u Identity [Pattern]
patt = many1 colour `sepEndBy` newline

colour :: ParsecT String u Identity Colour
colour = charToColour <$> (char 'w' <|> char 'u' <|> char 'b' <|> char 'r' <|> char 'g')

charToColour :: Char -> Colour
charToColour 'w' = White
charToColour 'u' = Blue
charToColour 'b' = Black
charToColour 'r' = Red
charToColour 'g' = Green
charToColour c = error $ "Unknown colour " ++ [c]

------ PART 1 ------

canBeMade4 :: [Towel] -> Pattern -> Bool
canBeMade4 tt pa = go 0 (M.singleton 0 1) pa
  where
    grouped :: [(Int, [Pattern])]
    grouped = map ((second NE.toList) . ((,) =<< length . NE.head)) . NE.groupBy (\a b -> length a == length b) . sortOn length $ tt

    go :: Int -> Map Int Int -> Pattern -> Bool
    go i m [] = maybe False (> 0) (m M.!? i)
    go i m p@(_ : ps)
      | maybe True (== 0) (m M.!? i) = go (i + 1) m ps
      | otherwise = go (i + 1) x ps
      where
        curr = m M.! i
        x = foldl' (\acc (len, patts) -> M.insertWith (+) (len + i) ((* curr) . length . filter (`isPrefixOf` p) $ patts) acc) m grouped

------ PART 2 ------

-- Minor change to the solution to part 1
canBeMadeCounts :: [Towel] -> Pattern -> Int
canBeMadeCounts tt pa = go 0 (M.singleton 0 1) pa
  where
    grouped :: [(Int, [Pattern])]
    grouped = map ((second NE.toList) . ((,) =<< length . NE.head)) . NE.groupBy (\a b -> length a == length b) . sortOn length $ tt

    go :: Int -> Map Int Int -> Pattern -> Int
    go i m [] = fromMaybe 0 (m M.!? i)
    go i m p@(_ : ps)
      | maybe True (== 0) (m M.!? i) = go (i + 1) m ps
      | otherwise = go (i + 1) x ps
      where
        curr = m M.! i
        x = foldl' (\acc (len, patts) -> M.insertWith (+) (len + i) ((* curr) . length . filter (`isPrefixOf` p) $ patts) acc) m grouped
