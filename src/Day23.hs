module Day23 (part1, part2, readInput) where

import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (find, intercalate, maximumBy)
import Data.List.Split (splitOneOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)
import Utils (listToPair)

part1 :: Input -> String
part1 = show . length . filter (any (headIs 't')) . findTriples

part2 :: Input -> String
part2 = intercalate "," . largestClique

readInput :: String -> Input
readInput = pairsToMaps . readPairs

------ PARSING ------

type Input = Map String (Set String)

headIs :: (Eq a) => a -> [a] -> Bool
headIs _ [] = False
headIs x (y : _) = x == y

readPairs :: String -> [(String, String)]
readPairs = map (listToPair . splitOneOf "-") . lines

pairsToMaps :: [(String, String)] -> Map String (Set String)
pairsToMaps = M.fromListWith S.union . map (second S.singleton) . ((++) <*> (map swap))

------ PART 1 ------

-- choose :: Int -> [a] -> [[a]]
-- choose _ [] = []
-- choose 0 _ = []
-- choose 1 xs = map (: []) xs
-- choose n (x : xs) = map (x :) (choose (n - 1) xs) ++ choose n xs

-- From stackOverflow looking for better functions than above https://stackoverflow.com/a/59932616
combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 1 as = map pure as
combinationsOf k as@(x : xs) = run (l - 1) (k - 1) as $ combinationsOf (k - 1) xs
  where
    l = length as

    run :: Int -> Int -> [a] -> [[a]] -> [[a]]
    run n k ys cs
      | n == k = map (ys ++) cs
      | otherwise = map (q :) cs ++ run (n - 1) k qs (drop dc cs)
      where
        (q : qs) = take (n - k + 1) ys
        dc = product [(n - k + 1) .. (n - 1)] `div` product [1 .. (k - 1)]
combinationsOf _ _ = undefined

-- This can be made much faster by looking at neighbourhoods of each node,
--  instead of all triples of nodes. This works tho
findTriples :: Input -> [[String]]
findTriples m = filter isValid allTriples
  where
    allTriples = combinationsOf 3 . M.keys $ m

    isValid [x, y, z] = x ~ y && y ~ z && x ~ z
    isValid _ = error "Logic error"

    (~) x y = maybe False (y `S.member`) $ m M.!? x

------ PART 2 ------

largestClique :: Input -> [String]
largestClique m = maximumBy (compare `on` length) cliquesForNode
  where
    nodes = M.keys m
    cliquesForNode = map largestCliqueWithNode nodes

    largestCliqueWithNode :: String -> [String]
    largestCliqueWithNode node = node : fromMaybe [] firstClique
      where
        neighbours = m M.! node
        size = S.size neighbours
        subsets = concatMap (\n -> combinationsOf n (S.toList neighbours)) . reverse $ [0 .. size]
        firstClique = find isClique subsets

    isClique [] = True
    isClique (x : xs) = all (~ x) xs && isClique xs

    (~) x y = maybe False (y `S.member`) $ m M.!? x
