module Day22 (part1, part2, readInput) where

import Data.Bits (Bits (xor))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Utils (pairWindows, windows)

part1 :: Input -> String
part1 = show . sum . map ((!! 2000) . iterate nextSecret)

-- 2024 too high :(
part2 :: Input -> String
part2 = show . bestSellScore

readInput :: String -> Input
readInput = map read . lines

------ PARSING ------

-- Trivial, done inline

type Input = [Int]

------ PART 1 ------

nextSecret :: (Integral a, Bits a) => a -> a
nextSecret x = x3
  where
    x1 = x `mixAndPrune` (x * 64)
    x2 = x1 `mixAndPrune` (x1 `div` 32)
    x3 = x2 `mixAndPrune` (x2 * 2048)

mixAndPrune :: (Integral a, Bits a) => a -> a -> a
mixAndPrune a b = prune (a `mix` b)

mix :: (Bits a) => a -> a -> a
mix = xor

prune :: (Integral a) => a -> a
prune = (`mod` 16777216)

------ PART 2 ------

windows4 :: [a] -> [(a, a, a, a)]
windows4 = map toQuad . windows 4
  where
    toQuad [x1, x2, x3, x4] = (x1, x2, x3, x4)
    toQuad _ = error "Logic error"

bestSellScore :: Input -> Int
bestSellScore i = maximum . M.elems $ combined
  where
    priceMaps = map (sellPrices . prices) $ i
    combined = foldl' (M.unionWith (+)) M.empty priceMaps

sellPrices :: [Int] -> Map (Int, Int, Int, Int) Int
sellPrices ps = M.fromListWith (const id) $ zip diffs (drop 4 ps)
  where
    diffs = windows4 . map (uncurry (flip (-))) . pairWindows $ ps

prices :: Int -> [Int]
prices = map (`mod` 10) . take 2000 . iterate nextSecret
