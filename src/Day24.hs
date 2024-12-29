{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day24 (part1, part2, readInput) where

import Data.Bifunctor (second)
import Data.Bits (Bits (bit, testBit), FiniteBits (finiteBitSize), (.<<.))
import Data.Char (toUpper)
import Data.Functor.Identity (Identity)
import Data.List (intercalate, sort, sortOn, uncons)
import Data.List.Split (chunk)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Debug.Trace (trace)
import Text.Parsec (ParsecT, anyChar, char, digit, eof, lookAhead, many, manyTill, newline, parse, space, string, try)
import Text.Printf (printf)
import Utils (fromParseResult)

part1 :: Input -> String
part1 = show . extractResult . run

-- nnt, gws
-- kvr, fmh
part2 :: Input -> String
-- part2 (a, b) = unlines . map show $ sortOn (\g -> out g) b
-- part2 = searchForIssues . snd
-- part2 = testSingleBitPairs . second (swapOutputs "z19" "cph" .swapOutputs "npf" "z13" . swapOutputs "z33" "hgj" . swapOutputs "nnt" "gws")
part2 _ = intercalate "," $ sort ["z19", "cph", "npf", "z13", "z33", "hgj", "nnt", "gws"]

readInput :: String -> Input
readInput = fromParseResult . parse input "Input"

------ PARSING ------

type Input = (Map String Bool, [Gate])

data Wire = Wire String Bool deriving (Show)

wireToTuple :: Wire -> (String, Bool)
wireToTuple (Wire s b) = (s, b)

data Gate = Gate {op :: Operation, in1 :: String, in2 :: String, out :: String}

instance Show Gate where
  show :: Gate -> String
  show (Gate op in1 in2 out) = concat [in1, " ", map toUpper $ show op, " ", in2, " -> ", out]

data Operation = And | Or | Xor deriving (Show, Eq, Ord)

input :: ParsecT String u Identity Input
input = do
  -- I need more practice at writing these, this is an awful mess :(
  initial <- (wire <* newline) `manyTill` newline
  gates <- manyTill (gate <* newline) (lookAhead $ try (many space >> eof))

  let initialMap = M.fromList . map wireToTuple $ initial
  pure (initialMap, gates)

wire :: ParsecT String u Identity Wire
wire = do
  name <- manyTill anyChar (char ':')
  _ <- space
  val <- digit
  case val of
    '0' -> pure $ Wire name False
    '1' -> pure $ Wire name True
    _ -> fail (show val)

gate :: ParsecT String u Identity Gate
gate = do
  -- _ <- parserTrace "gate"
  x1 <- manyTill anyChar (char ' ')
  op <- operation
  x2 <- manyTill anyChar (char ' ')
  _ <- string "-> "
  out <- manyTill anyChar (lookAhead newline)
  pure $ Gate op x1 x2 out

operation :: ParsecT String u Identity Operation
operation = do
  -- _ <- parserTrace "op"
  op <- manyTill anyChar (char ' ')
  case op of
    "AND" -> pure And
    "OR" -> pure Or
    "XOR" -> pure Xor
    _ -> fail (show op)

------ PART 1 ------

run :: Input -> Map String Bool
run = go
  where
    go :: (Map String Bool, [Gate]) -> Map String Bool
    go x = case step x of
      (m, []) -> m
      x' -> go x'

    step :: (Map String Bool, [Gate]) -> (Map String Bool, [Gate])
    step (m, gs)
      | length remaining == length gs = error $ concat ["Nothing changed.\n  Current:\n   ", show m, "\n  Results:\n   ", show results, "\n    ", show updates, "\n    ", show remaining]
      | otherwise =
          --   trace
          -- (concat ["Results:\n ", show results, "\n  ", show updates, "\n  ", show remaining])
          (m `M.union` updates, remaining)
      where
        results :: [(Gate, Maybe (String, Bool))]
        results = map ((,) <*> runGate) gs

        updates = M.fromList $ mapMaybe snd results
        remaining = map fst . filter (isNothing . snd) $ results

        runGate :: Gate -> Maybe (String, Bool)
        runGate (Gate op in1 in2 out)
          | isNothing val1 || isNothing val2 = Nothing
          | otherwise = Just (out, runOp op v1 v2)
          where
            val1 = m M.!? in1
            val2 = m M.!? in2

            v1 = fromJust val1
            v2 = fromJust val2

runOp :: Operation -> Bool -> Bool -> Bool
runOp And = (&&)
runOp Or = (||)
runOp Xor = (/=)

extractResult :: Map String Bool -> (Int)
extractResult =
  sum
    . map (bit . read . snd . fromJust)
    . M.keys
    . M.filterWithKey isResultKey
    . M.mapKeys uncons
  where
    isResultKey (Just ('z', _)) x = x
    isResultKey _ _ = False

------ PART 2 ------

testSingleBitPairs :: Input -> String
testSingleBitPairs (i, gs) = unlines . chunk (bitCounts + 1) $ map (\case True -> '.'; False -> 'x') results
  where
    bitCounts = length i `div` 2
    values = 0 : [bit x | x <- [0 .. bitCounts - 1]]
    numsToTest = [(x, y) | x <- values, y <- values]

    results = map (\(x, y) -> (x + y) == test gs x y) numsToTest

testBitsFirst :: Input -> String
testBitsFirst (i, gs) = show . filter (uncurry (/=)) $ (expected `zip` results)
  where
    secondVal = 0

    bitCounts = length i `div` 2
    values = 0 : [bit x | x <- [0 .. bitCounts - 1]]
    numsToTest = [(x, secondVal) | x <- values]

    results = map (\(x, y) -> test gs x y) numsToTest
    expected = map (+ secondVal) values

testBitsSecond :: Input -> String
testBitsSecond (i, gs) = show . filter (uncurry (/=)) $ (expected `zip` results)
  where
    firstVal = 0

    bitCounts = length i `div` 2
    values = 0 : [bit x | x <- [0 .. bitCounts - 1]]
    numsToTest = [(firstVal, x) | x <- values]

    results = map (\(x, y) -> test gs x y) numsToTest
    expected = map (firstVal +) values

test ::
  -- The logic gates
  [Gate] ->
  -- The two inputs x and y
  Int ->
  Int ->
  -- The result
  Int
test gates x y = result
  where
    inputX = map (\i -> (('x' : fmt i), testBit x i)) [0 .. finiteBitSize x]
    inputY = map (\i -> (('y' : fmt i), testBit y i)) [0 .. finiteBitSize y]

    result = extractResult . run $ (M.fromList (inputX ++ inputY), gates)

fmt :: Int -> String
fmt = printf "%02d"

rename :: String -> String -> [Gate] -> [Gate]
rename old new gs = trace (concat ["Rename ", old, ", ", new]) map f gs
  where
    f g@(Gate _ a b c) = g {in1 = change a, in2 = change b, out = change c}
    change x
      | x == old = new
      | otherwise = x

swapOutputs :: String -> String -> [Gate] -> [Gate]
swapOutputs o1 o2 gs = map f gs
  where
    f g@(Gate _ _ _ o) = g {out = newName o}
    newName o
      | o == o1 = o2
      | o == o2 = o1
      | otherwise = o

searchForIssues :: [Gate] -> String
searchForIssues gs = unlines . map show $ renamedGates1
  where
    inputNames = map (\i -> ('x' : fmt i, 'y' : fmt i)) [0 .. 44]
    directGates = zip [0 ..] . map (sortOn op) . map (\(x, y) -> filter (\g -> (in1 g == x || in1 g == y) && (in2 g == x || in2 g == y)) gs) $ inputNames

    -- Replace the immediate output of `xNN XOR yNN` and `xNN AND yNN`
    renamedGates1 = foldl' step gs directGates
      where
        step acc (i, [a, x]) = rename (out a) ('a' : fmt i) . rename (out x) ('b' : fmt i) $ acc
        step _ _ = undefined
