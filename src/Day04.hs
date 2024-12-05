module Day04 (part1, part2) where

import GHC.Arr qualified as Arr

part1 :: String -> String
part1 = show . findXmas . parseGrid

part2 :: String -> String
part2 = show . findCrossMas . parseGrid

parseGrid :: String -> Arr.Array (Int, Int) Char
parseGrid input = Arr.array ((0, 0), ((length . lines $ input) - 1, (length . head . lines $ input) - 1)) . withIndex2D . lines $ input

withIndex2D :: [[a]] -> [((Int, Int), a)]
withIndex2D = concat . zipWith (\i xs -> zipWith (\j y -> ((i, j), y)) [0 ..] xs) [0 ..]

findXmas :: Arr.Array (Int, Int) Char -> Int
findXmas a = sum . map (findXmasFrom a) $ Arr.indices a

findXmasFrom :: Arr.Array (Int, Int) Char -> (Int, Int) -> Int
findXmasFrom a (x, y) = horizontal + diagonalDown + vertical + diagonalUp
  where
    horizontal :: Int
      | Arr.inRange (Arr.bounds a) (x + 3, y) = fromEnum $ horizontalLine == "XMAS" || horizontalLine == "SAMX"
      | otherwise = 0
    diagonalDown :: Int
      | Arr.inRange (Arr.bounds a) (x + 3, y + 3) = fromEnum $ diagonalDownLine == "XMAS" || diagonalDownLine == "SAMX"
      | otherwise = 0
    vertical :: Int
      | Arr.inRange (Arr.bounds a) (x, y + 3) = fromEnum $ verticalLine == "XMAS" || verticalLine == "SAMX"
      | otherwise = 0
    diagonalUp :: Int
    diagonalUp
      | Arr.inRange (Arr.bounds a) (x + 3, y - 3) = fromEnum $ diagonalUpLine == "XMAS" || diagonalUpLine == "SAMX"
      | otherwise = 0

    horizontalLine, diagonalDownLine, verticalLine :: String
    horizontalLine = [a Arr.! (x, y), a Arr.! (x + 1, y), a Arr.! (x + 2, y), a Arr.! (x + 3, y)]
    diagonalDownLine = [a Arr.! (x, y), a Arr.! (x + 1, y + 1), a Arr.! (x + 2, y + 2), a Arr.! (x + 3, y + 3)]
    verticalLine = [a Arr.! (x, y), a Arr.! (x, y + 1), a Arr.! (x, y + 2), a Arr.! (x, y + 3)]
    diagonalUpLine = [a Arr.! (x, y), a Arr.! (x + 1, y - 1), a Arr.! (x + 2, y - 2), a Arr.! (x + 3, y - 3)]

findCrossMas :: Arr.Array (Int, Int) Char -> Int
findCrossMas a = sum . map (findCrossMasFrom a) $ Arr.indices a

findCrossMasFrom :: Arr.Array (Int, Int) Char -> (Int, Int) -> Int
findCrossMasFrom a (x, y) = fromEnum $ diagonalDown && diagonalUp
  where
    diagonalDown :: Bool
      | Arr.inRange (Arr.bounds a) (x + 2, y + 2) = diagonalDownLine == "MAS" || diagonalDownLine == "SAM"
      | otherwise = False
    diagonalUp :: Bool
      | Arr.inRange (Arr.bounds a) (x + 2, y + 2) = diagonalUpLine == "MAS" || diagonalUpLine == "SAM"
      | otherwise = False

    diagonalDownLine = [a Arr.! (x, y), a Arr.! (x + 1, y + 1), a Arr.! (x + 2, y + 2)]
    diagonalUpLine = [a Arr.! (x, y + 2), a Arr.! (x + 1, y + 1), a Arr.! (x + 2, y)]