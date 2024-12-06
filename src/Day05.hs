module Day05 (part1, part2) where

import Data.Functor.Identity (Identity)
import Data.List (sortBy)
import Text.Parsec (manyTill, newline, parse, sepBy1, sepEndBy1)
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec (char, digit, many1)

part1 :: String -> String
part1 = show . countValids . getData

part2 :: String -> String
part2 = show . sum . map midval . uncurry (map . makeValid) . getData

type Data = (Orderings, Updates)

type Orderings = [Order]

data Order = Order Int Int deriving (Show, Eq)

type Updates = [Update]

newtype Update = Update [Int] deriving (Show)

getData :: String -> Data
getData = throwError . parse parseData "input"
  where
    throwError :: (Show a) => Either a b -> b
    throwError (Left x) = error (show x)
    throwError (Right x) = x

parseData :: ParsecT String u Identity Data
parseData = do
  orderings <- parseOrderings
  updates <- parseUpdates
  return (orderings, updates)

parseOrderings :: ParsecT String u Identity Orderings
parseOrderings = parseOrder `manyTill` newline

parseOrder :: ParsecT String u Identity Order
parseOrder = do
  num1 <- many1 digit
  _ <- char '|'
  num2 <- many1 digit
  _ <- newline
  return $ Order (read num1) (read num2)

parseUpdates :: ParsecT String u Identity Updates
parseUpdates = parseUpdate `sepEndBy1` newline

parseUpdate :: ParsecT String u Identity Update
parseUpdate = do
  values <- many1 digit `sepBy1` char ','
  return . Update $ map read values

countValids :: Data -> Int
countValids (orderings, b) = sum . map midval . filter (isValid orderings) $ b

isValid :: Orderings -> Update -> Bool
isValid orderings (Update entries) = all (\(x, y) -> Order y x `notElem` orderings) $ orderedPairs entries

orderedPairs :: [a] -> [(a, a)]
orderedPairs (x : xs) = map (x,) xs ++ orderedPairs xs
orderedPairs [] = []

midval :: Update -> Int
midval (Update u) = mid u u
  where
    mid (_ : xs) (_ : _ : ys) = mid xs ys
    mid (x : _) _ = x
    mid a b = error $ "Bad list middle" ++ show (a, b, u)

makeValid :: Orderings -> Update -> Update
makeValid orderings (Update u) = Update $ sortBy (\a b -> if Order a b `elem` orderings then LT else GT) u