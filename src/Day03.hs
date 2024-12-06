module Day03 (part1, part2) where

import Control.Applicative ((<|>))
import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT, unexpected)
import Text.ParserCombinators.Parsec (ParseError, many, parse, try)
import Text.ParserCombinators.Parsec.Char
  ( anyChar,
    char,
    digit,
    string,
  )

type ParsedInput = [ParsedInputItem]

data ParsedInputItem
  = Mul Integer Integer
  | Do
  | Dont
  | Junk Char
  deriving (Show)

part1 :: String -> String
part1 = show . sum . map getValue . fromRight [] . parseInput
  where
    getValue :: ParsedInputItem -> Integer
    getValue (Mul x y) = x * y
    getValue _ = 0

part2 :: String -> String
part2 = show . snd . foldl getValue (True, 0) . fromRight [] . parseInput
  where
    getValue :: (Bool, Integer) -> ParsedInputItem -> (Bool, Integer)
    getValue (True, tot) (Mul x y) = (True, tot + x * y)
    getValue (False, tot) (Mul _ _) = (False, tot)
    getValue x (Junk _) = x
    getValue (_, tot) Do = (True, tot)
    getValue (_, tot) Dont = (False, tot)

parseInput :: [Char] -> Either ParseError ParsedInput
parseInput =
  parse
    ( many $
        try parseMul
          <|> try parseDont
          <|> try parseDo
          <|> parseJunk
    )
    "input"

parseJunk :: ParsecT [Char] u Identity ParsedInputItem
parseJunk = Junk <$> anyChar

parseMul :: ParsecT [Char] u Identity ParsedInputItem
parseMul = do
  _ <- string "mul("
  num1 <- parseNum
  _ <- char ','
  num2 <- parseNum
  _ <- char ')'
  return $ Mul (read num1) (read num2)

parseNum :: ParsecT [Char] u Identity [Char]
parseNum = do
  num <- many digit
  if not (null num) && length num <= 3
    then return num
    else unexpected "Wrong number of digits"

parseDo :: ParsecT [Char] u Identity ParsedInputItem
parseDo = do
  _ <- string "do()"
  return Do

parseDont :: ParsecT [Char] u Identity ParsedInputItem
parseDont = do
  _ <- string "don't()"
  return Dont