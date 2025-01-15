module Day03 (part1, part2) where

import Control.Applicative (Alternative (empty), asum, (<|>))
import Control.Monad (guard)
import Data.Bifunctor (first, second)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT, anyChar, char, digit, getState, many, modifyState, runParser, string, try)

part1 :: String -> String
part1 = show . fromRight 0 . runParser doParse1 (True, 0) "input"

part2 :: String -> String
part2 = show . fromRight 0 . runParser doParse2 (True, 0) "input"

doParse1 :: ParsecT String State Identity Integer
doParse1 = do
  _ <- many $ try parseMul <|> parseJunk
  (_, res) <- getState
  pure res

doParse2 :: ParsecT String State Identity Integer
doParse2 = do
  _ <- many $ do
    (b, _) <- getState
    asum
      [ f b (try parseMul),
        try parseDont,
        try parseDo,
        parseJunk
      ]
  (_, res) <- getState
  pure res

f :: (Alternative m) => Bool -> m a -> m a
f b x = if b then x else empty

parseJunk :: ParsecT [Char] State Identity ()
parseJunk = anyChar >> pure ()

parseMul :: ParsecT [Char] State Identity ()
parseMul = do
  _ <- string "mul("
  num1 <- parseNum
  _ <- char ','
  num2 <- parseNum
  _ <- char ')'
  modifyState (second (+ (num1 * num2)))

parseNum :: ParsecT [Char] u Identity Integer
parseNum = do
  num <- many digit
  guard (not (null num) && length num <= 3)
  pure $ read num

parseDo :: ParsecT [Char] State Identity ()
parseDo = do
  _ <- string "do()"
  modifyState (first (const True))

parseDont :: ParsecT [Char] State Identity ()
parseDont = do
  _ <- string "don't()"
  modifyState (first (const False))

--
type State = (Bool, Integer)