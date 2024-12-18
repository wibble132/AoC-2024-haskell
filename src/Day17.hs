module Day17 (part1, part2, readInput) where

import Control.Applicative (many, (<|>))
import Control.Parallel.Strategies (parList, rdeepseq, withStrategy)
import Data.Array (Array)
import Data.Array.IArray qualified as A
import Data.Bits (xor)
import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Text.Parsec (ParsecT, char, digit, newline, parse, sepBy, string)

part1 :: Input -> String
part1 = readOutput . uncurry (flip runProgram)

part2 :: Input -> String
part2 = show . uncurry (flip bruteUnmunch)

readInput :: String -> Input
readInput s = case parse input "input" s of
  Left e -> error $ show e
  Right x -> x

------ PARSING ------

type Input = (State, Program)

type Program = Array Int Int

type ProgramOutput = [Int]

data State = State {reg :: Registers, pointer :: Int, out :: ProgramOutput, halted :: Bool} deriving (Show)

data Registers = Registers {a :: Integer, b :: Integer, c :: Integer} deriving (Show)

data Opcode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Show, Enum)

type Operand = Int

input :: ParsecT String u Identity Input
input = do
  a <- register 'A'
  b <- newline >> register 'B'
  c <- newline >> register 'C'

  p <- newline >> newline >> program

  let r = Registers {a, b, c}
  let s = State r 0 [] False
  pure $ (s, A.listArray (0, length p - 1) p)

register :: Char -> ParsecT String u Identity Integer
register c = string ("Register " ++ [c] ++ ": ") >> number

program :: ParsecT String u Identity [Int]
program = string ("Program: ") >> number `sepBy` char ','

number :: (Read i, Integral i) => ParsecT String u Identity i
number = read <$> (many (char '-' <|> digit))

------ PART 1 ------

literalOperand :: Operand -> Integer
literalOperand op
  | op `elem` [0 .. 7] = toInteger op
  | otherwise = error "Invalid operand"

comboOperand :: Registers -> Operand -> Integer
comboOperand r op
  | op `elem` [0 .. 3] = toInteger op
  | op == 4 = a r
  | op == 5 = b r
  | op == 6 = c r
  | op == 7 = error "Operand 7 should not appear in valid programs"
  | otherwise = error "Invalid operand"

runOpcode :: (State) -> Opcode -> Operand -> State
-- runOpcode _ o1 o2 | trace (show (o1, o2)) False = undefined
runOpcode s@State {reg = r} Adv op = s {reg = r {a = (a r `div` (2 ^ comboOperand r op))}}
runOpcode s@State {reg = r} Bxl op = s {reg = r {b = (b r `xor` literalOperand op)}}
runOpcode s@State {reg = r} Bst op = s {reg = r {b = (comboOperand r op) `mod` 8}}
runOpcode s@State {reg = r, pointer = p} Jnz op
  | a r == 0 = s {pointer = (p + 2)}
  | otherwise = s {pointer = fromInteger (literalOperand op)}
runOpcode s@State {reg = r} Bxc _ = s {reg = (r {b = (b r `xor` c r)})}
runOpcode s@State {reg = r} Out op = s {out = (fromInteger (comboOperand r op `mod` 8)) : out s}
runOpcode s@State {reg = r} Bdv op = s {reg = r {b = (a r `div` (2 ^ comboOperand r op))}}
runOpcode s@State {reg = r} Cdv op = s {reg = r {c = (a r `div` (2 ^ comboOperand r op))}}

nextOps :: Program -> State -> Either State (State, Opcode, Operand)
nextOps prog s@State {pointer = p}
  | isFinished = Left (s {halted = True})
  | otherwise = Right (s {pointer = pointer s + 2}, toEnum opcode, fromJust operand)
  where
    isFinished = not $ isJust operand
    opcode = prog A.! p
    operand = prog A.!? (p + 1)

step :: Program -> State -> Either ProgramOutput State
step prog s = case nextOps prog s of
  (Left finalState) -> Left (reverse $ out finalState)
  (Right (s', opc, ope)) -> Right (runOpcode s' opc ope)

readOutput :: ProgramOutput -> String
readOutput = init . tail . filter (/= ' ') . show

whileRight :: (a -> Either b a) -> a -> b
whileRight f a = case f a of
  Right x -> whileRight f x
  Left x -> x

runProgram :: Program -> State -> ProgramOutput
runProgram prog s = whileRight (step prog) s

------ PART 2 ------

-- Try a brute force? Not gonna happen with my input hehe (gonna be 45-48 bits long oops! Does help for the final character)

bruteForceSolve :: Program -> State -> [Integer] -> Integer
bruteForceSolve prog s values = fst . fromJust . find (snd) . withStrategy (parList rdeepseq) . map isValid $ values
  where
    isValid :: Integer -> (Integer, Bool)
    isValid a = (a, (== A.elems prog) . runProgram prog $ s {reg = (reg s) {a = a}})

-- The trick to notice is my program munches 3 bits of A at once, doing some stuff with B and C, then outputting B

bruteUnmunch :: Program -> State -> Integer
bruteUnmunch prog s = bruteForceSolve prog s [aFinal - 10_000 `max` 0 .. aFinal + 10_000]
  where
    -- Something is weird with the last character, so I just brute force that normally
    aFinal = a $ reg sFinal
    sFinal = foldl' buStep (mapA (const 0) s) (init . reverse $ A.elems $ prog)

    buStep :: State -> Int -> State
    buStep s' n = s''
      where
        s'' = mapA (\a' -> 8 * (a' + unmunch)) s'
        unmunch = bruteUnmunchSigle n prog s'

-- Brute force only a single output at a time
bruteUnmunchSigle ::
  -- Target
  Int ->
  -- Program to run
  Program ->
  -- Initial State
  State ->
  --
  Integer
bruteUnmunchSigle t prog s =
  fromJust $
    find
      ( \a' ->
          (== t)
            . head
            . runProgram prog
            $ (mapA (+ toInteger a') s)
      )
      [0 .. 128]

mapA :: (Integer -> Integer) -> State -> State
mapA f s = s {reg = (reg s) {a = (f $ a (reg s))}}

-- 000 011 100 101 011 000 000