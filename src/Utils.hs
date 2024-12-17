-- Used for signatures in concatMapM
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
  ( windows,
    both,
    each,
    (.:),
    iterateN,
    withPositions,
    Position,
    toArray,
    concatMapM,
    Direction (..),
    cw,
    acw,
    getNeighbours,
  )
where

import Control.Monad (join)
import Data.Array (Array)
import Data.Array qualified as A
import Data.Bifunctor (bimap, first)
import Data.Foldable (Foldable (foldr'))
import Data.List (tails)
import Prelude hiding (Left, Right)

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

-- Apply a function to both elements of a pair
both :: (a -> b) -> (a, a) -> (b, b)
both = join bimap

-- Applies a function to combine two pairs component-wise
each :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
each f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

-- Like (.), but combining a 2-input function and a 1-input function to produce a 2-input function
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)

infixr 8 .:

-- Apply a function n times
iterateN :: Int -> (a -> a) -> (a -> a)
iterateN n = ((!! n) .) . iterate

withPositions :: [[a]] -> [((Int, Int), a)]
withPositions = concat . zipWith (\i_y ys -> zipWith (\i_x x -> ((i_x, i_y), x)) [0 ..] ys) [0 ..]

type Position = (Int, Int)

toArray :: [[a]] -> Array Position a
toArray = uncurry A.array . first (((0, 0),) . fst . last) . join (,) . withPositions

concatMapM :: forall m a b. (Monad m) => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr' f (pure [])
  where
    f :: a -> m [b] -> m [b]
    f x xs = do
      y :: [b] <- op x
      if null y
        then xs
        else do
          ys :: [b] <- xs
          pure $ y ++ ys

data Direction = Up | Right | Down | Left deriving (Show, Eq, Ord)

-- Clockwise rotation
cw :: Direction -> Direction
cw Up = Right
cw Right = Down
cw Down = Left
cw Left = Up

-- Anticlockwise rotation
acw :: Direction -> Direction
acw Right = Up
acw Down = Right
acw Left = Down
acw Up = Left

getNeighbours :: Position -> [Position]
getNeighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1)]