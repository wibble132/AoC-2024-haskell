module Utils
  ( windows,
    both,
    each,
    (.:),
    iterateN,
    withPositions,
    Position,
    toArray,
  )
where

import Control.Monad (join)
import Data.Array (Array)
import Data.Array qualified as A
import Data.Bifunctor (bimap, first)
import Data.List (tails)

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