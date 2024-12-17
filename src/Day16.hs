{-# LANGUAGE ScopedTypeVariables #-}

module Day16 (part1, part2, readInput) where

import Algorithm.Search (dijkstra)
import Control.Arrow (Arrow (second))
import Data.Array (Array)
import Data.Array.IArray qualified as A
import Data.List (find, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import GHC.Stack (HasCallStack)
import Utils (Direction, Position, acw, cw, toArray)
import Utils qualified as D

part1 :: Input -> String
part1 = show . search

part2 :: Input -> String
part2 = show . length . nub . concat . search2

readInput :: String -> Input
readInput = findStartEnd . A.amap charToTile . toArray . lines

------ PARSING ------

type Input = (Array Position Tile, Position, Position)

data Tile = Wall | Space | Start | End deriving (Show, Eq)

charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile '.' = Space
charToTile 'S' = Start
charToTile 'E' = End
charToTile c = error $ "Unknown tile " ++ [c]

findStartEnd :: Array Position Tile -> (Array Position Tile, Position, Position)
findStartEnd a = (a, findMatch Start, findMatch End)
  where
    as = A.assocs a
    findMatch x = fst . fromJust . find ((== x) . snd) $ as

------ PART 1 ------

type State = (Position, Direction)

search :: Input -> Integer
search (g, s, e) =
  fst . fromJust . dijkstra moves costs isEnd $ (s, D.Right)
  where
    moves :: State -> [State]
    moves x =
      filter ((/= Wall) . (g A.!) . fst) $
        fmap
          ($ x)
          [ left,
            right,
            forward
          ]
    costs :: (State -> State -> Integer)
    costs (a, _) (b, _)
      | a == b = 1000
      | otherwise = 1
    isEnd (p, _) = p == e

forward :: State -> State
forward ((x, y), D.Up) = ((x, y - 1), D.Up)
forward ((x, y), D.Right) = ((x + 1, y), D.Right)
forward ((x, y), D.Down) = ((x, y + 1), D.Down)
forward ((x, y), D.Left) = ((x - 1, y), D.Left)

right :: State -> State
right = second cw

left :: State -> State
left = second acw

------ PART 2 ------

search2 :: Input -> [[Position]]
search2 (g, s, e) = allPointsInPaths allPaths endPoints (s, D.Right)
  where
    (allPaths, endPoints) = dijkstraAll moves costs isEnd (s, D.Right)

    moves :: State -> [State]
    moves x =
      filter ((/= Wall) . (g A.!) . fst) $
        fmap
          ($ x)
          [ left,
            right,
            forward
          ]
    costs :: (State -> State -> Integer)
    costs (a, _) (b, _)
      | a == b = 1000
      | otherwise = 1
    isEnd (p, _) = p == e

allPointsInPaths :: Map State (Integer, Set State) -> Set State -> State -> [[Position]]
allPointsInPaths bestPaths b start = map reverse $ concatMap inner (S.toList b)
  where
    inner :: State -> [[Position]]
    inner s | s == start = [[fst s]]
    inner s@(p, _) = map (p :) . concatMap inner . S.toList . snd $ bestPaths M.! s

-- Copied the signature from search-algorithms function used for part 1, and modified for use case
-- Applies the Dijkstra algorithm, keeping track of all best routes to each node.
-- Terminates when any finish node is found.
-- Returns the map of node -> (cost to get there, set of possible states to come from)
-- Also returns the set of finish nodes that are reached in the optimal time
dijkstraAll ::
  (HasCallStack, Foldable f, Num cost, Ord cost, Ord state, Show state) =>
  -- \| Function to generate list of neighboring states given the current state
  (state -> f state) ->
  -- \| Function to generate transition costs between neighboring states. This is
  -- only called for adjacent states, so it is safe to have this function be
  -- partial for non-neighboring states.
  (state -> state -> cost) ->
  -- \| Predicate to determine if solution found. 'dijkstra' returns the shortest
  -- path to the first state for which this predicate returns 'True'.
  (state -> Bool) ->
  -- \| Initial state
  state ->
  -- \| (Total cost, list of steps) for the shortest paths found which
  -- satisfies the given predicate
  (Map state (cost, Set state), Set state)
dijkstraAll next cost found initial = inner (M.singleton 0 (S.singleton initial)) (M.singleton initial (0, S.empty))
  where
    -- inner :: (Num c, Ord c, Ord s) => Map c [s] -> Map s (c, [[s]]) -> (c, [[s]])
    inner toSearch bestPaths
      | foundEnd = (bestPaths, ends)
      | otherwise = uncurry inner xx
      where
        ((_, xs), m') = M.deleteFindMin toSearch

        foundEnd = any found xs
        ends = S.filter found xs

        xx = foldl' step (m', bestPaths) xs

    -- step :: (Num c, Ord c, Ord s) => (Map c [s], Map s (c, [[s]])) -> [s] -> (Map c [s], Map s (c, [[s]]))
    step maps sFrom = foldl' (\acc sTo -> updateMaps acc (cost sFrom sTo, sFrom, sTo)) maps (next sFrom)

updateMaps :: (Num c, Ord c, Ord s) => (Map c (Set s), Map s (c, Set s)) -> (c, s, s) -> (Map c (Set s), Map s (c, Set s))
updateMaps (toSearch, bestPaths) x@(_, _, sTo) = case updateBestPaths bestPaths x of
  Nothing -> (toSearch, bestPaths)
  Just (newCost, bestPaths') -> (M.insertWith (S.union) (newCost) (S.singleton sTo) toSearch, bestPaths')

updateBestPaths ::
  (Num c, Ord c, Ord s) =>
  -- \| The current best paths (state -> (current best cost, all known paths of that cost))
  Map s (c, Set s) ->
  -- \| The next step to take: (the cost to make the step, the state we are moving from, the step we are moving to)
  (c, s, s) ->
  -- \| The result:
  --  Nothing if the step is worse than current known steps
  --  Just updated best paths map if this step provides a better (or same cost) path to the destination state
  --    along with the (possibly new) total to reach that state
  Maybe (c, Map s (c, Set s))
updateBestPaths bp (cv, sFrom, sTo)
  -- Not visited this state before, add this state in
  | Nothing <- current = Just $ (newCost, M.insert sTo (newCost, S.singleton sFrom) bp)
  -- A better path exists, do nothing
  | curCost < newCost = Nothing
  -- This path is as good as a currently known path, add it to the list
  | curCost == newCost = Just $ (newCost, M.insertWith amendToCurrent sTo (newCost, S.singleton sFrom) bp)
  -- This is a new best path, replace existing paths and set a new cost
  | otherwise = Just $ (newCost, M.insert sTo (newCost, S.singleton sFrom) bp)
  where
    current = bp M.!? sTo
    (curCost, _) = fromJust current
    (fromCost, _) = fromJust $ bp M.!? sFrom
    newCost = fromCost + cv

    amendToCurrent (c1, p1) (c2, p2)
      | c1 /= c2 = error "bad"
      -- \| c1 > 150000 && ( length p1 > 40 || length p2 > 40) && trace (show (c1, length p1, length p2)) False = undefined
      | otherwise = (c1, S.union p1 p2)
