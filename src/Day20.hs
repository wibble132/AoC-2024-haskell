module Day20 (part1, part2, readInput) where

import Data.Array (Array)
import Data.Array.IArray qualified as A
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (Position, toArray)

part1 :: Input -> String
part1 = show . length . filter (>= 100) . findSingleSkips

part2 :: Input -> String
part2 = show . length . filter (>= 100) . findSkipsUpTo 20

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

-- Length of shortest path, allowing to skip a single wall
findSingleSkips :: Input -> [Int]
findSingleSkips (g, s, e)
  | forwardsTime /= backwardsTime = error "Forward and backward paths give different times"
  | otherwise = map costSave $ A.indices g
  where
    forwards = M.map fst $ dijkstraAll neighbours (const . const (1 :: Int)) s
    backwards = M.map fst $ dijkstraAll neighbours (const . const (1 :: Int)) e

    forwardsTime = fromJust $ forwards M.!? e
    backwardsTime = fromJust $ backwards M.!? s

    neighbours :: Position -> [Position]
    neighbours (x, y) = filter ((maybe False (/= Wall)) . (g A.!?)) $ [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

    costSave :: Position -> Int
    costSave p@(x, y)
      -- Removing anything but a wall is silly
      | g A.!? p /= Just Wall = 0
      | null forwardOptions || null backwardOptions = 0
      | otherwise = forwardsTime - bestForward - bestBackward - 2
      where
        forwardOptions = mapMaybe (forwards M.!?) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        backwardOptions = mapMaybe (backwards M.!?) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        bestForward = minimum forwardOptions
        bestBackward = minimum backwardOptions

------ PART 2 ------

-- Length of shortest path, allowing to skip a single wall
findSkipsUpTo :: Int -> Input -> [Int]
findSkipsUpTo cheatLim (g, s, e)
  | forwardsTime /= backwardsTime = error "Forward and backward paths give different times"
  | otherwise = concatMap findCheatsFrom (A.indices g)
  where
    forwards = M.map fst $ dijkstraAll neighbours (const . const (1 :: Int)) s
    backwards = M.map fst $ dijkstraAll neighbours (const . const (1 :: Int)) e

    forwardsTime = fromJust $ forwards M.!? e
    backwardsTime = fromJust $ backwards M.!? s

    neighbours :: Position -> [Position]
    neighbours (x, y) = filter ((maybe False (/= Wall)) . (g A.!?)) $ [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

    -- Start from a position, find cheats to other positions
    findCheatsFrom :: Position -> [Int]
    findCheatsFrom p
      -- Should start at a valid space
      | isNothing forwardScore = []
      -- Should have a valid space to cheat to
      --   | null backwardScores = []
      | otherwise = map (\(score, dist) -> forwardsTime - fromJust forwardScore - score - dist) backwardScores
      where
        forwardScore = forwards M.!? p
        backwardScores =
          map (\pb -> (backwards M.! pb, distance pb p))
            . filter (isJust . (backwards M.!?))
            . map (\(xb, yb) -> bimap (+ xb) (+ yb) p)
            . S.toList
            $ cheatableNeighbourOffsets

    cheatableNeighbourOffsets :: Set Position
    cheatableNeighbourOffsets = foldl' (\acc _ -> acc `S.union` S.fromList (concatMap adjacent acc)) (S.singleton (0, 0)) [1 .. cheatLim]

adjacent :: Position -> [Position]
adjacent (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

------ COPIED FROM DAY 17 (& slightly modified) -------

dijkstraAll ::
  (Foldable f, Num cost, Ord cost, Ord state, Show state) =>
  -- \| Function to generate list of neighboring states given the current state
  (state -> f state) ->
  -- \| Function to generate transition costs between neighboring states. This is
  -- only called for adjacent states, so it is safe to have this function be
  -- partial for non-neighboring states.
  (state -> state -> cost) ->
  -- \| Initial state
  state ->
  -- \| (Total cost, list of steps) for the shortest paths found which
  -- satisfies the given predicate
  (Map state (cost, Set state))
dijkstraAll next cost initial = inner (M.singleton 0 (S.singleton initial)) (M.singleton initial (0, S.empty))
  where
    -- inner :: (Num c, Ord c, Ord s) => Map c [s] -> Map s (c, [[s]]) -> (c, [[s]])
    inner toSearch bestPaths
      | foundEnd = (bestPaths)
      | otherwise = uncurry inner xx
      where
        foundEnd = M.null toSearch

        ((_, xs), m') = M.deleteFindMin toSearch
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
