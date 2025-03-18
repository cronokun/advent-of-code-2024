-- Day 18: RAM Run
module Day18 (part1, part2) where

import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers (splitOn)

type Tiles = Set Coord
type Coord = (Int, Int)
type Queue = Heap QueueEntry
type QueueEntry = Heap.Entry Int (Coord, [Coord])

-- Returns minimum number of steps needed to reach the exit.
part1 :: String -> Int -> Int
part1 input n =
  let tiles = parse input
      goal = getGoal tiles
      blocks = Set.fromList . take n $ tiles
   in length . fromJust $ traverseGrid blocks goal

-- Returns the coordinates of block that makes the exit unreachable.
part2 :: String -> Coord
part2 input =
  let tiles = parse input
      goal = getGoal tiles
  in traverseUntilCant (parse input) Set.empty [] goal
 where
   traverseUntilCant :: [Coord] -> Set Coord -> [Coord] -> Coord -> Coord
   traverseUntilCant (x : xs) tiles path goal
     | null path || x `elem` path =
       let tiles' = Set.insert x tiles
        in case traverseGrid tiles' goal of
             Just path' -> traverseUntilCant xs tiles' path' goal
             Nothing -> x
     | otherwise = traverseUntilCant xs (Set.insert x tiles) path goal

traverseGrid :: Tiles -> Coord -> Maybe [Coord]
traverseGrid tiles goal@(mx, my) =
  let start = (Heap.Entry 0 ((0,0), []))
   in traverse' start Heap.empty Set.empty
  where
    traverse' :: QueueEntry -> Queue -> Set Coord -> Maybe [Coord]
    traverse' (Heap.Entry cost (coord, path)) queue visited
      | coord == goal = Just path
      | otherwise =
        let visited' = Set.insert coord visited
            newQueue = foldr (Heap.insert) queue $ neighbours (coord, path) visited
         in case Heap.viewMin newQueue of
            Just (next, queue') -> traverse' next queue' visited'
            Nothing -> Nothing

    neighbours :: (Coord, [Coord]) -> Set Coord -> [QueueEntry]
    neighbours ((x, y), path) visited =
      let ns = [ (x + 1, y)
               , (x - 1, y)
               , (x, y + 1)
               , (x, y - 1) ]
       in map addCost . filter notVisited . filter notCorrupted . filter inBounds $ ns
      where
        addCost c = Heap.Entry (length path + 1 + dist c) (c, (c : path))
        inBounds (x', y') = x' >= 0 && x' <= mx && y' >= 0 && y' <= my
        notCorrupted coord = coord `Set.notMember` tiles
        notVisited coord = coord `Set.notMember` visited

    dist :: Coord -> Int
    dist (x,y) = mx - x + my - y

parse :: String -> [Coord]
parse input = map toCoord $ lines input
  where
    toCoord l = let [a,b] = splitOn ',' l in (read a :: Int, read b :: Int)

getGoal :: [Coord] -> Coord
getGoal = foldl (\(x, y) (x', y') -> (max x x', max y y')) (0, 0)
