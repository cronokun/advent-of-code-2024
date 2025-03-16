-- Day 18: RAM Run
module Day18 (part1) where

import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers (splitOn)

type Tiles = Set Coord
type Coord = (Int, Int)
type Queue = Heap QueueEntry
type QueueEntry = Heap.Entry Int (Coord, [Coord])

-- Returns minimum number of steps needed to reach the exit.
part1 :: String -> Int -> Coord -> Int
part1 input n goal =
  let tiles = Set.fromList . take n . parse $ input
   in traverseGrid tiles goal

traverseGrid :: Tiles -> Coord -> Int
traverseGrid tiles goal@(mx, my) = traverse' (Heap.Entry 0 ((0,0), [])) Heap.empty Set.empty
  where
    traverse' :: QueueEntry -> Queue -> Set Coord -> Int
    traverse' (Heap.Entry cost current@(coord, _path)) queue visited
      | coord == goal = cost
      | otherwise =
        let visited' = Set.insert coord visited
            newQueue = foldr (Heap.insert) queue $ neighbours current visited
            Just (next, queue') = Heap.viewMin newQueue
          in traverse' next queue' visited'

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

--- DEBUG:

printGrid (mx,my) tiles = helper (0, 0) ""
  where
    helper c@(x, y) acc
      | x == mx && y == my = reverse (charFor c : acc)
      | x == mx = helper (0, y + 1) ('\n' : charFor c : acc)
      | otherwise = helper (x + 1, y) (charFor c : acc)
    charFor c = if c `Set.member` tiles then '#' else '.'
