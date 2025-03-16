-- Day 16, part 2: Reindeer Maze
module Day16Part2 (part2) where

import Data.Heap (Heap, Entry)
import qualified Data.Heap as Heap
import Data.Set (Set)
import qualified Data.Set as Set

data Grid = Grid
  { gridMap :: GridMap
  , gridStart :: Coord
  , gridFinish :: Coord
  } deriving (Show)

data Move = MDown | MUp | MLeft | MRight deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type GridMap = Set Coord
type Path = Set Coord
type Position = Entry Score (Coord, Move, Set Coord)
type Queue = Heap Position
type Score = Int

-- Returns many tiles are part of at least one of the best paths through the maze.
part2 :: String -> Int
part2 input =
  let grid = parse input
      initPos = Heap.Entry 0 (gridStart grid, MRight, Set.empty)
      allPathes = traverseGrid initPos Heap.empty [] Set.empty 999999 grid
      tiles = foldr Set.union Set.empty allPathes
   in Set.size tiles

traverseGrid :: Position          -- current position
             -> Queue             -- queue
             -> [Path]            -- all min pathes with score
             -> Set (Coord, Move) -- visited
             -> Score             -- min score
             -> Grid              -- grid
             -> [Path]
traverseGrid (Heap.Entry score (pos, dir, path)) queue acc visited minScore grid =
  if pos == gridFinish grid 
  then
    let path' = Set.insert pos path
        acc' = (path' : acc)
        score' = min score minScore
     in case Heap.viewMin queue of
          Just (pos', queue') ->
            traverseGrid pos' queue' acc' visited score' grid
          Nothing -> acc'
  else
    let next = getAdjacent pos
        nextQueue = foldr Heap.insert queue next
        visited' = Set.insert (pos, dir) visited
     in case Heap.viewMin nextQueue of
          Just (pos', queue') ->
            traverseGrid pos' queue' acc visited' minScore grid
          Nothing -> acc
  where
    getAdjacent :: Coord -> [Position]
    getAdjacent (x,y) =
      let ns = [ ((x + 1, y), MRight)
               , ((x, y - 1), MUp)
               , ((x - 1, y), MLeft)
               , ((x, y + 1), MDown)
               ]
       in filter byScore . map toPos . filter notVisited . filter inGrid $ ns
      where
        inGrid = \(p,_) ->  p `Set.member` gridMap grid
        notVisited = \n -> n `Set.notMember` visited
        toPos = \(p,d) -> Heap.Entry (incScore d) (p, d, (Set.insert pos path))
        byScore = \(Heap.Entry score _payload) -> score <= minScore

    incScore :: Move -> Score
    incScore newDir
      | dir == newDir = score + 1
      | otherwise = score + 1001

parse :: String -> Grid
parse input = runParser (0,0) (0,0) (0,0) [] input
  where
    runParser (_,y) start finish acc ('\n':rest) = runParser (0,y+1) start finish acc rest
    runParser (x,y) start finish acc ('#':rest) = runParser (x+1,y) start finish acc rest
    runParser (x,y) start finish acc ('.':rest) = runParser (x+1,y) start finish ((x,y):acc) rest
    runParser (x,y) _     finish acc ('S':rest) = runParser (x+1,y) (x,y) finish ((x,y):acc) rest
    runParser (x,y) start _      acc ('E':rest) = runParser (x+1,y) start (x,y)  ((x,y):acc) rest
    runParser _ start finish acc "" =
      Grid { gridStart = start
           , gridFinish = finish
           , gridMap = Set.fromList acc }
