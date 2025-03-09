-- Day 16, part 1: Reindeer Maze
module Day16Part1 (part1) where

import qualified Data.Set as Set
import qualified Data.Heap as Heap

data Grid = Grid
  { gridMap :: GridMap
  , start :: Coord
  , finish :: Coord
  } deriving (Show)

data Move = MDown | MUp | MLeft | MRight deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type GridMap = Set.Set Coord
type Position = (Int, Coord, Move)
type Queue = Heap.Heap Position

-- Returns the lowers posible score.
part1 :: String -> Int
part1 input =
  let grid = parse input
   in traverseGrid 0 (0, start grid, MRight) Heap.empty Set.empty grid

traverseGrid :: Int -> Position -> Queue -> Set.Set Coord -> Grid -> Int
traverseGrid n (score, pos, dir) queue visited grid =
  case pos == finish grid of
    True -> score
    False ->
      let next = getAdjacent pos
          visited' = Set.insert pos visited
          queue' = foldr Heap.insert queue next
          nextPos = Heap.minimum queue'
          nextQueue = Heap.deleteMin queue'
       in traverseGrid (n+1) nextPos nextQueue visited' grid
  where
    getAdjacent :: Coord -> [Position]
    getAdjacent (x,y) =
      let ns = [ ((x + 1, y), MRight)
               , ((x, y - 1), MUp)
               , ((x - 1, y), MLeft)
               , ((x, y + 1), MDown)
               ]
       in addScore . filterNotVisited . filterInGrid $ ns
      where
        addScore = map (\(p, d) -> (incScore d, p, d))
        filterInGrid = filter (\(p, _) -> Set.member p $ gridMap grid)
        filterNotVisited = filter (\(p, _) -> Set.notMember p visited)

    incScore :: Move -> Int
    incScore newDir
      | dir == newDir = score + 1
      | isTurnaround dir newDir = score + 2001
      | otherwise = score + 1001
      where
        isTurnaround MUp MDown = True
        isTurnaround MDown MUp = True
        isTurnaround MLeft MRight = True
        isTurnaround MRight MLeft = True
        isTurnaround _ _ = False

parse :: String -> Grid
parse input = runParser (0,0) (0,0) (0,0) [] input
  where
    runParser (_,y) start finish acc ('\n':rest) = runParser (0,y+1) start finish acc rest
    runParser (x,y) start finish acc ('#':rest) = runParser (x+1,y) start finish acc rest
    runParser (x,y) start finish acc ('.':rest) = runParser (x+1,y) start finish ((x,y):acc) rest
    runParser (x,y) _     finish acc ('S':rest) = runParser (x+1,y) (x,y) finish ((x,y):acc) rest
    runParser (x,y) start _      acc ('E':rest) = runParser (x+1,y) start (x,y)  ((x,y):acc) rest
    runParser _ start finish acc "" =
      Grid { start = start
           , finish = finish
           , gridMap = Set.fromList acc }
