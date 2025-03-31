-- Day 20: Race Condition
module Day20 (part1, part2) where

import qualified Data.List as L
import qualified Data.Set as S

type Cheat = (Coord, Coord, Int, Int)
type Coord = (Int, Int)
type Path = [(Coord, Int)]

data Racetrack = Racetrack
  { rTiles :: S.Set Coord
  , rStart :: Coord
  , rFinish :: Coord
  } deriving (Show)

-- Returns how many 2-picosec cheats would save you at least N picoseconds.
part1 :: String -> Int -> Int
part1 input n = helper input 2 n

-- Returns how many 20-picosec cheats would save you at least N picoseconds.
part2 :: String -> Int -> Int
part2 input n = helper input 20 n

helper :: String -> Int -> Int -> Int
helper input clen n =
  let track = parse input
      path = traverseTrack track
      cheats = findCheats path clen n
   in length cheats

traverseTrack :: Racetrack -> Path
traverseTrack track = helper (rStart track) (rStart track) 0 []
  where
    helper :: Coord -> Coord -> Int -> Path -> Path
    helper tile@(x,y) prev dist path
      | tile == rFinish track = reverse ((tile, dist) : path)
      | otherwise =
        let next = nextTile tile prev
            path' = (tile, dist) : path
         in helper next tile (dist + 1) path'

    nextTile (x, y) prev =
      let xs = [(x + 1, y), (x - 1, y), (x, y + 1), (x, (y - 1))]
       in head . filter (/= prev) . filter inGrid $ xs

    inGrid tile = S.member tile $ rTiles track

findCheats :: Path -> Int -> Int -> [Cheat]
findCheats path len n =
  [ (a, b, dist, saveDist)
    | ((a, d1), i) <- zip path [1..]
  , (b, d2) <- drop i path
  , let dist = cheatDist a b
  , let saveDist = d2 - d1 - dist
  , dist >= 2, dist <= len, saveDist > 0, saveDist >= n
  ]
  where
    cheatDist (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

parse :: String -> Racetrack
parse input =
  let (s, e, xs) = helper (0,0) (0,0) (0,0) [] input
   in Racetrack { rStart = s, rFinish = e, rTiles = S.fromList xs }
  where
    helper s e _ acc [] = (s, e, acc)
    helper s e (x,y) acc ('S' : xs) = helper (x,y) e (x + 1, y) ((x,y) : acc) xs
    helper s e (x,y) acc ('E' : xs) = helper s (x,y) (x + 1, y) ((x,y) : acc) xs
    helper s e (x,y) acc ('.' : xs) = helper s e (x + 1, y) ((x,y) : acc) xs
    helper s e (x,y) acc ('#' : xs) = helper s e (x + 1, y) acc xs
    helper s e (x,y) acc ('\n' : xs) = helper s e (0, y + 1) acc xs
