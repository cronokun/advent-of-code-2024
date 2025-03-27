-- Day 20: Race Condition
module Day20 (part1) where

import qualified Data.List as L
import qualified Data.Set as S

type Coord = (Int, Int)
type Path = [(Coord, Int)]

data Racetrack = Racetrack
  { rTiles :: S.Set Coord
  , rStart :: Coord
  , rFinish :: Coord
  } deriving (Show)

-- Returns how many cheats would save you at least N picoseconds.
part1 :: String -> Int -> Int
part1 input n =
  let track = parse input
      path = traverseTrack track
      cheats = allCheats path (>= n)
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

allCheats :: Path -> (Int -> Bool) -> [Int]
allCheats path p = foldl (\acc x -> acc <> saves x) [] $ map fst path
  where
    saves c@(x, y) =
      let xs = [ ((x + 1, y), (x + 2, y))
               , ((x - 1, y), (x - 2, y))
               , ((x, y + 1), (x, y + 2))
               , ((x, y - 1), (x, y - 2))
               ]
          cs = map snd . filter (uncurry isCheat) $ xs
       in filter p $ map (savedDist c) cs

    isCheat c1 c2 = isWall c1 && isPath c2
    isPath = flip S.member pathTiles
    isWall = not . isPath
    pathTiles = S.fromList $ map fst path

    savedDist c c' =
      let Just d1 = L.lookup c path
          Just d2 = L.lookup c' path
       in d2 - d1 - 2

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
