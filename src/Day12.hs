-- Day 12: Garden Groups
module Day12 (part1, part2) where

import Data.List ((\\))
import qualified Data.List as List
import Data.Tuple (swap)
import Data.Map (Map)
import qualified Data.Map as Map

data Side = SDown | SLeft | SRight | SUp deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type PlotMap = Map Coord Char
type Plot = (Char, Coord)

--  Returns total price of fencing all regions on the map.
part1 :: String -> Int
part1 input =
  let plots = parse input
      gmap = Map.fromList $ map swap plots
      groups = splitIntoGroups gmap plots
   in sum . map (areaCost gmap) $ groups

--  Returns total price of fencing all regions on the map with bulk discount.
part2 :: String -> Int
part2 input =
  let plots = parse input
      gmap = Map.fromList $ map swap plots
      groups = splitIntoGroups gmap plots
   in sum . map (areaCost' gmap) $ groups

areaCost :: PlotMap -> [Plot] -> Int
areaCost gmap grp =
  let area = length grp
      perimeter = getPerimeter 0 grp
   in area * perimeter
  where
    getPerimeter acc [] = acc
    getPerimeter acc ((p, c):ps) = 
      let ns = neigboursWith gmap (/= p) c
       in getPerimeter (acc + length ns) ps

areaCost' :: PlotMap -> [Plot] -> Int
areaCost' gmap grp =
  let area = length grp
      sides = getGroupSides grp
   in area * sides
  where
    getGroupSides :: [Plot] -> Int
    getGroupSides g =
      let perimeter = getNeigbourFences [] g
          grouped = foldr groupBySide Map.empty perimeter
          lists = map (List.nub . List.sort . snd) $ Map.toList grouped
       in sum $ map conseqSidesCount lists

    getNeigbourFences :: [(Side, Coord)] -> [Plot] -> [(Side, Coord)]
    getNeigbourFences acc [] = acc
    getNeigbourFences acc ((pln, coord@(x, y)):rest) =
      let ns = neigboursWith gmap (/= pln) coord
          ns' = map withSide ns
       in getNeigbourFences (ns' <> acc) rest
      where
        withSide (_, (x', y')) =
          let side = case (x - x', y - y') of
                       (1,0) -> SLeft
                       (-1,0) -> SRight
                       (0,1) -> SUp
                       (0,-1) -> SDown
          in (side, coord)

    conseqSidesCount :: [Int] -> Int 
    conseqSidesCount [] = 0
    conseqSidesCount xs =
      let ixs = List.findIndices notSeq $ zip xs (drop 1 xs)
       in length ixs + 1 -- plus one for last/single element
      where
        notSeq (a, b) = abs (a - b) /= 1

    groupBySide :: (Side, Coord) -> Map (Side, Int) [Int] -> Map (Side, Int) [Int]
    groupBySide (SUp,    (x, y)) m  = Map.insertWith (<>) (SUp,    y) [x] m
    groupBySide (SDown,  (x, y)) m  = Map.insertWith (<>) (SDown,  y) [x] m
    groupBySide (SLeft,  (x, y)) m  = Map.insertWith (<>) (SLeft,  x) [y] m
    groupBySide (SRight, (x, y)) m  = Map.insertWith (<>) (SRight, x) [y] m

splitIntoGroups :: PlotMap -> [Plot] -> [[Plot]]
splitIntoGroups gmap plots = doSplit [] plots
  where
    doSplit acc [] = acc
    doSplit acc xs =
      let start = take 1 xs
          grp = makeGroup [] start
          rest = xs \\ grp
       in doSplit (grp:acc) rest

    makeGroup acc [] = acc
    makeGroup acc (plot@(p, c):rest)
      | plot `elem` acc = makeGroup acc rest
      | otherwise =
          let next = neigboursWith gmap (== p) c
           in makeGroup (plot:acc) (next <> rest)

neigboursWith :: PlotMap -> (Char -> Bool) -> Coord -> [Plot]
neigboursWith gmap f (x,y) =
  let ns = map mapper [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
   in filter (f . fst) ns
  where
    mapper c =
      let p = Map.findWithDefault ' ' c gmap
       in (p, c)

parse :: String -> [Plot]
parse input = [(plt, (x,y)) | (row, y) <- zip (lines input) [1..], (plt, x) <- zip row [1..]]
