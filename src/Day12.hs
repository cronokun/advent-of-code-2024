-- Day 12: Garden Groups
module Day12 (part1) where

import Data.List ((\\))
import Data.Tuple (swap)
import Data.Map (Map, findWithDefault, fromList)

type Coord = (Int, Int)
type PlotMap = Map Coord Char
type Plot = (Char, Coord)

--  Returns total price of fencing all regions on the map.
part1 :: String -> Int
part1 input =
  let plots = parse input
      gmap = fromList $ map swap plots
      groups = splitIntoGroups gmap plots
   in sum . map (areaCost gmap) $ groups

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
      let p = findWithDefault ' ' c gmap
       in (p, c)

parse :: String -> [Plot]
parse input = [(plt, (x,y)) | (row, y) <- zip (lines input) [1..], (plt, x) <- zip row [1..]]
