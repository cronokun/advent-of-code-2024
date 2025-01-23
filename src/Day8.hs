-- Day 8: Resonant Collinearity
module Day8 (part1, part2) where

import qualified Data.List as List
import qualified Data.Set as Set

type Coord = (Int, Int)

-- Part 1: How many unique locations within the bounds of the map contain an antinode?
part1 :: String -> Int
part1 input =
  let (m, size) = parse input
   in calcAntinodes antinodes size m

-- Part 2: How many unique locations within the bounds of the map contain an harmonic antinode?
part2 :: String -> Int
part2 input =
  let (m, size) = parse input
   in calcAntinodes antinodes' size m

-- Return two or less antinodes for two given antennas.
antinodes :: Coord -> [Coord] -> [Coord]
antinodes size [(x1,y1), (x2,y2)] =
  let dx = x2 - x1
      dy = y2 - y1
      a1 = (x1 - dx, y1 - dy)
      a2 = (x2 + dx, y2 + dy)
   in filter (isInside size) [a1, a2]

-- Return all harmonic antinodes for two given antennas.
antinodes' :: Coord -> [Coord] -> [Coord]
antinodes' size [(x1, y1), (x2, y2)] =
  let dx = x2 - x1
      dy = y2 - y1
      as1 = antinodesFrom (+) (dx, dy) (x1, y1)
      as2 = antinodesFrom (-) (dx, dy) (x1, y1)
   in as1 <> as2
  where
    antinodesFrom f delta point = takeWhile (isInside size) . iterate (vecApply f delta) $ point
    vecApply f (dx, dy) (x, y) = (f x dx, f y dy)

-- Check if node is inside map.
isInside :: Coord -> Coord -> Bool
isInside (mx, my) (x, y) = x > 0 && x <= mx && y > 0 && y <= my

-- Recursively calculate all antinodes.
calcAntinodes :: (Coord -> [Coord] -> [Coord]) -> Coord -> [(Char, Coord)] -> Int
calcAntinodes f size m = helper Set.empty m
  where
    helper acc [] = Set.size acc
    helper acc xs =
      let (fs, rest) = splitByKey xs
          ps = pairs $ map snd fs
          anti = concatMap (f size) ps
          acc' = Set.union (Set.fromList anti) acc
       in helper acc' rest

    isSameKey (k1, _) (k2, _) = k1 == k2
    pairs = filter ((2 == ) . length) . List.subsequences

    -- Take all elements with the same key and the rest of the list.
    splitByKey xs@((key, _):_) = span (\(k, _) -> k == key) xs

parse :: String -> ([(Char, Coord)], Coord)
parse = helper [] (1,1)
  where
    helper acc (x,y) "\n"        = (List.sort acc, (x - 1,y)) 
    helper acc (_,y) ('\n':rest) = helper acc (1, y + 1) rest
    helper acc (x,y) ('.':rest)  = helper acc (x + 1, y) rest
    helper acc (x,y) (freq:rest) = helper ((freq, (x,y)):acc) (x + 1, y) rest
