-- Day 8: Resonant Collinearity
module Day8 (part1) where

import qualified Data.List as List
import qualified Data.Set as Set

type Coord = (Integer, Integer)

-- Part 1: How many unique locations within the bounds of the map contain an antinode?
part1 :: String -> Integer
part1 input =
  let (m, size) = parse input
   in toInteger $ calcAntinodes size m

-- Recursively calculate all antinodes.
calcAntinodes :: Coord -> [(Char, Coord)] -> Int
calcAntinodes (mx, my) m = helper Set.empty m
  where
    helper acc [] = Set.size acc
    helper acc xs =
      let (fs, rest) = splitByKey xs
          ps = pairs $ map snd fs
          anti = concatMap antinodes ps
          acc' = Set.union (Set.fromList anti) acc
       in helper acc' rest

    isSameKey (k1, _) (k2, _) = k1 == k2

    antinodes [(x1,y1), (x2,y2)] =
      let dx = x2 - x1
          dy = y2 - y1
          a1 = (x1 - dx, y1 - dy)
          a2 = (x2 + dx, y2 + dy)
       in filter isInside [a1, a2]

    isInside (x, y) = x > 0 && x <= mx && y > 0 && y <= my

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
