-- Day 23: LAN Party
module Day23 (part1, part2) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Ord (comparing)
import Helpers (splitOn)

type AdjacencyMap = M.Map Vert [Vert]
type Vert = String

-- Returns number of inter-connected computer sets that has at least one t-computer.
part1 :: String -> Int
part1 input =
  let m = parse input
      ts = filter (\x -> head x == 't') $ M.keys m
      cs = L.nub $ map L.sort . concatMap (connections3 m) $ ts
   in length cs
  where
    connections3 :: AdjacencyMap -> Vert -> [[Vert]]
    connections3 m t =
      let ts = M.findWithDefault [] t m
          cs = filter (\[a, b] -> connected m a b) $ pairs ts
       in map ((t:)) cs

    pairs :: [a] -> [[a]]
    pairs = filter (\x -> length x == 2) . L.subsequences

part2 :: String -> String
part2 input =
  let clique = L.maximumBy (comparing length) . solve . parse $ input
   in L.intercalate "," $ L.sort clique

-- Finding maximal cliques with Bron-Kerbosch Algorithm
solve :: AdjacencyMap -> [[Vert]]
solve m = run [] (M.keys m) []
  where
    run acc [] [] = [acc]
    run acc ps xs = loop ps xs
      where
        loop [] _ = []
        loop (v:ps) xs =
          let res = run (v:acc) (ps `remove` v) (xs `remove` v)
           in res ++ loop ps (v:xs)

        remove xs v = filter (connected m v) xs

adjacents :: AdjacencyMap -> Vert -> [Vert]
adjacents m x = M.findWithDefault [] x m

connected :: AdjacencyMap -> Vert -> Vert -> Bool
connected m a b = b `elem` (adjacents m a)

parse :: String -> AdjacencyMap
parse input = foldl run M.empty . map (splitOn '-') $ lines input
  where run m [k, v] =
          let m1 = M.insertWith (<>) k [v] m
              m2 = M.insertWith (<>) v [k] m1
           in m2
