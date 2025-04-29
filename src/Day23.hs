-- Day 23: LAN Party
module Day23 (part1, part2) where

import qualified Data.List as L
import qualified Data.Map as M
import Helpers (splitOn)

type AdjacencyMap = M.Map String [String]

-- Returns number of inter-connected computer sets that has at least one t-computer.
part1 :: String -> Int
part1 input =
  let m = parse input
      ts = filter (\x -> head x == 't') $ M.keys m
      cs = L.nub $ map L.sort . concatMap (connections m) $ ts
   in length cs

connections :: AdjacencyMap -> String -> [[String]]
connections m t =
  let ts = M.findWithDefault [] t m
      cs = filter isConnected . filter isTuple . L.subsequences $ ts
   in map ((t:)) cs
  where isConnected [a, b] = b `elem` M.findWithDefault [] a m
        isTuple xs = length xs == 2


-- TBD
part2 :: String -> Int
part2 input = const 0 $ parse input

parse :: String -> AdjacencyMap
parse input = foldl run M.empty . map (splitOn '-') $ lines input
  where run m [k, v] =
          let m1 = M.insertWith (<>) k [v] m
              m2 = M.insertWith (<>) v [k] m1
           in m2
