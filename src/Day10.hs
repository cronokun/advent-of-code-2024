-- Day 10: Hoof It
module Day10 (part1, part2) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Helpers (charToInt)

type Position = (Int, Int)
type TMap = Map.Map Position Int
type Visited = Set.Set Position
type Tops = Set.Set Position

-- Returns sum of trailhead scores.
part1 :: String -> Int
part1 input = sumWith trailheadScore $ parse input

-- Return sum of trailhead ratings.
part2 :: String -> Int
part2 input = sumWith trailheadRating $ parse input

sumWith :: (TMap -> Position -> Int) -> TMap -> Int
sumWith f m =
  let trailheads = Map.keys . Map.filter (== 0) $ m
  in sum $ map (f m) trailheads

trailheadScore :: TMap -> Position -> Int
trailheadScore m trailhead = traverseTrail [trailhead] Set.empty Set.empty
  where
    traverseTrail :: [Position] -> Visited -> Tops -> Int
    traverseTrail [] _ acc = Set.size acc
    traverseTrail (pos:rest) visited acc
      | Set.member pos visited = traverseTrail rest visited acc
      | isTrailTop pos m = traverseTrail rest visited (Set.insert pos acc)
      | otherwise =
        let visited' = Set.insert pos visited
            next = getNeighbours pos m
         in traverseTrail (next <> rest) visited' acc

trailheadRating :: TMap -> Position -> Int
trailheadRating m trailhead = traverseTrail [trailhead] 0
  where
    traverseTrail [] acc = acc
    traverseTrail (pos:rest) acc
      | isTrailTop pos m = traverseTrail rest (acc + 1)
      | otherwise =
        let next  = getNeighbours pos m
         in traverseTrail (next <> rest) acc

getNeighbours :: Position -> TMap -> [Position]
getNeighbours pos@(x, y) m =
  let nextHeight = fmap succ (Map.lookup pos m)
      nextPositions = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
   in filter (isGoodHike nextHeight) nextPositions
  where
    isGoodHike h p = Map.lookup p m == h

isTrailTop :: Position -> TMap -> Bool
isTrailTop pos m = case Map.lookup pos m of
                     Just 9 -> True
                     _      -> False

parse :: String -> TMap
parse input = Map.fromList [ ((x, y), charToInt c)
                           | (row, y) <- zip (lines input) [1..],
                             (c, x) <- zip row [1..], c /= '.'
                           ]
