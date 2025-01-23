-- Day 10: Hoof It
module Day10 (part1) where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Position = (Int, Int)
type TMap = Map.Map Position Int
type Visited = Set.Set Position
type Tops = Set.Set Position

-- Returns sum of trailhead scores.
part1 :: String -> Int
part1 input =
  let m = parse input
      scores = map (trailheadScore m) (getTrailheads m)
  in sum scores 
 where getTrailheads = Map.keys . Map.filter (== 0)

trailheadScore :: TMap -> Position -> Int
trailheadScore m trailhead = traverseTrail [trailhead] Set.empty Set.empty
  where
    traverseTrail :: [Position] -> Visited -> Tops -> Int
    traverseTrail [] _ acc = Set.size acc
    traverseTrail (pos:next) visited acc
      | Set.member pos visited = traverseTrail next visited acc
      | isTrailTop pos = traverseTrail next visited (Set.insert pos acc)
      | otherwise =
        let visited' = Set.insert pos visited
            next' = getNeighbours pos <> next
         in traverseTrail next' visited' acc

    isTrailTop :: Position -> Bool
    isTrailTop pos =
      case getHeight pos of
        Just 9 -> True
        _      -> False

    getNeighbours :: Position -> [Position]
    getNeighbours pos@(x, y) =
      let Just currentHeight = getHeight pos
          nextHeight = currentHeight + 1
          next = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
       in filter (\p -> getHeight p == Just nextHeight) next

    getHeight :: Position -> Maybe Int
    getHeight pos = Map.lookup pos m

parse :: String -> TMap
parse input =
  let list = [ ((x, y), charToInt c)
             | (row, y) <- zip (lines input) [1..],
               (c, x) <- zip row [1..], c /= '.'
             ]
  in Map.fromList list
  where
    -- FIXME: move to Helpers module.
    charToInt :: Char -> Int
    charToInt c =
      case c of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
