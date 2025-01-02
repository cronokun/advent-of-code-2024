-- description: Day 6: Guard Gallivant
module Day6 (part1) where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Integer, Integer)
data Dir = MDown | MLeft | MRight | MUp

part1 :: String -> Int
part1 input =
  let (limits, start, obs) = parse input
      obs' = prepareMap obs
   in traverseMap MUp start limits obs' Set.empty

move :: Dir -> Coord -> (Map.Map Integer [Integer], Map.Map Integer [Integer]) -> Maybe Coord

move MLeft (x, y) (_, rows) =
  let obs' = obstaclesInRow y rows
      obsPos = nextObs (< x) obs'
   in case obsPos of
        [x'] -> Just (x' + 1, y)
        []   -> Nothing

move MRight (x, y) (_, rows) =
  let obs' = reverse $ obstaclesInRow y rows
      obsPos = nextObs (> x) obs'
   in case obsPos of
        [x'] -> Just (x' - 1, y)
        []   -> Nothing

move MUp (x, y) (cols, _) =
  let obs' = obstaclesInCol x cols
      obsPos = nextObs (< y) obs'
   in case obsPos of
        [y'] -> Just (x, y' + 1)
        []   -> Nothing

move MDown (x, y) (cols, _) =
  let obs' = reverse $ obstaclesInCol x cols
      obsPos = nextObs (> y) obs'
   in case obsPos of
        [y'] -> Just (x, y' - 1)
        []   -> Nothing

nextObs :: (Integer -> Bool) -> [Integer] -> [Integer]
nextObs fun obs = take 1 $ filter fun obs

obstaclesInCol :: Ord k => k -> Map.Map k [a] -> [a]
obstaclesInCol x cols = Map.findWithDefault [] x cols

obstaclesInRow :: Ord k => k -> Map.Map k [a] -> [a]
obstaclesInRow y rows = Map.findWithDefault [] y rows

traverseMap :: Dir -> Coord -> Coord -> (Map.Map Integer [Integer], Map.Map Integer [Integer]) -> Set Coord -> Int
traverseMap dir pos limits obs acc =
  case move dir pos obs of
    Just nextPos -> traverseMap (nextDir dir) nextPos limits obs (addVisited pos nextPos acc)
    Nothing -> Set.size $ addVisited pos (outerPos dir pos limits) acc
  where
    addVisited (x, y) (x', y') visited =
      let [x1, x2] = sort [x, x']
          [y1, y2] = sort [y, y']
          positions = [(xx, yy) | xx <- [x1..x2], yy <- [y1..y2]]
       in foldr Set.insert visited positions

    outerPos MDown  (x, _) (_, my) = (x,  my)
    outerPos MLeft  (_, y) (_,  _) = (1,  y)
    outerPos MRight (_, y) (mx, _) = (mx, y)
    outerPos MUp    (x, _) (_,  _) = (x,  1)

    nextDir direction =
      case direction of
        MDown  -> MLeft
        MLeft  -> MUp
        MRight -> MDown
        MUp    -> MRight

prepareMap :: [Coord] -> (Map.Map Integer [Integer], Map.Map Integer [Integer])
prepareMap obs = helper Map.empty Map.empty obs
  where
    helper cols rows [] = (cols, rows)
    helper cols rows ((x, y) : rest) =
      let newCols = Map.insertWith (<>) x [y] cols
          newRows = Map.insertWith (<>) y [x] rows
       in helper newCols newRows rest

parse :: String -> (Coord, Coord, [Coord])
parse input = doParse (1, 1) [] (1, 1) input
  where
    doParse start  obs (x, y) ""          = ((x - 1, y), start, Data.List.sort obs)
    doParse start  obs (x, y) ('.':rest)  = doParse start obs (x + 1, y) rest
    doParse _start obs (x, y) ('^':rest)  = doParse (x, y) obs (x + 1, y) rest
    doParse start  obs (x, y) ('#':rest)  = doParse start ((x,y):obs) (x + 1, y) rest
    doParse start  obs (_, y) ('\n':rest) = doParse start obs (1, y + 1) rest
