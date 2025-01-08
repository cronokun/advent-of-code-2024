-- Day 6: Guard Gallivant
module Day6 (part1, part2) where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Integer, Integer)
type Move = (Dir, Integer, (Integer, Integer))
type ObsMap = Map.Map Integer [Integer]
data Dir = MDown | MLeft | MRight | MUp deriving (Eq, Show)

part1 :: String -> Int
part1 input =
  let (limits, start, obs) = parse input
      obs' = prepareMap obs
      path = traverseMap MUp start limits obs' []
   in Set.size $ expandPath path

part2 :: String -> Int
part2 input =
  let (limits, start, obs) = parse input
      obs' = prepareMap obs
      path = Set.toList . expandPath $ traverseMap MUp start limits obs' []
      validObs = putObsAndTraverse [] start limits obs' path
  in length validObs

move :: Dir -> Coord -> (ObsMap, ObsMap) -> Maybe Coord
move MLeft (x, y) (_, rows) =
  let obs' = obstaclesInRow y rows
      obsPos = nextObs (< x) obs'
   in case obsPos of
        [x'] -> Just (x' + 1, y)
        _ -> Nothing

move MRight (x, y) (_, rows) =
  let obs' = reverse $ obstaclesInRow y rows
      obsPos = nextObs (> x) obs'
   in case obsPos of
        [x'] -> Just (x' - 1, y)
        _ -> Nothing

move MUp (x, y) (cols, _) =
  let obs' = obstaclesInCol x cols
      obsPos = nextObs (< y) obs'
   in case obsPos of
        [y'] -> Just (x, y' + 1)
        _ -> Nothing

move MDown (x, y) (cols, _) =
  let obs' = reverse $ obstaclesInCol x cols
      obsPos = nextObs (> y) obs'
   in case obsPos of
        [y'] -> Just (x, y' - 1)
        _ -> Nothing

nextObs :: (Integer -> Bool) -> [Integer] -> [Integer]
nextObs fun obs = take 1 $ filter fun obs

obstaclesInCol :: Ord k => k -> Map.Map k [a] -> [a]
obstaclesInCol x cols = Map.findWithDefault [] x cols

obstaclesInRow :: Ord k => k -> Map.Map k [a] -> [a]
obstaclesInRow y rows = Map.findWithDefault [] y rows

-- Traverse map until go outside or a loop is detected. If obstacle is met, turn right before the
-- obstacle and continue traversal.
traverseMap :: Dir -> Coord -> Coord -> (ObsMap, ObsMap) -> [Move] -> [Move]
traverseMap dir pos@(x, y) limits@(mx, my) obs acc =
  case move dir pos obs of
    Just next -> maybeContinue next
    Nothing -> (movement outerPos):acc
  where
    movement (x', y') =
      case dir of
        MDown -> (dir, x, (y, y'))
        MUp -> (dir, x, (y, y'))
        MLeft -> (dir, y, (x, x'))
        MRight -> (dir, y, (x, x'))

    maybeContinue p =
      case elem (movement p) acc of
        True -> []  -- It's a loop! Abort!
        False -> traverseMap nextDir p limits obs ((movement p):acc)

    outerPos =
      case dir of
        MDown -> (x, my)
        MUp -> (x, 1)
        MLeft -> (1, y)
        MRight -> (mx, y)

    nextDir =
      case dir of
        MDown  -> MLeft
        MLeft  -> MUp
        MRight -> MDown
        MUp    -> MRight

expandPath :: [Move] -> Set Coord
expandPath moves = reduce Set.empty moves
  where
    reduce acc [] = acc
    reduce acc (m:ms) =
      let acc' = foldr Set.insert acc (moveToList m)
      in reduce acc' ms

    moveToList  (MDown, x, (y1, y2)) = [(x, y) | y <- [y1..y2]]
    moveToList    (MUp, x, (y1, y2)) = [(x, y) | y <- [y2..y1]]
    moveToList  (MLeft, y, (x1, x2)) = [(x, y) | x <- [x2..x1]]
    moveToList (MRight, y, (x1, x2)) = [(x, y) | x <- [x1..x2]]


-- Put a new obstacle on the path and try to traverse. Increase acc if loop detected.
putObsAndTraverse :: [Coord] -> Coord -> Coord -> (ObsMap, ObsMap) -> [Coord] -> [Coord]
putObsAndTraverse acc _ _ _ [] = acc

putObsAndTraverse acc start limits obs@(cols, rows) (p:path) =
  let obs' = putNewObs p
      newPath = traverseMap MUp start limits obs' []
      acc' = if newPath == [] then (p:acc) else acc
   in putObsAndTraverse acc' start limits obs path
  where
    putNewObs (x, y) =
      (putInMap x [y] cols, putInMap y [x] rows)
    putInMap = Map.insertWith (\v vs -> reverse . sort $ v <> vs)


-- Convert a list of obstacles into Maps: obstacles by columns and by rows.
prepareMap :: [Coord] -> (ObsMap, ObsMap)
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
    doParse start  obs (x, y) []          = ((x - 1, y), start, Data.List.sort obs)
    doParse _start obs (x, y) ('^':rest)  = doParse (x, y) obs (x + 1, y) rest
    doParse start  obs (x, y) ('#':rest)  = doParse start ((x,y):obs) (x + 1, y) rest
    doParse start  obs (_, y) ('\n':rest) = doParse start obs (1, y + 1) rest
    doParse start  obs (x, y) (_:rest)  = doParse start obs (x + 1, y) rest
