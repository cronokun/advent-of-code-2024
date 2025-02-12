-- Day 15: Warehouse Woes
module Day15 (part1) where

import Data.List (find, partition, sort, sortBy)
import Helpers (lineGroups)

data Grid = Grid { robot :: Coord
                 , boxes :: [Coord]
                 , walls :: [Coord]
                 } deriving (Show)

data Move = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Eq, Show)
type Coord = (Int, Int)

part1 :: String -> Int
part1 input =
  let (grid, moves) =parse input
      grid' = doMoves grid moves
  in sum $ map gpsCoord (boxes grid')
 where
   gpsCoord :: Coord -> Int
   gpsCoord (x, y) = x + y * 100

doMoves :: Grid -> [Move] -> Grid
doMoves grid moves = foldl move grid $ zip [0..] moves
  where
    move :: Grid -> (Int, Move) -> Grid
    move grd (_, dir) =
      let pos = robot grd
          pos' = moveInDir dir pos
          isBox = pos' `elem` boxes grd
          isWall = pos' `elem` walls grd
       in case (isBox, isWall) of
            (True, False) ->
              -- try to move box(es)
              case maybeMoveBoxes dir pos grd of
                Just bs' ->
                  updateGridRobot pos' $ updateGridBoxes bs' grd
                Nothing ->
                  grd
            (False, True) ->
              -- can't move into the wall
              grd
            _otherwise ->
              -- move robot to free space
              updateGridRobot pos' grd

maybeMoveBoxes :: Move -> Coord -> Grid -> Maybe [Coord]
maybeMoveBoxes dir pos grid =
  let wp = getClosestWall dir pos grid
      (bs, rest) = getClosestBoxes dir pos wp grid
      boxesToMove = sortByDir dir bs 
   in case tryMove dir pos wp boxesToMove of
        Just (moved, rest') ->
          Just (concat [moved, rest', rest])
        Nothing ->
          Nothing

tryMove :: Move -> Coord -> Coord -> [Coord] -> Maybe ([Coord], [Coord])
tryMove dir pos wp bxs = helper [] pos wp bxs
  where
    helper acc pos wp bs
      | isAdjacent pos wp = Nothing -- bumped into wall, can't move!
      | null bs = Just (acc, [])
      | isAdjacent pos (head bs) = helper ((moveInDir dir (head bs)) : acc) (head bs) wp (tail bs)
      | otherwise = Just (acc, bs) -- found space, okay to move!

    isAdjacent (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1) == 1

moveInDir :: Move -> Coord -> Coord
moveInDir dir (x, y) =
  case dir of
    MoveUp ->    (x, y - 1)
    MoveDown ->  (x, y + 1)
    MoveLeft ->  (x - 1, y)
    MoveRight -> (x + 1, y)

sortByDir :: Move -> [Coord] -> [Coord]
sortByDir dir bs =
  let s = case dir of
            MoveUp -> (\(x1, y1) (x2, y2) -> y2 `compare` y1)
            MoveDown -> (\(x1, y1) (x2, y2) -> y1 `compare` y2)
            MoveLeft -> (\(x1, y1) (x2, y2) -> x2 `compare` x1)
            MoveRight -> (\(x1, y1) (x2, y2) -> x1 `compare` x2)
   in sortBy s bs

updateGridBoxes :: [Coord] -> Grid -> Grid
updateGridBoxes bs grid = grid { boxes = bs }

updateGridRobot :: Coord -> Grid -> Grid
updateGridRobot pos grid = grid { robot = pos }

getClosestBoxes :: Move -> Coord -> Coord -> Grid -> ([Coord], [Coord])
getClosestBoxes dir (x,y) (wx,wy) grid =
  let p = case dir of
            MoveUp    -> (\(x',y') -> x == x' && y > y' && wy < y')
            MoveDown  -> (\(x',y') -> x == x' && y < y' && wy > y')
            MoveLeft  -> (\(x',y') -> x > x' && x' > wx && y == y')
            MoveRight -> (\(x',y') -> x < x' && x' < wx && y == y')
   in partition p $ boxes grid

getClosestWall :: Move -> Coord -> Grid -> Coord
getClosestWall dir (rx, ry) grid =
  let p = case dir of
            MoveUp    -> (\(x,y) -> rx == x && ry > y)
            MoveDown  -> (\(x,y) -> rx == x && ry < y)
            MoveLeft  -> (\(x,y) -> rx > x && ry == y)
            MoveRight -> (\(x,y) -> rx < x && ry == y)
      (Just pos') = find p . sortByDir dir . walls $ grid
   in pos' 

parse :: String -> (Grid, [Move])
parse input =
  let [map', moves'] = lineGroups input
      (r, bs, ws) = parseMap (0,0) (0,0) [] [] $ unlines map'
      grid = Grid { robot = r, boxes = sort bs, walls = sort ws }
      moves = map toMove $ concat moves'
   in (grid, moves)
  where
    parseMap (x,y) r bs ws (c:rest)
      | null rest = (r, bs, ws)
      | c == '\n' = parseMap (0, y + 1) r bs ws rest
      | c == '@'  = parseMap (x + 1, y) (x, y) bs ws rest
      | c == 'O'  = parseMap (x + 1, y) r ((x, y):bs) ws rest
      | c == '#'  = parseMap (x + 1, y) r bs ((x, y):ws) rest
      | otherwise = parseMap (x + 1, y) r bs ws rest

    toMove x = case x of
                 '^' -> MoveUp
                 'v' -> MoveDown
                 '<' -> MoveLeft
                 '>' -> MoveRight
