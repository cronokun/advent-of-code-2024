-- Day 15, part 2: Bigger Warehouse Woes
module Day15Part2 (part2) where

import Helpers (lineGroups)
import Data.List (find, nub)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set

data Grid = Grid { robot :: Coord
                 , boxes :: Tiles Box
                 , walls :: Tiles Coord
                 } deriving (Show)

data Move = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Eq, Show)
type Coord = (Int, Int)
type Box = (Coord, Coord)
type Tiles a = Set.Set a

-- Returns sum of all boxes' GPS coordinates on a bigger map.
part2 :: String -> Int
part2 input =
  let (grid, moves) = parse input
      grid' = foldl moveRobot grid moves
      bxs = Set.toList $ boxes grid'
   in sum $ map gpsCoord bxs
  where
   gpsCoord :: Box -> Int
   gpsCoord ((x, y), _) = x + y * 100

moveRobot :: Grid -> Move -> Grid
moveRobot grid dir
  | isWall grid np = grid
  | isJust box =
      case maybeMoveBoxes dir (fromJust box) grid of
        Just boxes' -> grid { robot = np, boxes = boxes' }
        Nothing -> grid
  | otherwise = grid { robot = np }
  where
    r = robot grid
    np = moveTo dir r
    box = maybeBox grid np

maybeMoveBoxes :: Move -> Box -> Grid -> Maybe (Tiles Box)
maybeMoveBoxes dir box grid =
  let bs = boxSeq [box] []
   in case canMoveAllBoxes bs of
        True -> Just (updateGridBoxes bs)
        False -> Nothing
  where
    boxSeq :: [Box] -> [Box] -> [Box]
    boxSeq [] acc = acc
    boxSeq (b:bs) acc =
      case nextBox b of
        [] -> boxSeq bs (b:acc)
        bxs -> boxSeq (nub (bxs <> bs)) (b:acc)
      where
        nextBox :: Box -> [Box]
        nextBox ((x1,y), (x2,_))
          | dir == MoveLeft = maybeToList $ maybeBox grid (x1 - 1, y)
          | dir == MoveRight = maybeToList $ maybeBox grid (x1 + 2, y)
          | dir == MoveUp =
            let b1 = maybeToList $ maybeBox grid (x1, y - 1)
                b2 = maybeToList $  maybeBox grid (x2, y - 1)
             in nub (b1 <> b2)
          | dir == MoveDown =
          let b1 = maybeToList $ maybeBox grid (x1, y + 1)
              b2 = maybeToList $  maybeBox grid (x2, y + 1)
           in nub (b1 <> b2)

    canMoveAllBoxes :: [Box] -> Bool
    canMoveAllBoxes = not . any boxNextToWall
      where
        boxNextToWall :: Box -> Bool
        boxNextToWall ((x1,y), (x2,_))
          | dir == MoveLeft = isWall grid (x1 - 1, y)
          | dir == MoveRight = isWall grid (x1 + 2, y)
          | dir == MoveUp = (isWall grid (x1, y - 1)) || (isWall grid (x2, y - 1))
          | dir == MoveDown = (isWall grid (x1, y + 1)) || (isWall grid (x2, y + 1))

    updateGridBoxes :: [Box] -> (Tiles Box)
    updateGridBoxes bs =
      let newBoxes = map (\(p1, p2) -> (moveTo dir p1, moveTo dir p2)) bs
          withoutOldBoxes = foldr (Set.delete) (boxes grid) bs
          bs' = foldr (Set.insert) withoutOldBoxes newBoxes
       in bs'

    maybeToList :: Maybe a -> [a]
    maybeToList = \case
      Just x -> [x]
      Nothing -> []

-- See if there is a box next to a given position in the given direction:
maybeBox :: Grid -> Coord -> Maybe Box
maybeBox grid (x,y)
  | Set.member b1 boxes' = Just b1
  | Set.member b2 boxes' = Just b2
  | otherwise = Nothing
  where 
    b1 = ((x, y), (x + 1, y))
    b2 = ((x - 1, y), (x, y))
    boxes' = boxes grid

isWall :: Grid -> Coord -> Bool
isWall grid p = Set.member p $ walls grid

moveTo :: Move -> Coord -> Coord
moveTo dir (x, y) =
  case dir of
    MoveUp ->    (x, y - 1)
    MoveDown ->  (x, y + 1)
    MoveLeft ->  (x - 1, y)
    MoveRight -> (x + 1, y)

parse :: String -> (Grid, [Move])
parse input =
  let [m, moves'] = lineGroups input
      m' = map (concatMap expandTile) m
      g = makeGrid m'
      moves = map toMove $ concat moves'
   in (g, moves)
  where
    makeGrid m =
      let tiles = parseMap m
          (r, '@') = fromJust . find ((== '@') . snd) $ tiles
          bs = map expandBox . filter ((== '[') . snd) $ tiles
          ws = map fst . filter ((== '#') . snd) $ tiles
       in Grid { robot = r
               , boxes = Set.fromList bs
               , walls = Set.fromList ws }
      where
        expandBox ((x, y), '[') = ((x, y), (x + 1, y))

    parseMap m =
      let mx = length (head m) - 1
          my = length m - 1
       in helper (0,0) (mx,my) [] $ unlines m
      where
        helper _pos _lim acc [] = acc
        helper p@(x,y) (mx,my) acc (ch:rest)
          | ch == '\n' = helper (0, y+1) (mx, my) acc rest
          | ch == ']'  = helper (x+1, y) (mx, my) acc rest
          | ch == '.'  = helper (x+1, y) (mx, my) acc rest
          | otherwise = helper (x+1, y) (mx, my) ((p, ch) : acc) rest

    expandTile = \case
      '#' -> "##"
      '.' -> ".."
      '@' -> "@."
      'O' -> "[]"

    toMove = \case
      '^' -> MoveUp
      'v' -> MoveDown
      '<' -> MoveLeft
      '>' -> MoveRight

