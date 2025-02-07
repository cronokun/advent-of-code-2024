-- Day 14: Restroom Redoubt
module Day14 (part1, part2) where

import Data.List (groupBy, sortBy)

type Robot = ((Int, Int), (Int, Int))

-- Returns safety factor after 100 seconds.
part1 :: (Int, Int) -> String -> Int
part1 grid@(mx, my) input =
  product . countByQuadrant . moveRobots grid 100 . parse $ input
    where
      countByQuadrant :: [Robot] -> [Int]
      countByQuadrant rs =
        let hx = mx `div` 2
            hy = my `div` 2
            ps = map fst rs
        in helper hx hy 0 0 0 0 ps
       where
         helper _ _ a b c d [] = [a, b, c, d]
         helper hx hy a b c d ((x, y):rest)
           | x < hx && y < hy = helper hx hy (a+1) b c d rest
           | x > hx && y < hy = helper hx hy a (b+1) c d rest
           | x < hx && y > hy = helper hx hy a b (c+1) d rest
           | x > hx && y > hy = helper hx hy a b c (d+1) rest
           | otherwise = helper hx hy a b c d rest -- in the middle, ignore

{- Returns fewest seconds needed for the robots to display the Christmas tree.

   *******************************
   *                             *
   *                             *
   *                             *
   *                             *
   *              *              *
   *             ***             *
   *            *****            *
   *           *******           *
   *          *********          *
   *            *****            *
   *           *******           *
   *          *********          *
   *         ***********         *
   *        *************        *
   *          *********          *
   *         ***********         *
   *        *************        *
   *       ***************       *
   *      *****************      *
   *        *************        *
   *       ***************       *
   *      *****************      *
   *     *******************     *
   *    *********************    *
   *             ***             *
   *             ***             *
   *             ***             *
   *                             *
   *                             *
   *                             *
   *                             *
   *******************************

   Max steps is 103*101. Skip steps to speed up runtime.

-}
part2 :: (Int, Int) -> String -> Int
part2 grid input = moveTillHasTree 7000 $ parse input
  where
    moveTillHasTree :: Int -> [Robot] -> Int
    moveTillHasTree n rs =
      let rs' = moveRobots grid n rs
       in if any isLined $ rows rs'
          then n
          else moveTillHasTree (n + 1) rs

    isLined :: [Int] -> Bool
    isLined xs =
      let conseq = length . filter (\(a, b) -> b - a == 1) $ zip xs (drop 1 xs)
       in conseq >= 30

    rows :: [Robot] -> [[Int]]
    rows =
      map (map fst) .
      groupBy (\(_, y1) (_, y2) -> y1 == y2) .
      sortBy (\(x1, y1) (x2, y2) -> (y1,x1) `compare` (y2,x2)) .
      map fst

moveRobots :: (Int, Int) -> Int -> [Robot] -> [Robot]
moveRobots (mx, my) n rs = map move rs
  where
    move ((px, py), (vx, vy)) =
      let x = warp px vx mx
          y = warp py vy my
       in ((x, y), (vx, vy))

    warp p v m =
      let r = (p + v * n) `rem` m
       in if r >= 0 then r else r + m


parse :: String -> [Robot]
parse input = map toRobot . lines $ input
  where
    toRobot str =
      let (p, v) = split ' ' str
       in (parseNums p, parseNums v)
    
    parseNums str =
      let (x, y) = split ',' . drop 2 $ str
       in (read x, read y) :: (Int, Int)
          
    split p s = let (a, b) = break (== p) s in (a, drop 1 b)
