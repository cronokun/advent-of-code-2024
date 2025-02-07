-- Day 14: Restroom Redoubt
module Day14 (part1) where

type Robot = ((Int, Int), (Int, Int))

-- Returns safety factor after 100 seconds.
part1 :: (Int, Int) -> String -> Int
part1 (mx, my) input =
  product . countByQuadrant . map moveRobot . parse $ input
  where
    moveRobot :: Robot -> (Int, Int)
    moveRobot ((px, py), (vx, vy)) =
      let x = warp px vx mx
          y = warp py vy my
      in (x, y)
      where
        warp p v m =
          let r = (p + v * 100) `rem` m
           in if r >= 0 then r else r + m

    countByQuadrant :: [(Int, Int)] -> [Int]
    countByQuadrant ps =
      let hx = mx `div` 2
          hy = my `div` 2
      in helper hx hy 0 0 0 0 ps
     where
       helper _ _ a b c d [] = [a, b, c, d]
       helper hx hy a b c d ((x, y):rest)
         | x < hx && y < hy = helper hx hy (a+1) b c d rest
         | x > hx && y < hy = helper hx hy a (b+1) c d rest
         | x < hx && y > hy = helper hx hy a b (c+1) d rest
         | x > hx && y > hy = helper hx hy a b c (d+1) rest
         | otherwise = helper hx hy a b c d rest -- in the middle, ignore

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
