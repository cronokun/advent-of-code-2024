-- Day 11: Plutonian Pebbles
module Day11 (part1) where

-- How many stones will you have after blinking N times?
part1 :: Int -> String -> Int
part1 n input = length . process n . parse $ input

process :: Int -> [Int] -> [Int]
process 0 xs = xs
process c nums = process (c - 1) $ helper [] nums
  where
    helper :: [Int] -> [Int] -> [Int]
    helper acc [] = reverse acc
    helper acc (x:xs)
      | x == 0 = helper (1:acc) xs
      | hasEvenDigits x =
          let (l, r) = splitNum x
          in helper (r:l:acc) xs
      | otherwise = helper ((x * 2024) : acc) xs

    hasEvenDigits :: Int -> Bool
    hasEvenDigits x = even . numberOfDigits $ x

    numberOfDigits :: Int -> Int
    numberOfDigits num = helper' 1 num
      where
        helper' acc n =
          case n `div` 10 of
            0 -> acc
            m -> helper' (acc + 1) m

    splitNum :: Int -> (Int, Int)
    splitNum x =
      let len = numberOfDigits x
          d = 10 ^ (len `div` 2)
          l = x `div` d
          r = x `rem` d
       in (l, r)

parse :: String -> [Int]
parse = map (\x -> read x :: Int) . words
