-- Day 22: Monkey Market
module Day22 (part1, part2) where

import Data.Bits ((.&.), shift, xor)
import qualified Data.IntMap.Lazy as M

type Memo = M.IntMap Int

part1 :: String -> Int
part1 input = sum . map (secretNumberAt 2000) $ parse input
  where
    secretNumberAt n x = iterate nextSecretNumber x !! n

part2 :: String -> Int
part2 input = topPrice $ parse input

nextSecretNumber :: Int -> Int
nextSecretNumber num = calc 11 . calc (-5) . calc 6 $ num
  where
    calc n x = ((x `shift` n) `xor` x) .&. 16777215

topPrice :: [Int] -> Int
topPrice xs = run M.empty xs
  where
    run :: Memo -> [Int] -> Int
    run memo [] = maximum $ M.elems memo
    run memo (x:xs) =
      let m = priceForSequence x
          memo' = M.unionWith (+) memo m
       in run memo' xs

priceForSequence :: Int -> Memo
priceForSequence x =
  let xs = take 2000 . map (`mod` 10) . iterate nextSecretNumber $ x
   in M.fromListWith (const id) $ groupBy4 xs
  where
    groupBy4 (a : b : c : d : e : xs) =
      let delta = toKey (b - a, c - b, d - c, e - d)
       in (delta, e) : groupBy4 (b : c : d : e : xs)
    groupBy4 _ = []

toKey :: (Int, Int, Int, Int) -> Int
toKey (a, b, c, d) =
  let a' = (a + 9) `shift` 15
      b' = (b + 9) `shift` 10
      c' = (c + 9) `shift` 5
      d' = (d + 9)
   in a' + b' + c' + d'


price :: Int -> Int
price x = x `rem` 10

parse :: String -> [Int]
parse = map (\x -> read x :: Int) . lines
