-- Day 22: Monkey Market
module Day22 where

import Data.Bits ((.&.), shift, xor)

part1 :: String -> Int
part1 input = sum . map (secretNumber 2000) . parse $ input

secretNumber :: Int -> Int -> Int
secretNumber 0 x = x
secretNumber n x =
  let x' = calc 11 . calc (-5) . calc 6 $ x
   in secretNumber (n - 1) x'
  where
    calc n x = ((x `shift` n) `xor` x) .&. 16777215

parse :: String -> [Int]
parse = map (\x -> read x :: Int) . lines
