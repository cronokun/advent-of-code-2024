-- Day 22: Monkey Market
module Day22 (part1, part2) where

import Data.Bits ((.&.), shift, xor)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

type Memo = M.Map Int Int

part1 :: String -> Int
part1 input = sum . map (secretNumberAt 2000) . parse $ input

part2 :: String -> Int
part2 input = topPrice $ parse input

secretNumberAt :: Int -> Int -> Int
secretNumberAt 0 x = x
secretNumberAt n x = secretNumberAt (n - 1) $ nextSecretNumber x

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
      let res = sequentize $ priceAndChanges 2000 x
          acc' = foldl (\m (p, k) -> M.insertWith (+) k p m) memo res
       in run acc' xs

priceAndChanges :: Int -> Int -> [(Int, Int)]
priceAndChanges n num = run [(price num, 0)] n num
  where
    run acc 0 _ = reverse acc
    run acc@((p, _) : _) n' x =
      let x' = nextSecretNumber x
          p' = price x'
      in run ((p', p' - p) : acc) (n' - 1) x'

sequentize :: [(Int, Int)] -> [(Int, Int)]
sequentize xs =
  let (as, xs') = splitAt 4 . drop 1 $ xs
      p = fst $ last as
      seq = tuple4 as
      key = toKey seq
  in run [(p, key)] S.empty seq xs'
  where
    run acc s prev [] = reverse acc
    run acc s prev ((p, c) : xs) =
      let seq = shift4 prev c
          key = toKey seq
          (acc', s') = if S.member key s
                          then (acc, s)
                          else ((p, key) : acc, S.insert key s)
       in run acc' s' seq xs

    shift4 (_, a, b, c) x = (a, b, c, x)
    tuple4 [(_, a), (_, b), (_, c), (_, d)] = (a, b, c, d)

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
