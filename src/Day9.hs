-- Day 9: Disk Fragmenter
module Day9 (part1) where

import Helpers (inspect)

data DiskMode = FileMode | SpaceMode deriving (Eq, Show)
type Block = (DiskMode, Integer, Int)

-- Returns compressed filesystem checksum
part1 :: String -> Integer
part1 input =
  let str = parse input
      blocks = mapToBlocks str
      spaces = totalSpace blocks
   in checksum $ compress spaces blocks
  where totalSpace = sum . map (\(SpaceMode, _, n) -> n) . filter (\(t, _, _) -> t == SpaceMode)

mapToBlocks :: String -> [Block]
mapToBlocks str = helper [] 0 FileMode str
  where
    helper :: [Block] -> Integer -> DiskMode -> String -> [Block]
    helper acc i _mode     []       = reverse acc
    helper acc i FileMode  ('0':xs) = helper acc (i + 1) SpaceMode xs
    helper acc i SpaceMode ('0':xs) = helper acc i       FileMode  xs
    helper acc i FileMode  (x:xs)   = helper ((FileMode,  i, charToInt x):acc) (i + 1) SpaceMode xs
    helper acc i SpaceMode (x:xs)   = helper ((SpaceMode, 0, charToInt x):acc)  i      FileMode  xs

compress :: Int -> [Block] -> [Integer]
compress spaces blks = helper [] spaces blks (reverse blks)
  where
    helper acc 0 _ fs = reverse $ addRemainingFills acc fs

    helper acc
           c
           blks@((SpaceMode, 0, n):_)
           fills@((SpaceMode, 0, m):_)
           = helper acc (c - 1) blks (reduce fills)

    helper acc
           c
           blks@((SpaceMode, 0, n):_)
           fills@((FileMode, i, m):_)
           = helper (i:acc) (c - 1) (reduce blks) (reduce fills)

    helper acc
           c
           blks@((FileMode, i, n):_)
           fs
           = helper (i:acc) c (reduce blks) fs

    addRemainingFills acc fills@((FileMode, i, n):_) = addRemainingFills (i:acc) (reduce fills)
    addRemainingFills acc ((SpaceMode, 0, _):_) = acc

    reduce ((t, i, n):rest)
      | n == 1 = rest
      | otherwise = ((t, i, n - 1):rest)

checksum :: [Integer] -> Integer
checksum = toInteger . sum . map (uncurry (*)) . zip [0..]

charToInt :: Char -> Int
charToInt n = read [n] :: Int

parse :: String -> String
parse input = head $ lines input
