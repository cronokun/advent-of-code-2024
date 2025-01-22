-- Day 9: Disk Fragmenter
module Day9 (part1, part2) where

import Data.List (partition, sortOn)

data BMode = FB | SB deriving (Eq, Show)
type Block = (BMode, Integer, Integer, Integer)

-- Returns compressed filesystem checksum
part1 :: String -> Integer
part1 input =
  let str = init input
      blocks = mapToBlocks str
      spaces = totalSpace blocks
   in checksum $ compact1 spaces blocks
  where
    checksum :: [Integer] -> Integer
    checksum = toInteger . sum . map (uncurry (*)) . zip [0..]

    totalSpace :: [Block] -> Integer
    totalSpace = sum . map getLen . filter isSpace
      where
        getLen (_, _, _, n) = n
        isSpace (t, _, _, _) = t == SB

-- Returns checksum for compressed filesystem v2.0
part2 :: String -> Integer
part2 input = checksum . compact2 . mapToBlocks $ init input
  where
    checksum :: [Block] -> Integer
    checksum = sum . map blkChecksum
      where
        blkChecksum (FB, i, inx, n) = sum $ map (* i) [inx..(inx + n - 1)]

-- Moves file blocks from the right to free blocks on the left.
compact1 :: Integer -> [Block] -> [Integer]
compact1 spaces blks = helper [] spaces blks (reverse blks)
  where
    helper acc 0 _ fs = reverse $ addFills acc fs

    helper acc
           c
           blks@((SB, 0, _, n):_)
           fills@((SB, 0, _, m):_)
           = helper acc (c - 1) blks (reduce fills)

    helper acc
           c
           blks@((SB, 0, _, n):_)
           fills@((FB, i, _, m):_)
           = helper (i:acc) (c - 1) (reduce blks) (reduce fills)

    helper acc
           c
           blks@((FB, i, _, n):_)
           fs
           = helper (i:acc) c (reduce blks) fs

    addFills acc fills@((FB, i, _, n):_) = addFills (i:acc) (reduce fills)
    addFills acc ((SB, 0, _, _):_) = acc

    reduce ((t, i, x, n):rest)
      | n == 1 = rest
      | otherwise = ((t, i, x, n - 1):rest)

-- Moves whole files (if can) from the right to free block on the left.
compact2 :: [Block] -> [Block]
compact2 blks = 
  let (fbs, sbs) = splitByType blks 
   in helper [] sbs (reverse fbs)
  where
    helper acc _ [] = sortByIndex acc
    helper acc fs (x:xs) =
      let (blk, fs') = findFit [] x fs
       in helper (blk:acc) fs' xs

    findFit :: [Block] -> Block -> [Block] -> (Block, [Block])
    findFit acc blk [] = (blk, reverse acc)
    findFit acc blk (f:fs)
        -- file block is leftier than the first free space block, can't be moved
      | isLeft blk f = (blk, reverse acc <> fs)
      | otherwise = 
          case fillBlk f blk of
            [] -> findFit (f:acc) blk fs
            [b] -> (b, reverse acc <> fs)
            [b,s] -> (b, reverse (s:acc) <> fs)

    fillBlk (SB, _, i1, l1) (FB, bid, i2, l2)
      | l1 < l2  = []
      | l1 == l2 = [(FB, bid, i1, l1)]
      | l1 > l2  = [(FB, bid, i1, l2), (SB, 0, i1 + l2, l1 - l2)]

    isLeft (FB, _, i, _) (SB, _, j, _) = i < j

    sortByIndex = sortOn (\(_, _, i, _) -> i)
    splitByType = partition (\(t, _, _, _) -> t == FB)

mapToBlocks :: String -> [Block]
mapToBlocks str = helper [] 0 0 FB str
  where
    helper :: [Block] -> Integer -> Integer -> BMode -> String -> [Block]
    helper acc _ _ _ [] = reverse acc
    -- Ignore zero-length blocks
    helper acc bid idx FB ('0':xs) = helper acc (bid + 1) idx SB xs
    helper acc bid idx SB ('0':xs) = helper acc bid idx FB xs

    -- Add file block
    helper acc bid idx FB (x:xs) =
      let n = charToInt x
          blk = (FB, bid, idx, n)
       in helper (blk:acc) (bid + 1) (idx + n) SB xs

    -- Add free space block
    helper acc bid idx SB (x:xs) =
      let n = charToInt x
          blk = (SB, 0, idx, n)
      in helper (blk:acc) bid (idx + n) FB xs

    charToInt :: Char -> Integer
    charToInt n = read [n] :: Integer
