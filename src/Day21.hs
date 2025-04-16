-- Day 21: Keypad Conundrum
module Day21 (part1) where

import qualified Data.List as L
import qualified Data.Map as M

type Code = (Int, [Char])

-- Returns total complexity of all codes
part1 :: String -> Int
part1 input =
  let codes = parse input
      complexities = map (\(n, xs) -> n * codeSequence xs) codes
   in sum complexities

codeSequence :: [Char] -> Int
codeSequence code = minimum . seq3 . seq2 . seq1 $ [code]

-- Returns key sequence on door's keypad
seq1 :: [String] -> [String]
seq1 xs = seqWith numKeySeq xs

-- Returns key sequence on robot's keypad
seq2 :: [String] -> [String]
seq2 xs = seqWith dirKeySeq xs

seqWith :: (Char -> Char -> [String]) -> [String] -> [String]
seqWith f xs = L.nub $ concatMap (helper [] 'A') xs
  where
    helper acc key "" = acc
    helper acc key (x:xs) =
      let acc' = combine acc $ f key x
       in helper acc' x xs

    combine :: [String] -> [String] -> [String]
    combine [] xs = xs
    combine as bs = (map (<>) as) <*> bs

-- Returns only lengths of possible sequences.
--
-- Note: seq2 for a seq2 code produces codes of the same length, so we don't
--       need to get them all, just one.
seq3 :: [String] -> [Int]
seq3 xs = map (\x -> helper [] 'A' x) xs
  where
    helper acc key [] = length acc
    helper acc key (x:xs) =
      let s = head $ dirKeySeq key x
       in helper (acc <> s) x xs

-- All possible sequnces on numeric keypad.
numKeySeq :: Char -> Char -> [String]
numKeySeq '0' '1' = ["^<A"]
numKeySeq '0' '4' = ["^^<A", "^<^A"]
numKeySeq '0' '7' = ["^^^<A", "^^<^A", "^<^^A"]
numKeySeq '1' '0' = [">vA"]
numKeySeq '1' 'A' = [">v>A", ">>vA"]
numKeySeq '4' '0' = [">vvA", "v>vA"]
numKeySeq '4' 'A' = ["v>>vA", ">v>vA", ">>vvA", "v>v>A", ">vv>A"]
numKeySeq '7' '0' = [">vvvA", "vv>vA", "v>vvA"]
numKeySeq '7' 'A' = ["v>>vvA", ">v>vvA", "vv>>vA", "v>v>vA", ">vv>vA", ">>vvvA", "vv>v>A", "v>vv>A", ">vvv>A"]
numKeySeq 'A' '1' = ["<^<A", "^<<A"]
numKeySeq 'A' '4' = ["^<<^A", "<^<^A", "^^<<A", "^<^<A", "<^^<A"]
numKeySeq 'A' '7' = ["^<<^^A", "<^<^^A", "^^<<^A", "^<^<^A", "<^^<^A", "^^^<<A", "^^<^<A", "^<^^<A", "<^^^<A"]

numKeySeq fromChar toChar =
  let Just (x1, y1) = M.lookup fromChar numKeypad
      Just (x2, y2) = M.lookup toChar numKeypad
      moves = intToXKeypresses (x2 - x1) <> intToYKeypresses (y2 - y1)
      sqs = L.nub $ L.permutations moves
   in map (<> "A") sqs
  where
    intToXKeypresses dx
      | dx > 0 = replicate dx '>'
      | dx < 0 = replicate (abs dx) '<'
      | otherwise = ""

    intToYKeypresses dy
      | dy > 0 = replicate dy 'v'
      | dy < 0 = replicate (abs dy) '^'
      | otherwise = ""

    numKeypad = M.fromList([ ('A', (2, 3))
                           , ('0', (1, 3))
                           , ('1', (0, 2))
                           , ('2', (1, 2))
                           , ('3', (2, 2))
                           , ('4', (0, 1))
                           , ('5', (1, 1))
                           , ('6', (2, 1))
                           , ('7', (0, 0))
                           , ('8', (1, 0))
                           , ('9', (2, 0))
                           ])

-- All possible sequnces on directional keypad.
dirKeySeq :: Char -> Char -> [String]
dirKeySeq 'A' '<' = ["<v<A", "v<<A"]
dirKeySeq 'A' '>' = ["vA"]
dirKeySeq 'A' '^' = ["<A"]
dirKeySeq 'A' 'v' = ["<vA", "v<A"]
dirKeySeq '^' 'A' = [">A"]
dirKeySeq '^' 'v' = ["vA"]
dirKeySeq '^' '<' = ["v<A"]
dirKeySeq '^' '>' = [">vA", "v>A"]
dirKeySeq 'v' 'A' = ["^>A", ">^A"]
dirKeySeq 'v' '^' = ["^A"]
dirKeySeq 'v' '<' = ["<A"]
dirKeySeq 'v' '>' = [">A"]
dirKeySeq '<' 'A' = [">>^A", ">^>A"]
dirKeySeq '<' '>' = [">>A"]
dirKeySeq '<' '^' = [">^A"]
dirKeySeq '<' 'v' = [">A"]
dirKeySeq '>' 'A' = ["^A"]
dirKeySeq '>' '<' = ["<<A"]
dirKeySeq '>' '^' = ["<^A", "^<A"]
dirKeySeq '>' 'v' = ["<A"]
dirKeySeq from toSame = ["A"]

parse :: String -> [Code]
parse input = map toCode $ lines input
  where
    toCode :: String -> Code
    toCode xs =
      let num = read $ filter (`elem` ['0'..'9']) xs
       in (num, xs)
