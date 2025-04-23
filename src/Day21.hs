-- Day 21: Keypad Conundrum
module Day21 (part1, part2) where

import qualified Data.Map as M
import Helpers (splitAtC)

type Memo = M.Map (Int, String) Int

-- Returns total complexity of all codes.
part1 :: String -> Int
part1 input = solve (2 + 1) input

-- Returns total complexity of all codes.
part2 :: String -> Int
part2 input = solve (25 + 1) input

solve :: Int -> String -> Int
solve n input = fst . foldl calcWithMemo (0, M.empty) . lines $ input
  where
    calcWithMemo = \(total, memo) code ->
      let (result, memo') = calcComplexity memo n code
       in (total + result, memo')

calcComplexity :: Memo -> Int -> String -> (Int, Memo)
calcComplexity memo n code =
  let (len, memo') = seqLenFor memo n code
      num = toNum code
   in (num * len, memo')
  where
    toNum :: String -> Int
    toNum = read . filter (`elem` ['0'..'9'])

seqLenFor :: Memo -> Int -> String -> (Int, Memo)
seqLenFor memo goal code = minLenFor 0 memo code
  where
    -- Returns mininal length for a single A-code.
    minLenFor :: Int -> Memo -> String -> (Int, Memo)
    minLenFor n memo c
      | n + 1 == goal =
          let res = minimum . map length . expandWith expander $ c
           in (res, memo)
      | otherwise =
          let codes = expandWith expander c
           in subcodeLength [] (n + 1) memo codes
      where
        expander
          | n == 0 = numKeySeq
          | otherwise = dirKeySeq

    -- Returns sum of min lengths for subcodes in a A-code.
    subcodeLength :: [Int] -> Int -> Memo -> [String] -> (Int, Memo)
    subcodeLength acc n memo [] = (minimum acc, memo)
    subcodeLength acc n memo (c:cs) =
      let xs = splitAtC 'A' c
          (total, memo') = foldl calcWithMemo (0, memo) xs
       in subcodeLength (total : acc) n memo' cs
      where
        calcWithMemo = \(total, memo) x ->
          let (result, memo') = maybeCalc x memo
           in (total + result, memo')

        maybeCalc :: String -> Memo -> (Int, Memo)
        maybeCalc x m =
          case M.lookup (n, x) m of
            Just len ->
              (len, m)
            Nothing ->
              let (res, m') = minLenFor n m x
                  m'' = M.insert (n, x) res m'
               in (res, m'')

expandWith :: (Char -> Char -> [String]) -> String -> [String]
expandWith f code = run [""] code 'A'
  where
    run acc [] _ = acc
    run acc (x:xs) prev =
      let path = f prev x
          acc' = liftA2 (<>) acc path
       in run acc' xs x

numKeySeq :: Char -> Char -> [String]
numKeySeq '0' '1' = ["^<A"]
numKeySeq '0' '2' = ["^A"]
numKeySeq '0' '3' = [">^A","^>A"]
numKeySeq '0' '4' = ["^^<A"]
numKeySeq '0' '5' = ["^^A"]
numKeySeq '0' '6' = [">^^A","^^>A"]
numKeySeq '0' '7' = ["^^^<A","^^<^A","^<^^A"]
numKeySeq '0' '8' = ["^^^A"]
numKeySeq '0' '9' = [">^^^A","^>^^A","^^>^A","^^^>A"]
numKeySeq '0' 'A' = [">A"]
numKeySeq '1' '0' = [">vA"]
numKeySeq '1' '2' = [">A"]
numKeySeq '1' '3' = [">>A"]
numKeySeq '1' '4' = ["^A"]
numKeySeq '1' '5' = [">^A","^>A"]
numKeySeq '1' '6' = [">>^A","^>>A"]
numKeySeq '1' '7' = ["^^A"]
numKeySeq '1' '8' = [">^^A","^^>A"]
numKeySeq '1' '9' = [">>^^A","^>>^A","^^>>A",">^^>A"]
numKeySeq '1' 'A' = [">>vA"]
numKeySeq '2' '0' = ["vA"]
numKeySeq '2' '1' = ["<A"]
numKeySeq '2' '3' = [">A"]
numKeySeq '2' '4' = ["<^A","^<A"]
numKeySeq '2' '5' = ["^A"]
numKeySeq '2' '6' = [">^A","^>A"]
numKeySeq '2' '7' = ["<^^A","^^<A"]
numKeySeq '2' '8' = ["^^A"]
numKeySeq '2' '9' = [">^^A","^^>A"]
numKeySeq '2' 'A' = [">vA","v>A"]
numKeySeq '3' '0' = ["<vA","v<A"]
numKeySeq '3' '1' = ["<<A"]
numKeySeq '3' '2' = ["<A"]
numKeySeq '3' '4' = ["<<^A","^<<A"]
numKeySeq '3' '5' = ["<^A","^<A"]
numKeySeq '3' '6' = ["^A"]
numKeySeq '3' '7' = ["<<^^A","^<<^A","^^<<A","<^^<A"]
numKeySeq '3' '8' = ["<^^A","^^<A"]
numKeySeq '3' '9' = ["^^A"]
numKeySeq '3' 'A' = ["vA"]
numKeySeq '4' '0' = [">vvA","v>vA"]
numKeySeq '4' '1' = ["vA"]
numKeySeq '4' '2' = [">vA","v>A"]
numKeySeq '4' '3' = [">>vA","v>>A"]
numKeySeq '4' '5' = [">A"]
numKeySeq '4' '6' = [">>A"]
numKeySeq '4' '7' = ["^A"]
numKeySeq '4' '8' = [">^A","^>A"]
numKeySeq '4' '9' = [">>^A","^>>A"]
numKeySeq '4' 'A' = ["v>>vA",">>vvA",">vv>A"]
numKeySeq '5' '0' = ["vvA"]
numKeySeq '5' '1' = ["<vA","v<A"]
numKeySeq '5' '2' = ["vA"]
numKeySeq '5' '3' = [">vA","v>A"]
numKeySeq '5' '4' = ["<A"]
numKeySeq '5' '6' = [">A"]
numKeySeq '5' '7' = ["<^A","^<A"]
numKeySeq '5' '8' = ["^A"]
numKeySeq '5' '9' = [">^A","^>A"]
numKeySeq '5' 'A' = [">vvA","vv>A"]
numKeySeq '6' '0' = ["<vvA","vv<A"]
numKeySeq '6' '1' = ["<<vA","v<<A"]
numKeySeq '6' '2' = ["<vA","v<A"]
numKeySeq '6' '3' = ["vA"]
numKeySeq '6' '4' = ["<<A"]
numKeySeq '6' '5' = ["<A"]
numKeySeq '6' '7' = ["<<^A","^<<A"]
numKeySeq '6' '8' = ["<^A","^<A"]
numKeySeq '6' '9' = ["^A"]
numKeySeq '6' 'A' = ["vvA"]
numKeySeq '7' '0' = [">vvvA","vv>vA","v>vvA"]
numKeySeq '7' '1' = ["vvA"]
numKeySeq '7' '2' = [">vvA","vv>A"]
numKeySeq '7' '3' = [">>vvA","v>>vA","vv>>A",">vv>A"]
numKeySeq '7' '4' = ["vA"]
numKeySeq '7' '5' = [">vA","v>A"]
numKeySeq '7' '6' = [">>vA","v>>A"]
numKeySeq '7' '8' = [">A"]
numKeySeq '7' '9' = [">>A"]
numKeySeq '7' 'A' = ["v>>vvA",">v>vvA","vv>>vA",">vv>vA",">>vvvA","vv>v>A","v>vv>A",">vvv>A"]
numKeySeq '8' '0' = ["vvvA"]
numKeySeq '8' '1' = ["<vvA","vv<A"]
numKeySeq '8' '2' = ["vvA"]
numKeySeq '8' '3' = [">vvA","vv>A"]
numKeySeq '8' '4' = ["<vA","v<A"]
numKeySeq '8' '5' = ["vA"]
numKeySeq '8' '6' = [">vA","v>A"]
numKeySeq '8' '7' = ["<A"]
numKeySeq '8' '9' = [">A"]
numKeySeq '8' 'A' = [">vvvA","v>vvA","vv>vA","vvv>A"]
numKeySeq '9' '0' = ["<vvvA","v<vvA","vv<vA","vvv<A"]
numKeySeq '9' '1' = ["<<vvA","v<<vA","vv<<A","<vv<A"]
numKeySeq '9' '2' = ["<vvA","vv<A"]
numKeySeq '9' '3' = ["vvA"]
numKeySeq '9' '4' = ["<<vA","v<<A"]
numKeySeq '9' '5' = ["<vA","v<A"]
numKeySeq '9' '6' = ["vA"]
numKeySeq '9' '7' = ["<<A"]
numKeySeq '9' '8' = ["<A"]
numKeySeq '9' 'A' = ["vvvA"]
numKeySeq 'A' '0' = ["<A"]
numKeySeq 'A' '1' = ["<^<A","^<<A"]
numKeySeq 'A' '2' = ["<^A","^<A"]
numKeySeq 'A' '3' = ["^A"]
numKeySeq 'A' '4' = ["^<<^A","^^<<A","<^^<A"]
numKeySeq 'A' '5' = ["<^^A","^^<A"]
numKeySeq 'A' '6' = ["^^A"]
numKeySeq 'A' '7' = ["^<<^^A","<^<^^A","^^<<^A","<^^<^A","^^^<<A","^^<^<A","^<^^<A","<^^^<A"]
numKeySeq 'A' '8' = ["<^^^A","^<^^A","^^<^A","^^^<A"]
numKeySeq 'A' '9' = ["^^^A"]
numKeySeq _ _ = ["A"]

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
dirKeySeq _ _ = ["A"]
