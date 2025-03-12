{-# LANGUAGE RecordWildCards #-}

-- Day 17: Chronospatial Computer
module Day17 (part1, part2) where

import Data.Bits (xor)
import Data.List ((!?), intercalate, nub)

---- Computer implementation

data Computer = Computer
  { regA :: Int
  , regB :: Int
  , regC :: Int
  , output :: [Int]
  , pointer :: Int
  , program :: [Int]
  , halt :: Int
  } deriving (Show)

buildComputer :: (Int, Int, Int, [Int]) -> Computer
buildComputer (a, b, c, prg) =
  Computer { regA = a
           , regB = b
           , regC = c
           , output = []
           , program = prg
           , pointer = 0
           , halt = length prg
           }

runProgram :: Computer -> Computer
runProgram comp@Computer{..} =
  case runOp of
    Just comp' -> runProgram comp'
    Nothing -> comp
  where
    runOp =
      let opcode = program !? pointer
          operand = program !? (pointer + 1)
       in case (opcode, operand) of
              (Just 0, Just _) -> Just adv
              (Just 1, Just _) -> Just bxl
              (Just 2, Just _) -> Just bst
              (Just 3, Just _) -> Just jnz
              (Just 4, Just _) -> Just bxc
              (Just 5, Just _) -> Just out
              (Just 6, Just _) -> Just bdv
              (Just 7, Just _) -> Just cdv
              _otherwise       -> Nothing
        
    adv =
      let res = regA `div` 2 ^ combop 
       in comp { regA = res, pointer = pointer' }

    bdv =
      let res = regA `div` 2 ^ combop 
       in comp { regB = res, pointer = pointer' }

    cdv =
      let res = regA `div` 2 ^ combop 
       in comp { regC = res, pointer = pointer' }

    bst =
      let res = combop `mod` 8
       in comp { regB = res, pointer = pointer' }

    bxc =
      let res = regB `xor` regC
       in comp { regB = res, pointer = pointer' }

    bxl =
      let res = regB `xor` litop
       in comp { regB = res, pointer = pointer' }

    jnz =
      let p' = if regA == 0 then pointer + 2 else litop
       in comp { pointer = p' }

    out =
      let res = combop `mod` 8
          out' = (res : output)
       in comp { output = out', pointer = pointer' }

    pointer' = pointer + 2

    combop =
      case litop of
        0 -> 0
        1 -> 1
        2 -> 2
        3 -> 3
        4 -> regA
        5 -> regB
        6 -> regC

    litop = program !! (pointer + 1)

---- Solutions

-- Returns the output debug string.
part1 :: String -> String
part1 input =
  let info = parse input
      comp = buildComputer info
   in formatOutput . output . runProgram $ comp
  where
    formatOutput = intercalate "," . map show . reverse

-- Returns lowest positive initial value for register A that make program to output a copy of itself.
part2 :: String -> Int
part2 input =
  let info = parse input
      comp = buildComputer info
      candidates = narrowdown 0 (length $ program comp) [] comp
   in minimum candidates

narrowdown :: Int -> Int -> [[Int]] -> Computer -> [Int]
narrowdown cur len condidates comp
  | len == cur = bitsToNums condidates
  | otherwise =
  let expected = drop (len - cur - 1) $ program comp
      ns = filter (\x -> getOutputFrom x  == expected) $ expand condidates
      condidates' = unify ns
   in narrowdown (cur + 1) len condidates' comp
  where
    getOutputFrom :: Int -> [Int]
    getOutputFrom a = reverse . output . runProgram $ comp { regA = a }

    expand :: [[Int]] -> [Int]
    expand xs = bitsToNums (xs <> [[0..7]])

    unify :: [Int] -> [[Int]]
    unify [0,1,2,3,4,5,6,7] = [[0..7]]
    unify xs = reduce [] . map intToOct $ xs
      where
        takeHeads = foldr (\(a:b) (as,bs)  -> ((a:as), (b:bs))) ([],[])

        reduce acc ns
          | all null ns = reverse acc
          | otherwise =
            let (hs, ns') = takeHeads ns
             in reduce (nub hs : acc) ns'

    bitsToNums :: [[Int]] -> [Int]
    bitsToNums xs = helper (zip [0..] (reverse xs)) [0]
      where
        applicativeSum :: [Int] -> [Int] -> [Int]
        applicativeSum as bs = (map (+) bs) <*> as

        helper :: [(Int, [Int])] -> [Int] -> [Int]
        helper [] acc = acc
        helper ((p, ns):rest) acc =
          let ns' = map (\n -> (8 ^ p) * n) ns
              acc' = applicativeSum acc ns'
           in helper rest acc'

    intToOct :: Int -> [Int]
    intToOct n = helper n []
      where
        helper x acc
          | x == 0 = acc
          | otherwise = helper (x `div` 8) ((x `rem` 8) : acc)

parse :: String -> (Int, Int, Int, [Int])
parse input =
  let [la, lb, lc, _, p] = lines input
      [a,b,c] = map parseReg [la,lb,lc]
      prog = parseProg p
   in (a, b, c, prog)
  where
    parseReg r = read . last . words $ r :: Int
    parseProg p = read ("[" <> (last . words $ p) <> "]") :: [Int]
