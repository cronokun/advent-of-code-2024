{-# LANGUAGE RecordWildCards #-}

-- Day 17: Chronospatial Computer
module Day17 (part1) where

import Data.Bits (xor)
import Data.List ((!?), intercalate)

data Computer = Computer
  { regA :: Int
  , regB :: Int
  , regC :: Int
  , output :: [Int]
  , pointer :: Int
  , program :: [Int]
  , halt :: Int
  } deriving (Show)

-- Returns the output debug string.
part1 :: String -> String
part1 input =
  let info = parse input
      comp = buildComputer info
   in formatOutput . output . runProgram $ comp
  where
    formatOutput = intercalate "," . map show . reverse

emptyComputer :: Computer
emptyComputer = Computer { regA = 0
                         , regB = 0
                         , regC = 0
                         , output = []
                         , program = []
                         , pointer = 0
                         , halt = 0
                         }

buildComputer :: (Int, Int, Int, [Int]) -> Computer
buildComputer (a, b, c, prg) =
  emptyComputer { regA = a
                , regB = b
                , regC = c
                , program = prg
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

parse :: String -> (Int, Int, Int, [Int])
parse input =
  let [la, lb, lc, _, p] = lines input
      [a,b,c] = map parseReg [la,lb,lc]
      prog = parseProg p
   in (a, b, c, prog)
  where
    parseReg r = read . last . words $ r :: Int
    parseProg p = read ("[" <> (last . words $ p) <> "]") :: [Int]
