module Day01Test where

import Day01
import Test.HUnit
import System.IO.Unsafe

input1 :: String
input1 = "3   4\n\
         \4   3\n\
         \2   5\n\
         \1   3\n\
         \3   9\n\
         \3   3"

input2 :: String
input2 = unsafePerformIO . readFile $ "inputs/01_locations_list"

test1 :: Test
test1 = TestCase (assertEqual "Returns total distance between lists" 11 (part1 input1))

answer1 :: Test
answer1 = TestCase (assertEqual "Answer part one" 2192892 (part1 input2))

tests :: Test
tests = TestList [TestLabel "Day 1, part 1: example" test1,
                  TestLabel "Day 1, part 1: answer" answer1]
