module Day1Test where

import Day1
import Test.HUnit
import System.IO.Unsafe (unsafePerformIO)

example :: String
example = "3   4\n\
           \4   3\n\
           \2   5\n\
           \1   3\n\
           \3   9\n\
           \3   3"

input :: String
input = unsafePerformIO . readFile $ "inputs/1_locations_list"

test1 :: Test
test1 = TestCase (assertEqual
                  "Day1.part1 returns total distance between lists"
                   11 (part1 example))

test2 :: Test
test2 = TestCase (assertEqual
                  "Day1.part2 returns lists similarity score"
                  31 (part2 example))

answer1 :: Test
answer1 = TestCase (assertEqual "Answer for Day 1 part one" 2192892 (part1 input))

answer2 :: Test
answer2 = TestCase (assertEqual "Answer for Day 1 part two" 22962826 (part2 input))

tests :: Test
tests = TestList [TestLabel "Day 1, part 1 test example" test1,
                  TestLabel "Day 1, part 2 test example" test2,
                  TestLabel "Day 1, part 1 answer" answer1,
                  TestLabel "Day 1, part 2 answer" answer2]
