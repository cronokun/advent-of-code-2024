module Day2Test where

import Day2
import Test.HUnit
import System.IO.Unsafe (unsafePerformIO)

example :: String
example = "7 6 4 2 1\n\
           \1 2 7 8 9\n\
           \9 7 6 2 1\n\
           \1 3 2 4 5\n\
           \8 6 4 4 1\n\
           \1 3 6 7 9"

input :: String
input = unsafePerformIO . readFile $ "inputs/2_unusual_data"

test1 :: Test
test1 = TestCase (assertEqual
                  "Day2.part1 returns number of safe reports"
                  2 (part1 example))


test2 :: Test
test2 = TestCase (assertEqual
                  "Day2.part2 returns number of safe reports with one-off tolerance"
                  4 (part2 example))

answer1 :: Test
answer1 = TestCase (assertEqual "Answer for Day 2 part one" 663 (part1 input))

answer2 :: Test
answer2 = TestCase (assertEqual "Answer for Day 2 part two" 692 (part2 input))
