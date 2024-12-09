module Main (main) where

import Day1Test
import Day2Test
import System.Exit qualified as Exit
import Test.HUnit

testpass :: IO ()
testpass = do
  putStrLn "\ESC[32mPASSED\ESC[0m"
  Exit.exitSuccess

testfail :: IO ()
testfail = do
  putStrLn "\ESC[31mFAILED\ESC[0m"
  Exit.exitFailure

tests :: Test
tests =
  TestList
    [ TestLabel "Day 1, part 1 test example" Day1Test.test1
    , TestLabel "Day 1, part 2 test example" Day1Test.test2
    , TestLabel "Day 1, part 1 answer" Day1Test.answer1
    , TestLabel "Day 1, part 2 answer" Day1Test.answer2
    , TestLabel "Day 2, part 1 test example" Day2Test.test1
    , TestLabel "Day 2, part 2 test example" Day2Test.test2
    , TestLabel "Day 2, part 1 answer" Day2Test.answer1
    , TestLabel "Day 2, part 2 answer" Day2Test.answer2
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then testfail else testpass
