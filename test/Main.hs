module Main (main) where

import Day1Test (tests)
import Day2Test (tests)
import Test.HUnit
import qualified System.Exit as Exit

testpass :: IO ()
testpass = do
  putStrLn "\ESC[32mPASSED\ESC[0m"
  Exit.exitSuccess

testfail :: IO ()
testfail = do
  putStrLn "\ESC[31mFAILED\ESC[0m"
  Exit.exitFailure

allTests = TestList [Day1Test.tests, Day2Test.tests]

main :: IO ()
main = do
  result <- runTestTT allTests
  if failures result > 0 then testfail else testpass
