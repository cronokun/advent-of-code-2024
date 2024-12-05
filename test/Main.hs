module Main (main) where

import Day1Test (tests)
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

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then testfail else testpass
