module Day13Spec (spec) where

import Day13
import Test.Hspec hiding (example)

example :: String
example =
  "Button A: X+94, Y+34\n\
  \Button B: X+22, Y+67\n\
  \Prize: X=8400, Y=5400\n\
  \\n\
  \Button A: X+26, Y+66\n\
  \Button B: X+67, Y+21\n\
  \Prize: X=12748, Y=12176\n\
  \\n\
  \Button A: X+17, Y+86\n\
  \Button B: X+84, Y+37\n\
  \Prize: X=7870, Y=6450\n\
  \\n\
  \Button A: X+69, Y+23\n\
  \Button B: X+27, Y+71\n\
  \Prize: X=18641, Y=10279\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns fewest tokens needed to win all possible prizes" $ do
      (part1 example) `shouldBe` 480

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/13_machines"
      (part1 input) `shouldBe` 39748
