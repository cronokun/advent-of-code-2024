module Day7Spec (spec) where

import Day7
import Test.Hspec
import SpecHelper

example1 :: String
example1 = "190: 10 19\n\
           \3267: 81 40 27\n\
           \83: 17 5\n\
           \156: 15 6\n\
           \7290: 6 8 6 15\n\
           \161011: 16 10 13\n\
           \192: 17 8 14\n\
           \21037: 9 7 18 13\n\
           \292: 11 6 16 20"

input :: String
input = readFile' "inputs/7_calibration_equations"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns total calibration result" $ do
      shouldBe (part1 example1) 3749

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe (part1 input) 12553187650171
