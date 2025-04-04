module Day7Spec (spec) where

import Day7
import Test.Hspec

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

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns total calibration result" $ do
      shouldBe (part1 example1) 3749

  describe "part2" $ do
    it "returns total calibration result with additional operation" $ do
      shouldBe (part2 example1) 11387

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/7_calibration_equations"
      shouldBe (part1 input) 12553187650171

    it "for part 2" $ do
      input <- readFile "inputs/7_calibration_equations"
      shouldBe (part2 input) 96779702119491
