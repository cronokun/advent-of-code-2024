module Day3Spec (spec) where

import Day3
import Test.Hspec
import SpecHelper

input :: String
input = readFile' "inputs/3_corrupted_memory"

example1 :: String
example1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

example2 :: String
example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns sum of all multiplications" $ do
      shouldBe (part1 example1) 161

  describe "part2" $ do
    it "returns sum of only enabled multiplications" $ do
      shouldBe (part2 example2) 48

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe (part1 input) 170807108

    it "for part 2" $ do
      shouldBe (part2 input) 74838033
