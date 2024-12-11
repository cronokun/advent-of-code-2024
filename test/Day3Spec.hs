module Day3Spec (spec) where

import Day3
import Test.Hspec
import System.IO.Unsafe (unsafePerformIO)

input :: String
input = unsafePerformIO . readFile $ "inputs/3_corrupted_memory"

sample :: String
sample = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns sum of all multiplications" $ do
      shouldBe 161 (part1 sample)

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe 170807108 (part1 input)
