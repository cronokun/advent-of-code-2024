module Day22Spec (spec) where

import Day22
import Test.Hspec hiding (example)

example1 :: String
example1 = "1\n10\n100\n2024"

example2 :: String
example2 = "1\n2\n3\n2024"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns sum of 2000th secret numbers" $ do
      part1 example1 `shouldBe` 37327623

  describe "part2" $ do
    it "returns maximum number of bananas" $ do
      part2 example2 `shouldBe` 23

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/22_secret_numbers"
      part1 input `shouldBe` 20506453102

    it "for part 2" $ do
      input <- readFile "inputs/22_secret_numbers"
      part2 input `shouldBe` 2423
