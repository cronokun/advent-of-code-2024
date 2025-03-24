module Day19Spec (spec) where

import Day19
import Test.Hspec hiding (example)

example :: String
example = "r, wr, b, g, bwu, rb, gb, br\n\
          \\n\
          \brwrr\n\
          \bggr\n\
          \gbbr\n\
          \rrbgbr\n\
          \ubwu\n\
          \bwurrg\n\
          \brgr\n\
          \bbrgwb\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns number of possible towel designs" $ do
      part1 example `shouldBe` 6

  describe "part2" $ do
    it "returns sum of number of ways you could make each design" $ do
      part2 example `shouldBe` 16

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/19_patterns_and_designs"
      part1 input `shouldBe` 306

    it "for part 2" $ do
      input <- readFile "inputs/19_patterns_and_designs"
      part2 input `shouldBe` 604622004681855
