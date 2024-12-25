module Day5Spec (spec) where

import Day5
import Test.Hspec
import SpecHelper

example1 :: String
example1 = "47|53\n\
           \97|13\n\
           \97|61\n\
           \97|47\n\
           \75|29\n\
           \61|13\n\
           \75|53\n\
           \29|13\n\
           \97|29\n\
           \53|29\n\
           \61|53\n\
           \97|53\n\
           \61|29\n\
           \47|13\n\
           \75|47\n\
           \97|75\n\
           \47|61\n\
           \75|61\n\
           \47|29\n\
           \75|13\n\
           \53|13\n\
           \\n\
           \75,47,61,53,29\n\
           \97,61,53,29,13\n\
           \75,29,13\n\
           \75,97,47,61,53\n\
           \61,13,29\n\
           \97,13,75,29,47"

input :: String
input = readFile' "inputs/5_rules_and_pages"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns sum of middle numbers of correctly-ordered updates" $ do
      shouldBe 143 (part1 example1)

  describe "part2" $ do
    it "returns sum of middle numbers of corrected updates" $ do
      shouldBe 123 (part2 example1)

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe 5108 (part1 input)

    it "for part 2" $ do
      shouldBe 7380 (part2 input)
