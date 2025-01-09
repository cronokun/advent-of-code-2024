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
      shouldBe (part1 example1) 143

  describe "part2" $ do
    it "returns sum of middle numbers of corrected updates" $ do
      shouldBe (part2 example1) 123

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe (part1 input) 5108

    it "for part 2" $ do
      shouldBe (part2 input) 7380
