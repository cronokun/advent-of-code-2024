module Day10Spec (spec) where

import Day10
import Test.Hspec hiding (example)

example, example1, example2, example3, example4 :: String

example = "89010123\n\
          \78121874\n\
          \87430965\n\
          \96549874\n\
          \45678903\n\
          \32019012\n\
          \01329801\n\
          \10456732\n"

example1 = "0123\n\
           \1234\n\
           \8765\n\
           \9876\n"

example2 = "...0...\n\
           \...1...\n\
           \...2...\n\
           \6543456\n\
           \7.....7\n\
           \8.....8\n\
           \9.....9\n"

example3 = "..90..9\n\
           \...1.98\n\
           \...2..7\n\
           \6543456\n\
           \765.987\n\
           \876....\n\
           \987....\n"

example4 = "10..9..\n\
           \2...8..\n\
           \3...7..\n\
           \4567654\n\
           \...8..3\n\
           \...9..2\n\
           \.....01\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "Returns sum of trailhead scores" $ do
      (part1 example1) `shouldBe` 1
      (part1 example2) `shouldBe` 2
      (part1 example3) `shouldBe` 4
      (part1 example4) `shouldBe` 3
      (part1 example) `shouldBe` 36

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/10_topographic_map"
      (part1 input) `shouldBe` 0
