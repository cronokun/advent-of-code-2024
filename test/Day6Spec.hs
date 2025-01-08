module Day6Spec (spec) where

import Day6
import Test.Hspec
import SpecHelper

example1 :: String
example1 = "....#.....\n\
           \.........#\n\
           \..........\n\
           \..#.......\n\
           \.......#..\n\
           \..........\n\
           \.#..^.....\n\
           \........#.\n\
           \#.........\n\
           \......#..."

input :: String
input = readFile' "inputs/6_obstacles_map"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns number of disctint positions the guard will visit" $ do
      shouldBe (part1 example1) 41

  describe "part2" $ do
    it "returns how many different positions could you choose to create loops" $ do
      shouldBe (part2 example1) 6

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe (part1 input) 5145
    it "for part 2" $ do
      shouldBe (part2 input) 1523
