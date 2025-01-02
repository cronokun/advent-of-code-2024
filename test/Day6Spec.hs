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
      shouldBe 41 (part1 example1)

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe 5145 (part1 input)
