module Day8Spec (spec) where

import Day8
import Test.Hspec

example1 :: String
example1 = "............\n\
           \........0...\n\
           \.....0......\n\
           \.......0....\n\
           \....0.......\n\
           \......A.....\n\
           \............\n\
           \............\n\
           \........A...\n\
           \.........A..\n\
           \............\n\
           \............\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns how many uniq locations contain antinodes" $ do
      shouldBe (part1 example1) 14

  describe "part2" $ do
    it "returns how many uniq locations contain harmonic antinodes" $ do
      shouldBe (part2 example1) 34

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/8_antennas_map"
      shouldBe (part1 input) 220

    it "for part 2" $ do
      input <- readFile "inputs/8_antennas_map"
      shouldBe (part2 input) 813
