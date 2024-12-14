module Day4Spec (spec) where

import Day4
import Test.Hspec
import SpecHelper

example1 :: String
example1 = "MMMSXXMASM\n\
           \MSAMXMSMSA\n\
           \AMXSXMAAMM\n\
           \MSAMASMSMX\n\
           \XMASAMXAMM\n\
           \XXAMMXXAMA\n\
           \SMSMSASXSS\n\
           \SAXAMASAAA\n\
           \MAMMMXMMMM\n\
           \MXMXAXMASX"

input :: String
input = readFile' "inputs/4_word_search"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns number of times XMAS appear" $ do
      shouldBe 18 (part1 example1)

  describe "part2" $ do
    it "returns number of times X-MAS appear" $ do
      shouldBe  9 (part2 example1)

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe 2434 (part1 input)

    it "for part 2" $ do
      shouldBe 1835 (part2 input)
