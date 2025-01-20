module Day9Spec (spec) where

import Day9
import Test.Hspec

example1 :: String
example1 = "2333133121414131402\n"

spec :: Spec
spec = do
  describe "part1" $ do 
    it "returns compressed filesystem checksum" $ do
      shouldBe (part1 example1) 1928 

  describe "part2" $ do
    it "returns checksum for compressed filesystem v2.0" $ do
      shouldBe (part2 example1) 2858

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/9_disk_map"
      shouldBe (part1 input) 6262891638328

    it "for part 2" $ do
      input <- readFile "inputs/9_disk_map"
      shouldBe (part2 input) 6287317016845
