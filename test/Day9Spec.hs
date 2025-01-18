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

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/9_disk_map"
      shouldBe (part1 input) 6262891638328
