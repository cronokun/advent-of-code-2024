module Day23Spec (spec) where

import Day23
import Test.Hspec hiding (example)

example :: String
example = "kh-tc\n\
          \qp-kh\n\
          \de-cg\n\
          \ka-co\n\
          \yn-aq\n\
          \qp-ub\n\
          \cg-tb\n\
          \vc-aq\n\
          \tb-ka\n\
          \wh-tc\n\
          \yn-cg\n\
          \kh-ub\n\
          \ta-co\n\
          \de-co\n\
          \tc-td\n\
          \tb-wq\n\
          \wh-td\n\
          \ta-ka\n\
          \td-qp\n\
          \aq-cg\n\
          \wq-ub\n\
          \ub-vc\n\
          \de-ta\n\
          \wq-aq\n\
          \wq-vc\n\
          \wh-yn\n\
          \ka-de\n\
          \kh-ta\n\
          \co-tc\n\
          \wh-qp\n\
          \tb-vc\n\
          \td-yn\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns number of inter-connected computer set that has at least one t-computer" $ do
      part1 example `shouldBe` 7

  describe "part2" $ do
    it "returns password to get into the LAN party" $ do
      part2 example `shouldBe` "co,de,ka,ta"

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/23_local_network_map"
      part1 input `shouldBe` 1269

    it "for part 2" $ do
      input <- readFile "inputs/23_local_network_map"
      part2 input `shouldBe` "ad,jw,kt,kz,mt,nc,nr,sb,so,tg,vs,wh,yh"
