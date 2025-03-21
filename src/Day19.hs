{-# LANGUAGE OverloadedStrings #-}

-- Day 19: Linen Layout
module Day19 where

import Data.Maybe (fromJust, isJust)
import qualified Data.List as L
import qualified Data.Text as T

-- Returns number of possible towel designs.
part1 :: String -> Int
part1 input =
  let (patterns, designs) = parse input
      patterns' = reverse $ L.sortOn T.length patterns 
   in length . filter (isPossible patterns') $ designs

isPossible :: [T.Text] -> T.Text -> Bool
isPossible patterns design = helper [Just design]
  where
    helper xs =
      case stripPatternsFrom xs of
        [] ->
          False
        rs ->
          if any (== Just "") rs then True else helper rs

    stripPatternsFrom xs = L.nub [s | d <- xs, p <- patterns, let s = strip p d, isJust s]
      where
        strip p (Just d) = T.stripPrefix p d

parse :: String -> ([T.Text], [T.Text])
parse input =
  let [ps, ds] = T.splitOn "\n\n" (T.pack input)
      patterns = T.splitOn ", " ps
      designs = T.lines ds
   in (patterns, designs)
