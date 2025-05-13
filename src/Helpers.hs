module Helpers ( charToInt
               , inspect
               , lineGroups
               , replace
               , splitAtC
               , splitOn
               , splitOnL
               ) where

import Debug.Trace (trace)
import qualified Data.List as L

inspect :: Show a => String -> a -> a
inspect msg res = trace (msg ++ ": " ++ show res) res

splitAtC :: Char -> String -> [String]
splitAtC p str = run [] "" str
  where
    run acc "" [] = reverse acc
    run acc wacc [] = reverse (wacc : acc)
    run acc wacc (x:xs)
      | x == p = run ((wacc <> [p]) : acc) [] xs
      | otherwise = run acc (wacc <> [x]) xs

-- Split a string into two on the specified char.
splitOn :: Char -> String -> [String]
splitOn p str = doSplit [] [] str
  where
    doSplit as acc "" = reverse ((reverse as) : acc)
    doSplit as acc (x:xs)
      | p == x = doSplit [] ((reverse as) : acc) xs
      | otherwise = doSplit (x:as) acc xs

splitOnL :: String -> String -> [String]
splitOnL pattern str = helper [] [] str
  where
    helper acc as [] = reverse (reverse as : acc)
    helper acc as s@(x : xs) =
      case L.stripPrefix pattern s of
        Just xs' -> helper ((reverse as) : acc) [] xs'
        Nothing -> helper acc (x : as) xs

replace :: String -> String -> String -> String
replace p r str = run [] str
  where run acc [] = acc
        run acc xs
          | p `L.isPrefixOf` str = run (acc <> r) (drop (length p) xs)
          | otherwise = run (acc <> take 1 xs) (drop 1 xs)

-- Split a string into groups of strings.
lineGroups :: String -> [[String]]
lineGroups str = groupsFrom [] [] $ lines str
  where
    groupsFrom as acc [] = reverse ((reverse as) : acc)
    groupsFrom as acc (x:xs)
      | x == "" = groupsFrom [] ((reverse as) : acc) xs
      | otherwise = groupsFrom (x:as) acc xs

-- Parse a single digit.
charToInt :: Char -> Int
charToInt c =
  case c of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
