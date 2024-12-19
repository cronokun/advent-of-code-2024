-- description: Day 5: Print Queue
module Day5 (part1) where

import qualified Data.Map as Map

parse :: String -> ([(Integer, Integer)], [[Integer]])
parse input = helper 1 [] [] (lines input)
  where
    helper 1 as bs ("":xs) = helper 2 as bs xs
    helper 2 as bs [] = (reverse as, reverse bs)
    helper 1 as bs (x:xs) = helper 1 ((parseRule x):as) bs xs
    helper 2 as bs (x:xs) = helper 2 as ((parsePages x):bs) xs
    parseRule rule = let [a, b] = splitOn '|' rule in (readInt a, readInt b)
    parsePages ns  = map (readInt) $ splitOn ',' ns
    readInt n = read n :: Integer

splitOn :: Char -> String -> [String]
splitOn p str = doSplit [] [] p str
  where
    doSplit as acc _ "" = reverse ((reverse as) : acc)
    doSplit as acc p (x:xs)
      | p == x = doSplit [] ((reverse as) : acc) p xs
      | otherwise = doSplit (x:as) acc p xs


middleElem :: [Integer] -> Integer
middleElem lst =
  let idx = div (length lst) 2
   in lst !! idx

part1 :: String -> Integer
part1 input =
  let (rules, updates) = parse input
      rules' = groupRules rules
   in sum . map middleElem . filter (\xs -> isCorrect rules' xs) $ updates
  where
    groupRules rules = Map.fromListWith (<>) $ map (\(a, b) -> (a, [b])) rules
    isCorrect rules xs =
      let pairs = zip xs (tail xs)
       in all correct pairs
      where
        correct (a, b) =
          let lst1 = Map.findWithDefault [] a rules
              lst2 = Map.findWithDefault [] b rules
           in elem b lst1 && notElem a lst2
