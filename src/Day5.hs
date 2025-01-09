-- description: Day 5: Print Queue
module Day5 (part1, part2) where

import qualified Data.Map as Map
import Helpers (splitOn)

parse :: String -> (Map.Map Integer [Integer], [[Integer]])
parse input = helper [] [] (lines input)
  where
    -- parse rules
    helper as bs ("":xs) = helper' as bs xs
    helper as bs (x:xs) = helper ((parseRule x):as) bs xs

    -- parse updates
    helper' as bs [] =
      let rs = map (\(a, b) -> (a, [b])) (reverse as)
          rules = Map.fromListWith (<>) rs
          updates = reverse bs
       in (rules, updates)
    helper' as bs (x:xs) = helper' as ((parsePages x):bs) xs

    parseRule rule = let [a, b] = splitOn '|' rule in (readInt a, readInt b)
    parsePages ns  = map (readInt) $ splitOn ',' ns
    readInt n = read n :: Integer

middleElem :: [Integer] -> Integer
middleElem lst =
  let idx = div (length lst) 2
   in lst !! idx

middleSumOf :: [[Integer]] -> Integer
middleSumOf = sum . map middleElem

isCorrect :: (Map.Map Integer [Integer]) -> [Integer] -> Bool
isCorrect rules xs =
  let pairs = zip xs (tail xs)
   in all (isCorrectPair rules) pairs

isCorrectPair rules (a, b) =
  let lst1 = Map.findWithDefault [] a rules
      lst2 = Map.findWithDefault [] b rules
   in elem b lst1 && notElem a lst2

doCorrect rules xs = fixer [] xs
  where
    fixer acc [] = checkCorrect $ reverse acc
    fixer acc [a] = fixer (a:acc) []
    fixer acc (a:b:rest)
      | isCorrectPair rules (a, b) = fixer (a:acc) (b:rest)
      | otherwise = fixer (b:acc) (a:rest)

    checkCorrect lst =
      case isCorrect rules lst of
        True -> lst
        False -> doCorrect rules lst

part1 :: String -> Integer
part1 input =
  let (rules, updates) = parse input
      correctUpdates = filter (\xs -> isCorrect rules xs) updates
   in middleSumOf correctUpdates 

part2 :: String -> Integer
part2 input =
  let (rules, updates) = parse input
      notCorrectUpdates = filter (\xs -> not $ isCorrect rules xs) updates
      correctedUpdates = map (doCorrect rules) notCorrectUpdates
   in middleSumOf correctedUpdates

