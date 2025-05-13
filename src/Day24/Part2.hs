-- Day 24, part 2: Crossed Wires
module Day24.Part2 where

import qualified Data.List as L
import Data.Maybe (isJust)
import Helpers (lineGroups)

type Op = String
type Wire = String

data Rule = Rule Op Wire Wire Wire deriving (Show)

-- Test typle equivalence: (1, 2) === (2, 1).
infix 4 ===
(===) :: Eq a => (a, a) -> (a, a) -> Bool
(===) (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)

instance Eq Rule where
  (==) (Rule op1 a1 b1 o1) (Rule op2 a2 b2 o2) =
    op1 == op2 && (a1, b1) === (a2, b2) && o1 == o2

-- Match rule by op and (passible swapped)inputs.
infix 4 =~
(=~) :: Rule -> Rule -> Bool
(=~) (Rule op1 a1 b1 _) (Rule op2 a2 b2 _) =
  op1 == op2 && (a1, b1) === (a2, b2)

-- Match rule by op, output and one of inputs.
infix 4 =~=
(=~=) :: Rule -> Rule -> Bool
(=~=) (Rule op1 a1 b1 o1) (Rule op2 a2 b2 o2) =
  op1 == op2 && o1 == o2 && (a1 == a2 || a1 == b2 || b1 == a2 || b1 == b2)

getRuleOut :: Maybe Rule -> Wire
getRuleOut (Just (Rule _ _ _ o)) = o

getRuleInput :: Maybe Rule -> Wire -> Wire
getRuleInput (Just (Rule _ a b _)) c
  | a == c = b
  | b == c = a

findCout :: [Rule] -> Int -> Wire
findCout rules n
  | n == 0 =
    let x = gateName "x" 0
        y = gateName "y" 0
        cin = getRuleOut $ L.find ((Rule "AND" x y "") =~) rules
     in cin
  | otherwise =
    let x = gateName "x" n
        y = gateName "y" n
        r1 = L.find ((Rule "AND" x y "") =~) rules
        o1 = getRuleOut r1
        r2 = L.find (\(Rule op a b _) -> op == "OR" && (a == o1 || b == o1)) rules
        cout = getRuleOut r2
     in cout

ruleToList :: Rule -> [String]
ruleToList (Rule op a b c) =
  let (a', b') = if a <= b then (a, b) else (b, a)
   in [op, a', b', c]

swapRules :: [Rule] -> Wire -> Wire -> [Rule]
swapRules rules x y =
  let (inv, valid) = L.partition (\(Rule _ _ _ o) -> o == x || o == y) rules
      fixed = map fixRule inv
   in valid <> fixed
  where
    fixRule (Rule op a b c)
      | c == x = (Rule op a b y)
      | c == y = (Rule op a b x)

-- Solution:

part2 :: String -> String
part2 input =
  let !(n, rules) = parse input
      res = run [] 1 (n - 1) rules
   in format res
  where
    format = L.intercalate "," . L.nub . L.sort
    run acc n maxN rules
      | n > maxN = acc
      | otherwise =
          case testFullAdder rules n of
            [] -> run acc (n + 1) maxN rules
            [a, b] -> run (a:b:acc) (n + 1) maxN (swapRules rules a b)

testHalfAdder :: [Rule] -> Int -> [Wire]
testHalfAdder rules n =
  let x = gateName "x" n
      y = gateName "y" n
      z = gateName "z" n
      r1 = L.find ((Rule "XOR" x y z) ==) rules
      r2 = L.find ((Rule "AND" x y "") =~) rules
  in if isJust r1 && isJust r2
        then []
        else [getRuleOut r1 , getRuleOut r2]

testFullAdder :: [Rule] -> Int -> [Wire]
testFullAdder rules n = checkRules [] [rule1, rule2]
  where
    checkRules [] [] = []
    checkRules [] (r:rs) = checkRules r rs
    checkRules res _ = res

    rule1 =
      let zr = L.find (\(Rule op a b _) -> op == "XOR" && (a == cin || b == cin)) rules
          zo = getRuleOut zr
      in if zo == z
            then []
            else [z, zo]

    rule2 =
      let r1 = L.find ((Rule "XOR" x y "") =~) rules
          o1 = getRuleOut r1
          rz = L.find (\(Rule op a b o) -> op == "XOR" && o == z && (a == cin || b == cin)) rules
          o2 = getRuleInput rz cin
       in if o1 == o2
             then []
             else [o1, o2]

    x = gateName "x" n
    y = gateName "y" n
    z = gateName "z" n

    cin = findCout rules (n - 1)

-- Helper functions:

gateName :: String -> Int -> String
gateName s n = s <> (padLeft 2 '0' $ show n)
  where
    padLeft :: Int -> Char -> String -> String
    padLeft t x s = replicate (t - length s) x ++ s

parse :: String -> (Int, [Rule])
parse input =
  let [as, bs] = lineGroups input
      n = length as `div` 2
      rules = map toRule bs
   in (n, rules)
  where
    toRule str =
      let [a, op, b, "->", c] = words str
       in Rule op a b c
