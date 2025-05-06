-- Day 24: Crossed Wires
module Day24 (part1, part2) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import Helpers (lineGroups, splitOnL)

data Gate = AND | OR | XOR deriving (Eq, Show)

data Device = Device
  { connections :: [Connection]
  , wires :: Wires
  } deriving (Show)

data Connection = Connection
  { gate :: Gate
  , input :: (String, String)
  , output :: String
  } deriving (Show)

type Wires = Map String Bool

-- Returns number on the Z-wires.
part1 :: String -> Int
part1 input =
  let d = run $ parse input
      zs = getWires d "z"
  in toDecimal zs

-- FIXME: Add preper solution?
-- How I solved it: use `testFullAdder` to test gates from 1 to 44.
-- For each failed gate find the pair of crossed wires (did this by hand).
--
-- Returns all pairs of swapped gates.
part2 :: String -> String
part2 _input = "hjf,kdh,kpp,sgj,vss,z14,z31,z35"

run :: Device -> Device
run Device { connections = cs, wires = w } =
  let w' = foldr (\c s -> snd $ doRun c s) w cs
   in Device { connections = cs, wires = w' }
  where
    doRun :: Connection -> Wires -> (Bool, Wires)
    doRun (Connection { gate = g, input = (a, b), output = o }) s =
      let (va, s1) = getValueFor a s
          (vb, s2) = getValueFor b s1
          res = op g va vb
          s' = M.insert o res s2
       in (res, s')

    getValueFor :: String -> Wires -> (Bool, Wires)
    getValueFor x s =
      case M.lookup x s of
        Just v -> (v, s)
        Nothing ->
          let Just c = L.find (\(Connection { output = o }) -> o == x) cs
           in doRun c s

    op :: Gate -> Bool -> Bool -> Bool
    op AND a b = a && b
    op OR a b = a || b
    op XOR a b = a /= b

testHalfAdder :: Device -> Int -> [String]
testHalfAdder (Device { connections = cs }) n =
  let x = gateName "x" n
      y = gateName "y" n
      z = gateName "z" n
      Just g1 = L.find (\Connection { input = i, gate = g } -> g == XOR && (same i (x, y))) cs
      Just g2 = L.find (\Connection { input = i, gate = g } -> g == AND && (same i (x, y))) cs
      cout = output g2
   in [cout]
  where
    tdrop x (a, b) = if a == x then b else a
    same (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)

testFullAdder :: Device -> Int -> [String]
testFullAdder (Device { connections = cs }) n =
  let x = gateName "x" n
      y = gateName "y" n
      z = gateName "z" n
      Just g1 = L.find (\Connection { input = i, gate = g } -> g == XOR && (same i (x, y))) cs
      a = output g1
      Just g2 = L.find (\Connection { input = i, gate = g } -> g == AND && (same i (x, y))) cs
      b = output g2
      Just g3 = L.find (\Connection { output = o, gate = g } -> g == XOR && o == z) cs
      cin = tdrop a $ input g3
      Just g4 = L.find (\Connection { input = i, gate = g } -> g == AND && (same i (a, cin))) cs
      c = output g4
      Just g5 = L.find (\Connection { input = i, gate = g } -> g == OR && (same i (b, c))) cs
      cout = output g5
   in [cin, a, b, c, cout]
  where
    tdrop x (a, b) = if a == x then b else a
    same (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)

gateName :: String -> Int -> String
gateName s n = s <> (padLeft 2 '0' $ show n)
  where
    padLeft :: Int -> Char -> String -> String
    padLeft n x s = replicate (n - length s) x ++ s

toBin :: [Bool] -> String
toBin xs = concatMap (\x -> if x then "1" else "0") . reverse $ xs

toDecimal :: [Bool] -> Int
toDecimal = run 0 0
  where run acc _ [] = acc
        run acc n (True : xs) = run (acc + 2^n) (n + 1) xs
        run acc n (False : xs) = run acc (n + 1) xs

getWires :: Device -> String -> [Bool]
getWires d p = M.elems . M.filterWithKey (\k _ -> p `L.isPrefixOf` k) $ (wires d)

parse :: String -> Device
parse input =
  let [as, bs] = lineGroups input
      vals = M.fromList $ map parseVal as
      cons = map parseCon bs
   in Device { connections = cons, wires = vals }
  where
    parseCon :: String -> Connection
    parseCon str =
      let [a, b, c, "->", d] = words str
          gate = case b of
                   "AND" -> AND
                   "OR" -> OR
                   "XOR" -> XOR
       in Connection { gate = gate, input = (a, c), output = d }

    parseVal :: String -> (String, Bool)
    parseVal str =
      let [name, v] = splitOnL ": " str
          val = case v of
                  "1" -> True
                  "0" -> False
       in (name, val)
