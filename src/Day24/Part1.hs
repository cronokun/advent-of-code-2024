-- Day 24: Crossed Wires
module Day24.Part1 (part1) where

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

getWires :: Device -> String -> [Bool]
getWires d p = M.elems . M.filterWithKey (\k _ -> p `L.isPrefixOf` k) $ (wires d)

toDecimal :: [Bool] -> Int
toDecimal = run 0 0
  where run acc _ [] = acc
        run acc n (True : xs) = run (acc + 2^n) (n + 1) xs
        run acc n (False : xs) = run acc (n + 1) xs

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
