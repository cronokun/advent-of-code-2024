module Helpers where

import Debug.Trace (trace)

inspect :: Show a => String -> a -> a
inspect msg res = trace ("\n" ++ msg ++ ": " ++ show res) res
