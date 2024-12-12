module SpecHelper where

import System.IO.Unsafe (unsafePerformIO)

readFile' :: String -> String
readFile' = unsafePerformIO . readFile
