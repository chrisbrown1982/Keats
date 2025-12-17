module Main where

import REPL


import Data.Maybe

main :: IO ()
main = repl inter [] 




example = "(\\ x . x) : * -> * "

zero = "(\\f . (\\x . x) : * -> *) : * -> *"