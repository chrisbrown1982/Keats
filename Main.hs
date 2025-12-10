module Main where

import Grammar
import Syntax
import Pretty
import TypeChecker
import Data.Maybe

main :: IO ()
main = putStrLn "Welcome to Keats!"


typeFile :: FilePath -> IO [Either Err Derivation]
typeFile f = 
	do 
		x <- readFile f 
		let mod = parseModule x 
		-- putStrLn $ show mod
		Right der <- runTcMonad [] (typeCheckM mod)
		putStrLn $ printDerivations der
		return [] 


example = "(\\ x . x) : * -> * "

zero = "(\\f . (\\x . x) : * -> *) : * -> *"