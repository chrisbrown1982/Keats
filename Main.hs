module Main where

import Parsing
import Parser
import Syntax
import Pretty
import TypeChecker
import Data.Maybe

main :: IO ()
main = putStrLn "Welcome to Keats!"

parseTerm :: Monad m => String -> m Term  
parseTerm s = 
           do 
	         let p =  parse parseTerm1 s
	         return (fst $ head p)

typeTerm :: String -> IO (Either Err Derivation)
typeTerm t = do 
				p <- parseTerm t 
				runTcMonad [] (inferType p )
				

typeTerms :: [ Term ] -> IO  [ Either Err Derivation ]
typeTerms [] = return [] 
typeTerms (t : ts) = 
	do 
		ty <- runTcMonad [] (inferType t )
		tys <- typeTerms ts 
		return (ty : tys)


typeFile :: FilePath -> IO [Either Err Derivation]
typeFile f = 
	do 
		x <- readFile f 
		let terms = map (parse parseTerm1) (lines x)
		putStrLn $ show terms
		putStrLn $ show (map (head (map fst terms)))
		putStrLn $ show (map fst (head terms))
	--	tys <- typeTerms (head (map fst terms))
	--	putStrLn $ show tys
		return [] 


example = "(\\ x . x) : * -> * "

zero = "(\\f . (\\x . x) : * -> *) : * -> *"