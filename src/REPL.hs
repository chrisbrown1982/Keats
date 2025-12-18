module REPL where

{- 

The implementation is mostly taken and heavily inspired from
https://github.com/ilya-klyuchnikov/lambdapi/blob/master/src/REPL.hs


-}

import Syntax
import TypeChecker
import Grammar
import Pretty
import Text.PrettyPrint.Boxes
import Control.Monad.Except
import System.IO
import System.IO.Error
import Data.Char
import Data.List
import Eval

data Interpreter =
  I {
  		iname   :: String,
  		iprompt :: String

	}


inter :: Interpreter 
inter = I {
    iname   = "Keats: a Simply Typed Lambda Calculus interpreter",
	iprompt = "Keats> "

} 


data Command = TypeOf String 
			|  Quit 
			|  Help
			|  Noop
			|  Load	String	
			|  Derivation String 
			|  Parse String
			|  Eval String 

data InteractiveCommand = Cmd [String] String (String -> Command) String


helpTxt :: [ InteractiveCommand ] -> String
helpTxt cs
	= "List of commands:" ++ 
	  " ... to write \n "

commands :: [ InteractiveCommand ] 
commands 
 = [ Cmd [":t", ":type"]		"<expr>"		TypeOf					"print type of expression", 
     Cmd [":q"] 				""				(const Quit)			"exit interpreter",
     Cmd [":help", ":h", ":?"]	""				(const Help)			"display this list of commands",
     Cmd [":l", ":load"]        "<filename>"	Load					"load file",
     Cmd [":d", ":derivation"]  "<expr>"		Derivation 				"print derivation",
	 Cmd [":p", ":parse"]		"<filename>"    Parse					"parse and print AST",
	 Cmd [":e", ":eval"]		"<expr"			Eval					"evaluate expression"
 ]

repl :: Interpreter -> ([ Decl ], [(String, Derivation)]) -> IO () 
repl int con = 
	let rec int con = 
		do 
			putStr (iprompt int)
			x <- catchIOError (fmap Just getLine) (\_ -> return Nothing)
			case x of 
				Nothing -> return () 
				Just "" -> rec int con 
				Just x  -> 
					do 
						c <- interpretCommand x 
						con' <- executeCommand int con c 
						maybe (return ()) (rec int) con'
	in 
		do 
				putStrLn ((iname int) ++ ".\n" ++  
					"Type :? for help.")
				rec int con 

interpretCommand :: String -> IO Command 
interpretCommand x = do 
	let (cmd, t')    = break isSpace x  
	    t            = dropWhile isSpace t' 
	let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
	case matching of 
		[] -> do 
				 putStrLn ("Unknown command `" ++ cmd ++ "'. Type :? for help.")
				 return Noop     
		[Cmd _ _ f _]
				-> do return (f t)
		x       -> do 
						putStrLn "Ambiguous command."
						return Noop

executeCommand :: Interpreter -> ([ Decl ], [(String, Derivation)]) -> Command -> IO (Maybe ([ Decl ], [(String, Derivation)]))
executeCommand int con cmd = case cmd of 
	Quit 	-> putStrLn "bye!" >> return Nothing 
	Help 	-> putStr (helpTxt commands) >> return (Just con)
	Noop    -> return (Just con)
	Load f  -> do 
				x <- readFile f 
				let mod = parseModule x 
		
				der <- runTcMonad (Context []) (typeCheckM mod)
				case der of 
					Right d -> do 
								putStrLn $ printDerivations d
								return (Just (getDecs mod, d))
					Left (Err e) -> do 
								putStrLn e 
								return (Just con)
	TypeOf d -> do 
					putStrLn (printTypeDef d (snd con)) >> return (Just con)
	Derivation d -> do
				printBox (printDerivationDef d (snd con)) >> return (Just con)
	Parse f -> do 
				x <- readFile f 
				let mod = parseModule x 
				putStrLn (show mod)
				return (Just con)
	Eval e -> do 
				let term = parseTerm e
				-- todo: typecheck the term!!
				t' <- runEvalMonad $ evalTerm (fst con) term 
				case t' of 
					Right t'' -> do 
									putStrLn (show t'' )
									return (Just con)
					Left (Err err) -> error err

