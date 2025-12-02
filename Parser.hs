module Parser where

import Syntax 
import Parsing

parseTerm1 :: Parser Term 
parseTerm1 = do
                t1 <- parseTerm2 
                (parseRest t1) ||| return t1
    where 
       parseRest t1 = do
                ts <- many parseTerm2             
                return (foldl (App) t1 ts)

parseTerm2 :: Parser Term 
parseTerm2 = 
            parseAbs ||| parseVar



parseAbs :: Parser Term 
parseAbs = do 
                symbol "\\"
                var <- identifier 
                symbol "." 
                t  <- parseTerm1 
                symbol ":"
                ty <- parseType 
                return (Abs (Var var) t ty )

parseVar :: Parser Term 
parseVar = do 
                var <- identifier 
                return (VarT (Var var))

parseType :: Parser Type 
parseType = do
                symbol "*" 
                restType ||| return TypeVar 

    where restType =
            do 
                symbol "->"
                t' <- parseType 
                return (Fun TypeVar t' )
                


