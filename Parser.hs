module Parser where

import Syntax 
import Parsing

parseTerm :: Parser Term 
parseTerm = 
            parseVar ||| parseAbs ||| parseApp

parseApp :: Parser Term 
parseApp = do
                t1 <- parseTerm 
                t2 <- parseTerm 
                return (App t1 t2)

parseAbs :: Parser Term 
parseAbs = do 
                char '\\'
                var <- identifier 
                char '.' 
                t  <- parseTerm 
                char ':'
                ty <- parseType 
                return (Abs (Var var) t ty )

parseVar :: Parser Term 
parseVar = do 
                var <- identifier 
                return (VarT (Var var))

parseType :: Parser Type 
parseType = parseFunType ||| parseKind

parseKind :: Parser Type 
parseKind = do
                symbol "*" 
                return TypeVar 

parseFunType :: Parser Type 
parseFunType = do
                    ty1 <- parseType 
                    symbol "->"
                    ty2 <- parseType 
                    return (Fun ty1 ty2)