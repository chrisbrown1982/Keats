{ 
module Grammar where

import AlexToken
import Syntax
}

-- name of parser 
%name keats
%tokentype { Token }
%error { parseError }

%token
    let                             {TokenLet}
    assume                          {TokenAssume}
    type                            {TokenType}
    module                          {TokenModule}
    where                           {TokenWhere}
    '='                             {TokenEq}
    VAR                             {TokenSym $$}
    '\\'                            {TokenLambda}
    '.'                             {TokenDot}
    '('                             {TokenLParen}
    ')'                             {TokenRParen}
    '->'                            {TokenRArrow}
    '*'                             {TokenStar}   
    ':'                             {TokenColon} 


%right '->'
%%

Module : module VAR where Decls             { Module $2 $4 }

Decls : Decl                                { [ $1 ]}
     | Decl Decls                           { $1 : $2 }

Decl : let VAR '=' Term                     { Decl $2 $4}
     | assume '(' VAR ':' Type ')'          { Assume (varN $3) $5 }
     | type '(' VAR ':' Kind ')'            { Type (varN $3) $5   }

Term : Term ':' Type                        { Ann $1 $3}
     | '\\' VAR '.' Term                    { lam $2 $4}
     | Tuple                                { $1 }


Tuple : Tuple Single                        { App $1 $2}
     | Single                               { $1 }

Single : '(' Term ')'                       { $2}
     | VAR                                  { var $1}

Type : Type '->' Type                       { Fun $1 $3 }
     | VAR                                  { TypeVar (varN $1) }
     | '(' Type ')'                         { $2 }


Kind : '*'                                  { Star }


{
parseError :: [Token] -> a
parseError t = error ("Parse error " ++ (show t))

parseModule :: String -> Module
parseModule = keats . scanTokens
}