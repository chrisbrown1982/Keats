{
module AlexToken (Token(..), scanTokens) where
import Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
	$eol							;
	$white+							;
	"--".*							;
	let 							{ \s -> TokenLet }
	assume							{ \s -> TokenAssume }
	type 							{ \s -> TokenType }
	module							{ \s -> TokenModule }
	where  							{ \s -> TokenWhere }
	$digit+							{ \s -> TokenNum (read s) }
	\.								{ \s -> TokenDot }
	\=								{ \s -> TokenEq }
	\\								{ \s -> TokenLambda }
	\(								{ \s -> TokenLParen }
	\)								{ \s -> TokenRParen }
	"->"							{ \s -> TokenRArrow }
	\*								{ \s -> TokenStar }
	$alpha [$alpha $digit \_ \']*	{ \s -> TokenSym s}
	\:								{ \s -> TokenColon }


{
data Token 	=	TokenLet 
				|	TokenNum Int 
				|	TokenDot
				|	TokenEq
				|	TokenLambda
				|	TokenLParen
				|	TokenRParen
				|	TokenSym String
				|	TokenRArrow
				|	TokenStar
				|	TokenColon
				|	TokenAssume
				|   TokenWhere 
				|   TokenModule
				|   TokenType
				deriving (Eq, Show)

scanTokens = alexScanTokens
}