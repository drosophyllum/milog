{
module Lexer where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$upper = [A-Z]

tokens :- 
	$white 		;
	$upper $alpha*  {\s-> Variable s} 
	$alpha+ 	{\s-> Id s}
	"(" 		{\s-> Begin}
	")" 		{\s-> End}
	"."		{\s-> Period}
	":-"		{\s-> Horn}
	"?-"		{\s-> Ask}
	","		{\s-> Comma}
{
data Token = Id {name::String}
	     |Variable {name::String} 
	     |Begin
	     |End
	     |Horn
	     |Period
	     |Comma
	     |Ask
		deriving (Show)
}
