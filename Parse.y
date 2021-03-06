{
module Parse where
import Lexer
import Grammar 
}

%name prolog
%tokentype {Token}
%error {parseError}
%monad { E } { thenE } { returnE }
%token
	id	{Id _}
	var	{Variable _}
	'('	{Begin}	
	')'	{End}	
	':-'	{Horn}
	'.'	{Period}
	','	{Comma}

%%
Facts : Clause Facts  		{($1) : $2}
	| Clause  		{[$1]}
	
Clause : Term ':-' TermList '.'	{$1 :- $3}
	| Term '.'		{$1 :- []}

Term : id '(' TermList ')' 	{Function (name $1) $3}
	| id 			{Function (name $1) []}
	| var 			{Var (name $1) 0}

TermList : Term ','  TermList 	{$1 : $3}
	|  Term 			{[$1]}

{
data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       	Ok a -> k a
	Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      	Ok a -> Ok a
	Failed e -> k e

parseError tokens = failE "Parse error"
}


