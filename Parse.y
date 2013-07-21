{
module Parse where
import Lexer
import Grammar 
}

%name prolog
%tokentype {Token}
%error {parseError}

%token
	id	{Id _}
	var	{Variable _}
	'('	{Begin}	
	')'	{End}	
	':-'	{Horn}
	'?-'	{Ask}
	'.'	{Period}
	','	{Comma}

%%
Commands : Facts		{$1}
	| Facts Queries 	{$1 ++ [$2]}
Facts : Clause Facts  		{(Fact $1) : $2}
	| Clause  		{[Fact $1]}
	
Queries : Query Queries		{Query $ ((\(Query qs)->qs) $1) ++ ((\(Query qs)-> qs) $2)}
	| Query			{$1}
Query : '?-' Term '.'		{Query [$2]}	

Clause : Term ':-' TermList '.'	{$1 :- $3}
	| Term '.'		{$1 :- []}

Term : id '(' TermList ')' 	{Function (name $1) $3}
	| id 			{Function (name $1) []}
	| var 			{Var (name $1) 0}

TermList : Term ','  TermList 	{$1 : $3}
	|  Term 			{[$1]}

{
parseError :: [Token] -> a
parseError _ = error "I can haz purrrs error, I.D.K wurrr"
}


