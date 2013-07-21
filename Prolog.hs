module Prolog where

import Parse
import Lexer
import Grammar
import Data.Maybe
import Data.Char 
import System.Environment
import Debug.Trace
import Data.Generics


parse:: String -> [Command]
parse x = prolog $ alexScanTokens x


main = do 
	[filePath] 	<- getArgs
	s 		<- readFile filePath
	let program =  prolog $ alexScanTokens $ s
	let rules = map clause 	$ init program 
	let goals = query 	$ last program 
	
	putStrLn $show 		$prove rules goals




-- perhaps using generics here is bad?
apply :: Substitution -> [Term] -> [Term]
apply s ts = everywhere (mkT (applySubst s)) ts

applySubst :: Substitution -> Term -> Term
applySubst ((v , v'):s) var 
		| v==var 	= v'
		| otherwise	= var			
applySubst  _ x 		= x 
-- explore: is chaining valuable?


unify :: Term -> Term -> Maybe Substitution 
unify v@(Var _ _) t = Just [(v,t)]
unify t v@(Var _ _) = Just [(v,t)]
unify (Function a as) (Function b bs)
	| a == b 	= unifyList as bs
	| otherwise 	= Nothing

unifyList :: [Term] -> [Term] -> Maybe Substitution
unifyList [] [] = Just true
unifyList (x:xs) (y:ys) = do
	s <- unify x y
	s' <- unifyList (apply s xs) (apply s ys)
	return (s ++ s')
unifyList _ _  = Nothing


prove :: Rules -> [Term] -> [Substitution]
prove rules goals = find rules 1 goals

find :: Rules -> Int -> [Term] -> [Substitution]
find rules i [] = [true]
find rules i goals = do 
	let rules' = rename rules i
	(s, goals')  <- branch rules' goals
	solution <- find rules (i + 1) goals'
	return (s ++ solution)

branch :: Rules -> [Term] -> [(Substitution,[Term])]
branch rules (goal:goals) = do
	head :- body <- rules
	s <- maybeToList $ unify goal head
	return (s, apply s (body ++ goals))

rename:: Rules -> Int -> Rules
rename rules i =  everywhere (mkT renameVar) rules  -- rename vairables everywhere in rules
	where 
		renameVar (Var s _) 		= Var s i	-- Tranform vars
		renameVar x			= x  		-- Do not transform non-vars



