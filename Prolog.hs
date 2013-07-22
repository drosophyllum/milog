module Prolog where

import Parse
import Lexer
import Grammar
import Data.Maybe
import Data.Char 
import System.Environment
import Debug.Trace
import Data.Generics
import Control.Monad.State
import Control.Monad.Maybe

parse:: String -> [Command]
parse x = prolog $ alexScanTokens x


main = do 
	[filePath] 	<- getArgs
	s 		<- readFile filePath
	let program =  prolog $ alexScanTokens $ s
	let rules = map clause 	$ init program 
	let goals = query 	$ last program 
	let sub   =  prove rules goals
	putStrLn $ show $ sub 
	putStrLn $ show $ map ((flip apply) goals) sub
-- apply everywhere the substitution transform in the subtree
apply :: Substitution -> [Term] -> [Term]
apply s ts = everywhere (mkT (apply' s)) ts

-- replace variable as per the substitution
apply' :: Substitution -> Term -> Term
apply' ((v , v'):s) var 
		| v==var 	= apply' s v'  -- chain
		| otherwise	= apply' s var -- chain
			
apply'  _ x 			= x 
-- explore: is chaining valuable?

--Using the maybe monad.
unify :: Term -> Term -> Maybe Substitution
unify v@(Var _ _) t = Just [(v,t)]
unify t v@(Var _ _) = Just [(v,t)]
unify (Function x xs) (Function y ys) 
	| similar 	= (liftM concat) $ mapM (uncurry unify) (zip xs ys)
	| otherwise	= Nothing
	where similar =  (x==y) && (length xs == length ys)


prove :: Rules -> [Term] -> [Substitution]
prove rules goals = find rules 1 goals

--bfs in the list monad
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
	return 	(s::Substitution, apply s (body ++ goals))

rename:: Rules -> Int -> Rules
rename rules i =  everywhere (mkT renameVar) rules  -- rename vairables everywhere in rules
	where 
		renameVar (Var s _) 		= Var s i	-- Tranform vars
		renameVar x			= x  		-- Do not transform non-vars
