{-# LANGUAGE DeriveDataTypeable #-}

module Grammar where 

import Data.Either
import Data.Generics
import Data.List

infix 6 :-
data Term = Var String Int 
	| Function String [Term] 
	 deriving (Eq,Data,Typeable)
data Clause = Term :- [Term] 
	 deriving (Eq,Data,Typeable) 
data Command = Fact 	{clause::Clause}
	| Query 	{query::[Term]} 
	 deriving (Show,Eq,Data,Typeable)

type Rules = [Clause]
type Substitution = [(Term,Term)]

true = []

instance Show Term where
	show (Var str _) = str
	show (Function str []) = str
	show (Function str terms) = str ++ "(" ++(intercalate "," $map show terms) ++ ")"

instance Show Clause where
	show (term :- []) = (show term) ++ ".\n"
	show (term :- terms) = (show term) ++ ":-" ++ (intercalate "," $map show terms) ++ ".\n"

