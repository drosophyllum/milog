{-# LANGUAGE DeriveDataTypeable #-}

module Grammar where 

import Data.Either
import Data.Generics

infix 6 :-
data Term = Var String Int 
	| Function String [Term] 
	 deriving (Show,Eq,Data,Typeable)
data Clause = Term :- [Term] 
	 deriving (Show,Eq,Data,Typeable) 
data Command = Fact 	{clause::Clause}
	| Query 	{query::[Term]} 
	 deriving (Show,Eq,Data,Typeable)

type Rules = [Clause]
type Substitution = [(Term,Term)]

true = []

