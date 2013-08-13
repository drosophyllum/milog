module Prolog where

import Parse
import Lexer
import Grammar
import Data.Maybe
import System.Environment
import Debug.Trace
import Data.Generics
import Control.Monad
import Data.List(intercalate)
import DLP
import ILP
import Data.List
import System.IO.Strict(readFile)  
import Prelude hiding(readFile)
import System.IO(putStr,hFlush,stdout)


-- -parse:: String -> [Clause]
parse x 
	|(Ok y) <-prolog $ alexScanTokens x = y
	|otherwise = []


main = do 
	[filePath] 	<- getArgs
	interpreter filePath

interpreter filePath = do
	s 		<- readFile filePath
	putStrLn $ "HAL is opening your file: " ++ filePath
	let program =   parse s
	if (program /= []) 
			then putStrLn "HAL understands your file."
			else putStrLn "File contents do not parse."	
	putStrLn "HAL is updating his wisdom." 
	w' 	 	<- readFile "wisdom.HAL"
	let wisdom 	= parse w'
	writeFile "wisdom.HAL"$ concatMap show $ nub (program++wisdom)
	putStrLn "HAL has asimilated the knowledge."
	query     <- var []
	knowledge <- var (program++wisdom) 
	forever $ do
		q <- get query 
		knowledge' <- get knowledge
		if (q /= []) 
			then  think q knowledge'
			else return  []   -- does not do what you think
		putStr ">>" 
		hFlush stdout
		line <-  getLine 
		case line of
			('?':ln)  -> modify query (++ (parse line))
			otherwise -> modify knowledge ( ++ (parse line))

think:: Rules -> [Clause] -> IO([Term])
think question knowledge = do 
		putStrLn "Thinking ...."
		return $ deduct question knowledge
-- search 
--



--		knowldge <- get knowledge
--	let rules = map clause 	$ init program 
--	let goals = query 	$ last program 
--	let found = deduct rules goals
--	putStrLn $"\n\n\n" ++ (concatMap show$ rules)
--	putStrLn $ "\n####DEDUCED#####\n" ++ (intercalate "\n" (map   show  found)) ++ "\n\n\n"
