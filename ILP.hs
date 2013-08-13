{-# LANGUAGE DataKinds #-}

module ILP where 
import Control.Monad 
import Control.Monad.Loops
import Data.List
import System.Random
import Data.Random
import Data.Random.Shuffle.Weighted
import Data.IORef
import Data.List

--class Hypothesis h where
--neralize :: [String] -> [String]
-- todo : !!!! FIX
--
--
---
main = do
	induct [["Red","Ball"],["Red","Snake"], ["Square","Tooth"],["Square","Socks"],["Red","Flower"]] [[[]]]



generalize :: [[[String]]] ->[String] ->[[[String]]]
generalize hps ex = sanitize $ do
			properties <- subsequences ex
			h  <- hps'
			junc <- [ properties:h]
			[junc ++ [properties]]
	where 	
		hps' = map (map nonEmpties) hps

nonEmpties xs = nub $ (filter (not.null) xs )
sanitize = nonEmpties.(map (nonEmpties.(map (nonEmpties))))

subsumes :: [String]->[String]->Bool
subsumes  = mySubset   

mySubset :: (Eq a) => [a] -> [a] -> Bool 
mySubset xs ys = all (`elem` ys) xs -- && (xs /= ys)

evalH:: [[String]] -> [String] -> Bool
evalH h ex =  or [evalC c ex | c <- h]
		where evalC c ex = mySubset c ex 

weight:: [[String]] -> [[String]] -> Double
weight exs h = case h of
		[[]] -> 0
		x    -> exp $ (likelihood h) / (0.5  * prior h)
		where 	prior h = fromIntegral $ (length (concat h))
		    	likelihood h = fromIntegral $ length (filter id [evalH h ex | ex <- exs])


--collapse :: (Eq a) => [[a]] -> [[a]] 
prune:: [[[String]]] -> IO [[[String]]] 
prune h = return $ nubBy (mySubset) $ hSortedByLength
       where 	f x y = compare (length x) (length y)
		hSortedByLength:: [[[String]]] 
		hSortedByLength = (sortBy f h)

induct examplar hyp = do
	stdRand <- getStdGen                    -- Get system-dependant random source
	randomSource  <- var stdRand            -- put it in a variable
	randomlySample <- return (\n xs -> runRVar (weightedSample n xs) randomSource)
	hypotheses  <- var   hyp 
	example <- var examplar
	background <- var []
	whileM_ (moreMoreExamples example) $ do

		examples <- get example
		ex <- liftM head $ randomlySample 1 (zip ([1,1..]::[Double]) examples)
		modify background (++[ex])

		putStrLn $ "\nEXAMPLE: " ++ show ex 

		modify example (\\[ex])

		backgrd <- get background
		hypotheses' <- (get hypotheses)
		let w  =(hypotheses' `weightedBy` (weight backgrd))
		putStrLn $ "HYPOTHESES BEFORE: "  ++ (show w)

		chosenHypotheses <- (runRVar (weightedSample 10 w) randomSource ) 
		newHypotheses <- expand chosenHypotheses ex 
		prunedHypotheses <- prune newHypotheses
		let finale  = sortBy (\(w,_) (w',_) -> compare w' w)  $ normalize (((hypotheses' ++ prunedHypotheses ) ) `weightedBy` (weight examplar))

		putStrLn $ "HYPOTHESES AFTER: "  ++ (show finale)
		set hypotheses (sanitize (hypotheses' ++ prunedHypotheses ) )
		
	--hypopotomus <- get hypotheses
	--putStrLn $ "\n" ++( show  (normalize ( nub hypopotomus)))  --`weightedBy` ( weight examplar)

normalize:: [(Double,x)] -> [(Double, x)]
normalize xs = map (\(w,s)-> ((2**w / total),s)) xs 
		where total = sum $ map (\(w,x)-> 2**w) xs 
		
moreMoreExamples vr  = do
		hslst <- get vr
		return $ not $ null hslst



------ Utility-------------
--expand :: Hypothesis h => [h] -> IO  [h]
expand:: [[[String]]] -> [String] -> IO [[[String]]]
expand hs ex 
	| [] <-hs    = expand [[[]]] ex
	| otherwise =  do 
		out <- return $ concatMap (\h->generalize [h] ex) hs
		putStrLn "###################################"
		putStrLn $ show hs
		putStrLn $ show ex
		putStrLn $ show out
		putStrLn "###################################"
		return out
		
--prune :: Hypothesis h => [h] -> IO  [h]
--prune hs = return $ collapseBy subsumes hs

weightedBy ::  [[[String]]] -> ([[String]]->Double) -> [(Double,[[String]])]
weightedBy hypotheses heuristic = map (valueAndResultOf (heuristic)) hypotheses
	where 
		valueAndResultOf heuristic el = ((heuristic el),el)



collapseBy subsum hs = nubBy (\x y -> subsumes x y == True ) $ sortBy (\x y -> if (subsum x y) then LT else GT ) hs

{-
initRandom :: IO (Int -> [(Double, a)] -> IO [a])  
initRandom = do 
	stdRand <- getStdGen                    -- Get system-dependant random source
	randomSource  <- var stdRand            -- put it in a variable
	return (\n xs -> runRVar (weightedSample n xs) randomSource)
-}

pop lstRef = do
		l:ls <- get lstRef
		set lstRef ls
		return l


----Variables ----------
var = newIORef
set = writeIORef
modify = modifyIORef
get = readIORef
------------------------


{--




--- Allergic to do-notation? this will not serve you well.
train :: State LoopSt ()
train = state (\s -> (s,s))
		>>= (\hSt@(h, counter) ->
				if counter < 5
					then state (\_ -> ( (), (update h, counter + 1)  )) >> train 
					else 
						    state (\_ -> ((), (h, counter))) )


train2 :: State LoopSt ()
-- doesn't work with do notation. why?
train2 = do 
			(hs, counter) <- get
			if counter < 5 
				then do put (update hs, counter + 1)
						train2
				else put (hs, counter)
			


update :: [Hypothesis] -> [Hypothesis]
update hs = let 
		posEx = [(["bl", "sq"], True), (["bl", "tr"], True)]
	    	negEx = [(["re", "sq"], False), (["re", "tr"], False)]
	        allEx = posEx ++ negEx
	        ex = pick posEx
	        newHs = collapse (append hs (generalize ex))
	            -- also: delete duplicates. should handle permutations correctly. 
	        in 
	 		resample newHs (getWeights newHs allEx)



pick :: [a] -> a
--TODO: pick randomly from list
pick l = init l

resample :: Fractional w => [a] -> [w] -> [Int] -> [a]
--TODO: make this function. the Int is how many samples to draw. 

getWeights :: [Hypothesis] -> [Example] -> [()]
getWeights hs exs = [(prior h) * (likelihood h) | h <- hs]
				where 
					prior h = length (concat h)
					likelihood h = length (filter (== True) [evalH ex | ex <- exs])

generalize :: Hypothesis -> Example ->  [Hypothesis]
--TODO: make this work for arbitrary collections of attributes
generalize ex@([color, shape], _) = [ [color], [shape], [color, shape], [[color], [shape]] ] 

append :: [Hypothesis] -> [Hypothesis] -> [Hypothesis]
append hsOld hsNew = hsOld ++ [h ++ hNew | h <- hs, hNew <-hsNew]

mySubset :: (Eq a) => [a] -> [a] -> Bool 
mySubset ys xs = all (`elem` ys) xs && (xs /= ys)

collapse :: (Eq a) => [[a]] -> [[a]] 
collapse h = nubBy (mySubset) (sortBy f h)
       where f x y = compare (length x) (length y)

evalH :: Hypothesis -> Example -> Int 
evalH h ex =  any [evalC c ex | c <- h]
				where evalC c ex = mySubset c ex 
  




--}
