module ILP.Milog where 


instance Hypothesis Clause where
	generalize backgroud	= 
	weight 		   	= 
	subsumes (head :- terms) (head' :- terms')	= (name head == name head') ||	(head `imlpiedBy` terms')

impliedBy (Function name' _) terms = everything (||) (mkQ False eqName)
	where 
		eqName x 
			| name x == name'	= True
			| otherwise 		= False

