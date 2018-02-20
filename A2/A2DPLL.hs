module A2DPLL where

-- It's OK to import stuff from the standard library. Put your import lines here.

-- Question 2.

data Atom = Pos String | Neg String deriving Show

-- True if satisfiable, False if unsatisfiable.
cnfSAT :: [[Atom]] -> Bool
cnfSAT [[]] = False
cnfSAT [[],_] = False
cnfSAT [] = True
-- return the result of try set the first variable to true or false
cnfSAT lst =  (cnfSAT (simplify lst (Pos (getFirst (head (head lst)))))) || (cnfSAT (simplify lst (Neg (getFirst (head (head lst))))))




getFirst :: Atom -> String
getFirst (Pos var) = var
getFirst (Neg var) = var




-- simplifies the givin list of list of Atom according to givin Atom
-- if Atom is of (Pos var), then remove all the [Atom] that contains (Pos var)
--                          and remove all the (Neg var) in each [Atom]
-- if Atom is of (Neg var), then remove all the [Atom] that contains (Neg var)
--                          and remove all the (Pos var) in each [Atom]
simplify :: [[Atom]] -> Atom -> [[Atom]]
-- treat the given var as true
simplify lst (Pos var)
        | length lst == 0 = []
        -- remove all clauses that contains (Pos var)
        | length [ (Pos var1) | (Pos var1) <- (head lst), var1 == var] /= 0 = simplify (tail lst) (Pos var)
        -- for each clause that doesn't contain (Pos var), check if it contains (Neg var)
        -- if yes then remove (Neg var) from the elmt list
        | length [(Neg var2) | (Neg var2) <- (head lst), var2 == var] /= 0 = [removeAtom (head lst) (Neg var)] ++ simplify (tail lst) (Pos var)
        | otherwise = [(head lst)] ++ simplify (tail lst) (Pos var)
-- treat the given var as false
simplify lst (Neg var)
        | length lst == 0 = []
        -- remove all clauses that contains (Neg var)
        | length [ (Neg var1) | (Neg var1) <- (head lst), var1 == var] /= 0 = simplify (tail lst) (Neg var)
        -- for each clause that doesn't contain (Neg var), check if it contains (Pos var)
        -- if yes then remove (Pos var) from the [Atom]
        | length [(Pos var2) | (Pos var2) <- (head lst), var2 == var] /= 0 = [removeAtom (head lst) (Pos var)] ++ simplify (tail lst) (Neg var)
        | otherwise = [(head lst)] ++ simplify (tail lst) (Neg var)



-- removes the givin Atom in the given [Atoms]
-- to remove a (Pos var), then extract every (Neg var), and (Pos var2) which var2 != var 
-- to remove a (Neg var), do the opposite
removeAtom :: [Atom] -> Atom -> [Atom]
removeAtom lst (Neg var) = [(Pos var1) | (Pos var1) <- lst] 
                       ++  [(Neg var2) | (Neg var2) <- lst, var2 /= var]
removeAtom lst (Pos var) = [(Neg var1) | (Neg var1) <- lst] 
                       ++  [(Pos var2) | (Pos var2) <- lst, var2 /= var]                       






--let u = [ [Pos "x", Pos "y", Pos "z"], [Pos "x", Pos "y", Neg "z"], [Pos "x", Neg "y", Pos "z"], [Pos "x", Neg "y", Neg "z"], [Neg "x", Pos "y", Pos "z"], [Neg "x", Pos "y", Neg "z"], [Neg "x", Neg "y", Pos "z"], [Neg "x", Neg "y", Neg "z"]]
--let a = [[Pos "x", Pos "y", Pos "z"], [Pos "x", Pos "y", Neg "z"]]