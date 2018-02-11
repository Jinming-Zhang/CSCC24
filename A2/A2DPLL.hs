module A2DPLL where

-- It's OK to import stuff from the standard library. Put your import lines here.

-- Question 2.

data Atom = Pos String | Neg String deriving Show

-- True if satisfiable, False if unsatisfiable.
cnfSAT :: [[Atom]] -> Bool
cnfSAT = error "TODO"
