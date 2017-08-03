module Deduction where

import Data.List


--------------------------------------------------------------------------------
--- Now the generic deduction stuff ...

-- Takes a list of unary rules, a list of binary rules, and a list of axioms
deduce :: (Eq a) => [a -> [a]] -> [a -> a -> [a]] -> [a] -> [a]
deduce unaryRules binaryRules axioms = fst (loop (oneStep unaryRules binaryRules) ([], axioms))

oneStep :: (Eq a) => [a -> [a]] -> [a -> a -> [a]] -> ([a], [a]) -> ([a], [a])
oneStep _ _ (chart,[]) = (chart,[])
oneStep unaryRules binaryRules (chart,agenda) = (newChart, tail agenda ++ newItems)
        where
                trigger = head agenda
                newChart = nub (trigger:chart)
                newItems = nub (concat ([bothWays r trigger item | r <- binaryRules, item <- chart] ++ [r trigger | r <- unaryRules])) \\ newChart

bothWays :: (a -> a -> [a]) -> a -> a -> [a]
bothWays f x y = f x y ++ f y x

loop :: (Eq a) => (a -> a) -> a -> a
loop f x = if (f x == x) then x else (loop f (f x))


-----------------------------------------
---Propositional logic data type
-----------------------------------------

-- data Form = Atom String
--              | Neg  Form
--              | Impl Form Form
--              | Conj [Form]
--              | Disj [Form]
--              deriving Eq

-- instance Show Form where
--   show (Atom s)   = s 
--   show (Neg form)    = '~' : (show form)
--   show (Impl f1 f2)  = "(" ++ show f1 ++ "->" ++ show f2 ++ ")"
--   show (Conj fs)     = "&" ++ show fs
--   show (Disj fs)     = "V" ++ show fs


-- form1, form2, form3 :: Form
-- form1 = Neg (Neg (Conj [Neg (Atom "p"), (Atom "q")]))
-- form2 = Impl (Atom "q") (Atom "r")
-- form3 = Impl (Neg (Atom "s")) (Atom "p")



