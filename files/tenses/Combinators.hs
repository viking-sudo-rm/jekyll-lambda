module Combinators where

type VarName = String
data Term = Var VarName | Lambda VarName Term | Appl Term Term | Forall VarName Term | Exists VarName Term | Impl Term Term | Conj Term Term
            
            --added
            | Equal Term Term | After Term Term | Before Term Term | Throughout Term Term | Dense Term | At Term Term | Subset Term Term | Append Term VarName

     deriving Eq

instance Show Term where
         show (Var n) = n
         show (Lambda n t) = "(\\" ++ n ++ ". " ++ (show t) ++ ")"
         show (Appl t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"
         show (Forall n t) = "A " ++ n ++ " " ++  (show t)
         show (Exists n t) = "E " ++ n ++ " " ++  (show t)        
         show (Impl t1 t2) = "(" ++ (show t1) ++ "=>" ++ (show t2) ++ ")"
         show (Conj t1 t2) = "(" ++ (show t1) ++ " & " ++ (show t2) ++ ")"

         --added
         show (Equal t1 t2) = "(" ++ (show t1) ++ " = " ++ (show t2) ++ ")"
         show (Subset t1 t2) = "(" ++ (show t1) ++ " ~ " ++ (show t2) ++ ")"
         show (Before t1 t2) = "(" ++ (show t1) ++ " < " ++ (show t2) ++ ")"
         show (After t1 t2) = "(" ++ (show t1) ++ " > " ++ (show t2) ++ ")"
         show (Dense t1) = "(D " ++ (show t1) ++ ")"
         show (At t1 t2) = "(" ++ (show t1) ++ " @ " ++ (show t2) ++ ")"
         show (Append t n) = "(" ++ (show t) ++ " ++ " ++ n ++ ")"

zero, one, two :: Term
zero = Lambda "f" $ Lambda "x" $ Var "x"
one  = Lambda "f" $ Lambda "x" $ Appl (Var "f") (Var "x")
two  = Lambda "f" $ Lambda "x" $ Appl (Var "f") (Appl (Var "f") (Var "x"))

next :: Term
next = Lambda "n" $ Lambda "s" $ Lambda "z" $ Appl (Var "s") $ Appl (Appl (Var "n") (Var "s")) (Var "z")

plus :: Term
plus = Lambda "a" $ Lambda "b" $ Appl (Appl (Var "a") next) (Var "b")

eval :: Term -> Term
eval (Var n) = Var n
eval (Lambda v t) = Lambda v (eval t)
eval (Appl (Lambda v t) t') = eval (sub v (eval t') (eval t))
eval (Appl t1 t2) =
     case (eval t1) of
          (Lambda v t) -> eval (sub v (eval t2) t)
          t            -> Appl t (eval t2)
eval (Forall n t) = Forall n (eval t)
eval (Exists n t) = Exists n (eval t)
eval (Impl t1 t2) = Impl (eval t1) (eval t2)
eval (Conj t1 t2) = Conj (eval t1) (eval t2)

--added
eval (Equal t1 t2) = Equal (eval t1) (eval t2)
eval (Subset t1 t2) = Subset (eval t1) (eval t2)
eval (Before t1 t2) = Before (eval t1) (eval t2)
eval (After t1 t2) = After (eval t1) (eval t2)
eval (Dense t1) = Dense (eval t1)
eval (At t1 t2) = At (eval t1) (eval t2)
eval (Append t n) = Append (eval t) n

--turn append statements into new variable names
reduce :: Term -> Term
reduce (Var n) = Var n
reduce (Lambda v t) = Lambda v (reduce t)
reduce (Appl t1 t2) = Appl (reduce t1) (reduce t2)
reduce (Forall n t) = Forall n (reduce t)
reduce (Exists n t) = Exists n (reduce t)
reduce (Impl t1 t2) = Impl (reduce t1) (reduce t2)
reduce (Conj t1 t2) = Conj (reduce t1) (reduce t2)
reduce (Equal t1 t2) = Equal (reduce t1) (reduce t2)
reduce (Subset t1 t2) = Subset (reduce t1) (reduce t2)
reduce (Before t1 t2) = Before (reduce t1) (reduce t2)
reduce (After t1 t2) = After (reduce t1) (reduce t2)
reduce (Dense t1) = Dense (reduce t1)
reduce (At t1 t2) = At (reduce t1) (reduce t2)
reduce (Append (Var s) n) = (Var (s ++ n)) --append a string to a variable name
reduce (Append t n) = reduce (Append (reduce t) n) --CAUTION: this is an infinite loop if you try to reduce an append statement whose term is not a variable



sub :: VarName -> Term -> Term -> Term
sub v t (Var v') = if (v == v') then t else (Var v')
sub v t (Appl t1 t2) = Appl (sub v t t1) (sub v t t2)
sub v t (Impl t1 t2) = Impl (sub v t t1) (sub v t t2)
sub v t (Conj t1 t2) = Conj (sub v t t1) (sub v t t2)

sub v t (Lambda v' t') = if (v == v') then (error "variable collision not implemented") else (Lambda v' (sub v t t'))

sub v t old@(Forall n t') = if (v == n) then old else (Forall n (sub v t t'))
sub v t old@(Exists n t') = if (v == n) then old else (Exists n (sub v t t'))

--added
sub v t (Equal t1 t2) = Equal (sub v t t1) (sub v t t2)
sub v t (Subset t1 t2) = Subset (sub v t t1) (sub v t t2)
sub v t (Before t1 t2) = Before (sub v t t1) (sub v t t2)
sub v t (After t1 t2) = After (sub v t t1) (sub v t t2)
sub v t (Dense t1) = Dense (sub v t t1)
sub v t (At t1 t2) = At (sub v t t1) (sub v t t2)
sub v t (Append t1 n) = Append (sub v t t1) n

b    = Lambda "f" $ Lambda "g" $ Lambda "w" $ Appl (Var "f") (Appl (Var "g") (Var "w"))
t    = Lambda "w" $ Lambda "f" $ (Appl (Var "f") (Var "w"))
z    = Lambda "f" $ Lambda "g" $ Lambda "x" $ (Appl (Appl (Var "f") (Appl (Var "g") (Var "x"))) (Var "x"))
s    = Lambda "f" $ Lambda "g" $ Lambda "x" $ (Appl (Appl (Var "f") (Var "x")) (Appl (Var "g") (Var "x")))
k    = Lambda "x" $ Lambda "y" $ (Var "x")
i    = Lambda "x" $ (Var "x")

