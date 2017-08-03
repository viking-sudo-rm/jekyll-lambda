module CCGSem where

import Combinators
import Deduction

--added new SynTypes
data SynType = NP | S | N | Sup | Inf | Adj | SynType :/ SynType |  SynType :\ SynType
     deriving (Eq)

instance Show SynType where
         show NP = "NP"
         show S = "S"
         show N = "N"
         show Sup = "Sup"
         show Inf = "Inf"
         show Adj = "Adj"
         show (a :/ b) = "(" ++ (show a) ++ "/" ++ (show b) ++ ")"
         show (a :\ b) = "(" ++ (show a) ++ "\\" ++ (show b) ++ ")"
     

data CCGItem = MkItem SynType Term Int Int
     deriving Eq

--added this method to access the meaning of parse items
getMeaning :: CCGItem -> Term
getMeaning (MkItem _ t _ _) = eval t



type Chart = [CCGItem]

instance Show CCGItem where
         show (MkItem c t i j) = "[" ++ (show c) ++ ", " ++ (show i) ++ ", " ++ (show j) ++  ", " ++ (show t) ++ "]"

data LexEntry = Lex String SynType Term
     deriving (Eq, Show)

type Lexicon = [LexEntry]


forwardApp :: CCGItem -> CCGItem -> [CCGItem]
forwardApp (MkItem (a :/ b) t1 i j) (MkItem c t2 k l)
           | b == c && j == k    = [MkItem a (eval (Appl t1 t2)) i l]
           | otherwise           = []
forwardApp _ _ = []

backwardApp :: CCGItem -> CCGItem -> [CCGItem]
backwardApp (MkItem c t2 i j) (MkItem (a :\ b) t1 k l)
            | b == c && j == k   = [MkItem a (eval (Appl t1 t2)) i l]
            | otherwise          = []
backwardApp _ _ = []

forwardComp :: CCGItem -> CCGItem -> [CCGItem]
forwardComp  (MkItem (u :/ v) t1 i j) (MkItem (w :/ x) t2 k l)
             | v == w && j == k   = [MkItem (u :/ x) (eval (Appl (Appl b t1) t2)) i l]
             | otherwise         = []
forwardComp _ _ = []

backwardComp :: CCGItem -> CCGItem -> [CCGItem]
backwardComp  (MkItem (u :\ v) t2 i j) (MkItem (w :\ x) t1 k l)
              | x == u && j == k    = [MkItem (w :\ v) (eval (Appl (Appl b t1) t2)) i l]
              | otherwise          = []
backwardComp _ _ = []

typeRaise :: CCGItem -> [CCGItem]
typeRaise (MkItem NP t1 i j) = [MkItem (S :/ (S :\ NP)) (eval (Appl t t1)) i j,
                                MkItem (S :\ (S :/ NP)) (eval (Appl t t1)) i j]

typeRaise _ = []

getAxioms :: Lexicon -> [String] -> [CCGItem]
getAxioms lex words = [MkItem cat term i (i+1) | (Lex word cat term) <- lex,
                                         i <- [0 .. (length words -1)],
                                         (words !! i) == word]

parse :: Lexicon -> String -> Chart
parse l sent = filter (longitem (length (words sent))) allItems
      where
--        allItems = deduce [] [forwardApp,backwardApp] (getAxioms l (words sent))
--        allItems = deduce [] [forwardApp,backwardApp, forwardComp] (getAxioms l (words sent))
        allItems = deduce [typeRaise] [forwardApp,backwardApp, forwardComp] (getAxioms l (words sent))

longitem :: Int -> CCGItem -> Bool
longitem n (MkItem _ _ i j)
         | i == 0 && j == n = True
         | otherwise        = False


lex1 =    [Lex "john" NP (Var "john"),
           Lex "mary" NP (Var "mary"),
           Lex "laughs" (S :\ NP) (Lambda "u" $ Appl (Var "laugh") (Var "u")),
           Lex "sees" ((S :\ NP) :/ NP) (Lambda "u" $ Lambda "v" $ Appl (Appl (Var "see") (Var "u")) (Var "v")),
           Lex "knows" ((S :\ NP) :/ NP) (Lambda "u" $ Lambda "v" $ Appl (Appl (Var "know") (Var "u")) (Var "v")),
           Lex "thinks" ((S:\NP):/S) (Lambda "P" $ Lambda "u" (Appl (Appl (Var "think") (Var "P")) (Var "u"))),
           Lex "that" (S:/S) (Lambda "P" (Var "P"))
           ]

lex2 =    [Lex "john" NP (Var "john"),
           Lex "mary" NP (Var "mary"),
           Lex "laughs" (S :\ NP) (Lambda "u" $ Appl (Var "laugh") (Var "u")),
           Lex "laugh" (S :\ NP) (Lambda "u" $ Appl (Var "laugh") (Var "u")),           
           Lex "sees" ((S :\ NP) :/ NP) (Lambda "u" $ Lambda "v" $ Appl (Appl (Var "see") (Var "u")) (Var "v")),
           Lex "knows" ((S :\ NP) :/ NP) (Lambda "u" $ Lambda "v" $ Appl (Appl (Var "know") (Var "u")) (Var "v")),
           Lex "thinks" ((S:\NP):/S) (Lambda "P" $ Lambda "u" (Appl (Appl (Var "think") (Var "P")) (Var "u"))),
           Lex "that" (S:/S) (Lambda "P" (Var "P")),
           Lex "and" (((S:/NP) :\ (S:/NP)) :/ (S:/NP)) (Lambda "P" $ Lambda "Q" $ Lambda "R" (Conj (Appl (Var "Q") (Var "R")) (Appl (Var "P") (Var "R")))),
           Lex "and"  (((S:\NP) :\ (S:\NP)) :/ (S:\NP)) (Lambda "P" $ Lambda "Q" $ Lambda "R" (Conj (Appl (Var "Q") (Var "R")) (Appl (Var "P") (Var "R")))),
           Lex "and" (((S :/ (S :\ NP)) :\ (S :/ (S :\ NP))) :/ (S :/ (S :\ NP))) (Lambda "P" $ Lambda "Q" $ Lambda "R" (Conj (Appl (Var "Q") (Var "R")) (Appl (Var "P") (Var "R"))))       
           ]

lex3 =    [Lex "john" NP (Var "john"),
           Lex "mary" NP (Var "mary"),
           Lex "lucy" NP (Var "lucy"),
           Lex "laughs" (S :\ NP) (Lambda "u" $ Appl (Var "laugh") (Var "u")),
           Lex "sees" ((S :\ NP) :/ NP) (Lambda "u" $ Lambda "v" $ Appl (Appl (Var "see") (Var "u")) (Var "v")),
           Lex "knows" ((S :\ NP) :/ NP) (Lambda "u" $ Lambda "v" $ Appl (Appl (Var "know") (Var "u")) (Var "v")),
           Lex "thinks" ((S:\NP):/S) (Lambda "P" $ Lambda "u" (Appl (Appl (Var "think") (Var "P")) (Var "u"))),
           Lex "that" (S:/S) (Lambda "P" (Var "P")),
           Lex "who" ((N :\ N) :/ (S :\ NP)) (Lambda "P" $ Lambda "Q" $ Lambda "z" (Conj (Appl (Var "Q") (Var "x")) (Appl (Var "P") (Var "z")))),
           Lex "whom" ((N :\ N) :/ (S :/ NP)) (Lambda "P" $ Lambda "Q" $ Lambda "z" (Conj (Appl (Var "Q") (Var "x")) (Appl (Var "P") (Var "z")))),
           Lex "and" (((S:/NP) :\ (S:/NP)) :/ (S:/NP)) (Lambda "P" $ Lambda "Q" $ Lambda "R" (Conj (Appl (Var "Q") (Var "R")) (Appl (Var "P") (Var "R")))),
           Lex "and"  (((S:\NP) :\ (S:\NP)) :/ (S:\NP)) (Lambda "P" $ Lambda "Q" $ Lambda "R" (Conj (Appl (Var "Q") (Var "R")) (Appl (Var "P") (Var "R")))),
           Lex "and" (((S :/ (S :\ NP)) :\ (S :/ (S :\ NP))) :/ (S :/ (S :\ NP))) (Lambda "P" $ Lambda "Q" $ Lambda "R" (Conj (Appl (Var "Q") (Var "R")) (Appl (Var "P") (Var "R")))),  
           Lex "every" ((S :/ (S :\NP)) :/ N) (Lambda "P" $ Lambda "Q" $ Forall "x" (Impl (Appl (Var "P") (Var "x")) (Appl (Var "Q") (Var "x")))),
           Lex "every" ((S :\ (S :/NP)) :/ N) (Lambda "P" $ Lambda "Q" $ Forall "x" (Impl (Appl (Var "P") (Var "x")) (Appl (Var "Q") (Var "x")))),
           Lex "some"  ((S :/ (S :\NP)) :/ N) (Lambda "P" $ Lambda "Q" $ Exists "y" (Conj (Appl (Var "P") (Var "y")) (Appl (Var "Q") (Var "y")))),
           Lex "some" ((S :\ (S :/NP)) :/ N) (Lambda "P" $ Lambda "Q" $ Exists "y" (Conj (Appl (Var "P") (Var "y")) (Appl (Var "Q") (Var "y")))),
           Lex "donkey" N (Lambda "u" $ Appl (Var "donkey") (Var "u")),
           Lex "student" N (Lambda "w" $ Appl (Var "student") (Var "w"))
--         Lex "happy"
--         Lex "near"
           ]