--main file
module TenseSemantics where

--CCG parser from the last lecture with the following changes:
-- * new SynTypes Sup, Inf, Adj
import CCGSem as Parser

--lambda calculus implementation with the following changes:
-- * added temporal ordering relations for events and an Append term for generating new variable names
import Combinators

--aspects

--a completed action
perfect :: Term
perfect = Lambda "s" $ Lambda "r1'" $ Before (Var "s") (Var "r1")

--an action in progress
simple :: Term
simple = Lambda "s" $ Lambda "r1'" $ Equal (Var "s") (Var "r1'")

--an aspect-like thing for an action taking place over an interval
dense :: Term
dense = Lambda "s" $ Dense (Var "s")

--tenses

past :: Term
past = Lambda "r1'" $ Before (Var "r1'") (Var "r0")

present :: Term
present = Lambda "r1'" $ Subset (Var "r0") (Var "r1'")

future :: Term
future = Lambda "r1'" $ After (Var "r1'") (Var "r0")

--methods for inflecting lambda expressions

inflect :: Term -> Term -> Term -> Term
inflect root aspect tense = addTense (addAspect root aspect) tense

--apply an aspect to a root
addAspect :: Term -> Term -> Term
addAspect root aspect = eval (Appl root aspect)

--apply a tense to a root
addTense :: Term -> Term -> Term
addTense root tense = eval (Appl root tense)

--make an action extended by adding the term (D i) to all exists statements which it contains
makeDense :: Term -> Term
makeDense (Impl t1 t2) = Impl (makeDense t1) (makeDense t2)
makeDense (Conj t1 t2) = Conj (makeDense t1) (makeDense t2)
makeDense (Lambda v' t') = Lambda v' (makeDense t')
makeDense (Forall v t) = Forall v (makeDense t)
makeDense (Exists v t) = Exists v (Conj (makeDense t) (Appl dense (Var v)))
makeDense (Appl t1 t2) = Appl t1 t2
makeDense (Equal t1 t2) = Equal t1 t2
makeDense (Subset t1 t2) = Subset t1 t2
makeDense (Before t1 t2) = Before t1 t2
makeDense (After t1 t2) = After t1 t2
makeDense (Throughout t1 t2) = Throughout t1 t2
makeDense (Dense t1) = Dense t1
makeDense (At t1 t2) = At t1 t2
makeDense (Var t) = Var t

fill2ndArg :: Term
fill2ndArg = Lambda "s*" $ Lambda "u*" $ Lambda "a*" $ Appl (Appl (Var "s*") (Var "a*")) (Var "u*")

fill3rdArg = Lambda "s*" $ Lambda "u*" $ Lambda "a*" $ Lambda "t**" $ Appl (Appl (Appl (Var "s*") (Var "a*")) (Var "t**")) (Var "u*")

--used to swap the order of arguments on non-finite transitive forms
transSup :: Term -> Term
transSup root = eval (Appl fill3rdArg root)

--a (unused) method for abstracting over reference time
absR1 :: Term -> Term
absR1 t = Lambda "r1" $ t

--vocab

be :: Term
be = Lambda "a'" $ Lambda "t*'" $ Lambda "v'" $ Lambda "u'" $ Appl (inflect (Var "v'") (Var "a'") (Var "t*'")) (Var "u'")

--have and do have an aspect argument which is not used
have :: Term
have = Lambda "a''" $ Lambda "t*''" $ Lambda "v''" $ Lambda "u''" $ Appl (inflect (Var "v''") perfect (Var "t*''")) (Var "u''")

doV :: Term
doV = Lambda "a'''" $ Lambda "t*'''" $ Lambda "p" $ Lambda "z" $ Appl (inflect (Var "p") simple (Var "t*'''")) (Var "z")

laugh :: Term
laugh = Lambda "a" $ Lambda "t*" $ Lambda "u" $ Lambda "r1" $ Conj (Exists "i" (Conj (At (Appl (Var "laugh") (Var "u")) (Var "i")) (Appl (Appl (Var "a") (Var "i")) (Var "r1")))) (Appl (Var "t*") (Var "r1"))

see :: Term
see = Lambda "a" $ Lambda "t*" $ Lambda "u" $ Lambda "v" $ Lambda "r1" $ Conj (Exists "i" (Conj (At (Appl (Appl (Var "see") (Var "u")) (Var "v")) (Var "i")) (Appl (Appl (Var "a") (Var "i")) (Var "r1")))) (Appl (Var "t*") (Var "r1"))

------------------------------------------------------------------
--infinite forms have no aspect and no tense
--present participles are modified to be "dense"
--finite forms are inflected for aspect and tense
------------------------------------------------------------------

eng = [

       --dummy nouns
       Lex "john" NP (Var "john"),
       Lex "mary" NP (Var "mary"),

       Lex "have" (Inf :/ Sup) (transSup have),
       Lex "having" (Adj :/ Sup) (transSup have),
       Lex "has" ((S :\ NP) :/ Sup) (inflect have simple present),
       Lex "had" ((S :\ NP) :/ Sup) (inflect have simple past),

       Lex "be" (Inf :/ Adj) (transSup be),
       Lex "being" (Adj :/ Adj) (transSup (makeDense be)),
       Lex "been" (Sup :/ Adj) (transSup be),
       Lex "is" ((S :\ NP) :/ Adj) (inflect be simple present),
       Lex "was" ((S :\ NP) :/ Adj) (inflect be simple past),

       Lex "does" ((S :\ NP) :/ Inf) (inflect doV simple present),
       Lex "did" ((S :\ NP) :/ Inf) (inflect doV simple past),
       Lex "will" ((S :\ NP) :/ Inf) (inflect doV simple future),

       --These require more than one reference point to be implemented properly
       --Lex "going" (Adj :/ Inf') going,
       --Lex "about" (Adj :/ Inf') going,
       --Lex "to" (Inf' :/ Inf) (Lambda "x'" $ Var "x'"),

       Lex "laugh" Inf laugh,
       Lex "laughed" Sup laugh,
       Lex "laughing" Adj (makeDense laugh),
       Lex "laughs" (S :\ NP) (inflect laugh simple present),
       Lex "laughed" (S :\ NP) (inflect laugh simple past),

       Lex "see" (Inf :/ NP) (transSup see),
       Lex "seen" (Sup :/ NP) (transSup see),
       Lex "seeing" (Adj :/ NP) (transSup (makeDense see)),
       Lex "sees" ((S :\ NP) :/ NP) (inflect see simple present),
       Lex "saw" ((S :\ NP) :/ NP) (inflect see simple past),

       Lex "while" ((S :/ S) :\ S) (Lambda "s1" $ Lambda "s2" $ Lambda "r*" $ Conj (Conj (Appl (Var "s1") (Append (Var "r*") "_0")) (Appl (Var "s2") (Append (Var "r*") "_1"))) (Equal (Append (Var "r*") "_0") (Append (Var "r*") "_1"))),
       Lex "before" ((S :/ S) :\ S) (Lambda "s1" $ Lambda "s2" $ Lambda "r*" $ Conj (Conj (Appl (Var "s1") (Append (Var "r*") "_0")) (Appl (Var "s2") (Append (Var "r*") "_1"))) (Before (Append (Var "r*") "_0") (Append (Var "r*") "_1"))),
       Lex "after" ((S :/ S) :\ S) (Lambda "s1" $ Lambda "s2" $ Lambda "r*" $ Conj (Conj (Appl (Var "s1") (Append (Var "r*") "_0")) (Appl (Var "s2") (Append (Var "r*") "_1"))) (After (Append (Var "r*") "_0") (Append (Var "r*") "_1")))

       ]

interpret :: Lexicon -> String -> Term
interpret l s = (reduce . eval) (Appl (getMeaning (head (parse l s))) (Var "r1"))