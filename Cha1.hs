module Cha1 where

import CPL

constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

reglement :: Formula 
reglement = Or (And (Var "A") (Not (Var "B"))) (And (Not(Var "A")) (Var "B"))

challenge :: Formula
challenge = And constraint reglement
