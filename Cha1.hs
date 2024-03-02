module Cha1 where

import CPL

door1 :: Formula
door1 = And (Var "p1") (Var "t2")

door2 :: Formula
door2 = And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))


constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

reglement :: Formula 
reglement = Or (And (Not door1) (door2)) (And (door1) (Not door2))

challenge1 :: Formula
challenge1 = And constraint reglement


solution :: [World]
solution = findWorlds challenge1
