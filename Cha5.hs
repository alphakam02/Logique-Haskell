{-# OPTIONS_GHC -Wall #-}

-- | Module Cha5: Implémente la cinquième épreuve du roi.
module Cha5 (challenge5) where

-- Importation du module CPL.
import CPL

-- | Formule représentant l'affiche de la porte 1.
door1 :: Formula
door1 = Or (And (Var "p1") (Var "t2")) (And (Var "t1") (Var "p2"))

-- | Formule représentant l'affiche de la porte 2.
door2 :: Formula
door2 = And (Var "p1") (Var "t2")

-- | Formule représentant la contrainte globale.
constraint :: Formula
constraint =
  And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

-- | Formule représentant la règle de l'épreuve.
rule :: Formula
rule =
  And
    (Or (And door1 (Var "p1")) (And (Not door1) (Var "t1")))
    (Or (And door2 (Var "t2")) (And (Not door2) (Var "p2")))

-- | Formule représentant la formule logique de l’épreuve.
challenge5 :: Formula
challenge5 = And constraint rule

-- | Liste des mondes satisfaisant la formule logique de l'épreuve.
solution :: [World]
solution = findWorlds challenge5