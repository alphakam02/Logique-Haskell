{-# OPTIONS_GHC -Wall #-}

-- | Module Cha2: Implémente la deuxième épreuve du roi.
module Cha2 (challenge2) where

-- Importation du module CPL.
import CPL

-- | Formule représentant l'affiche de la porte 1.
door1 :: Formula
door1 = Or (Var "p1") (Var "p2")

-- | Formule représentant l'affiche de la porte 2.
door2 :: Formula
door2 = Var "t1"

-- | Formule représentant la contrainte globale.
constraint :: Formula
constraint =
  And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

-- | Formule représentant la règle de l'épreuve.
rule :: Formula
rule = Eqv door1 door2

-- | Formule représentant la formule logique de l’épreuve.
challenge2 :: Formula
challenge2 = And constraint rule

-- | Liste des mondes satisfaisant la formule logique de l'épreuve.
solution :: [World]
solution = findWorlds challenge2