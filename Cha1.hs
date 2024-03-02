{-# OPTIONS_GHC -Wall #-}

-- | Module Cha1: Implémente la première épreuve du roi.
module Cha1 (challenge1) where

-- Importation du module CPL.
import CPL

-- | Formule représentant l'affiche de la porte 1.
door1 :: Formula
door1 = And (Var "p1") (Var "t2")

-- | Formule représentant l'affiche de la porte 2.
door2 :: Formula
door2 = And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

-- | Formule représentant la contrainte globale.
constraint :: Formula
constraint =
  And
    (Not (And (Var "p1") (Var "t1")))
    (Not (And (Var "p2") (Var "t2")))

-- | Formule représentant la règle de l'épreuve.
rule :: Formula
rule = Or (And door1 (Not door2)) (And (Not door1) door2)

-- | Formule représentant la formule logique de l’épreuve.
challenge1 :: Formula
challenge1 = And constraint rule

-- | Liste des mondes satisfaisant la formule logique de l'épreuve.
solution :: [World]
solution = findWorlds challenge1