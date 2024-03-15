{-# OPTIONS_GHC -Wall #-}

-- | Module Cha6: Implémente la sixième épreuve du roi.
module Cha6 (challenge6) where

-- Importation du module CPL.
import CPL

-- | Formule représentant l'affiche de la porte 1.
door1 :: Formula
door1 = Var "t1"

-- | Formule représentant l'affiche de la porte 2.
door2 :: Formula
door2 = Var "p2"

-- | Formule représentant l'affiche de la porte 3.
door3 :: Formula
door3 = Var "t2"

-- | Formule représentant la contrainte globale.
constraint :: Formula
constraint =
  And
    (And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2"))))
    (Eqv (Var "p3") (Not (Var "t3")))

-- | Formule représentant la règle de l'épreuve.
rule :: Formula
rule =
  And
    ( Or
        (And door1 (And (Not door2) (Not door3)))
        ( Or
            (And door2 (And (Not door1) (Not door3)))
            (And door3 (And (Not door1) (Not door2)))
        )
    )
    ( Or
        (And (Var "p1") (And (Var "t2") (Var "t3")))
        ( Or
            (And (Var "p2") (And (Var "t1") (Var "t3")))
            (And (Var "p3") (And (Var "t1") (Var "t2")))
        )
    )

-- | Formule représentant la formule logique de l’épreuve.
challenge6 :: Formula
challenge6 = And constraint rule

-- | Liste des mondes satisfaisant la formule logique de l'épreuve.
solution :: [World]
solution = findWorlds challenge6