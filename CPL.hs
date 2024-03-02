{-# OPTIONS_GHC -Wall #-}

-- | Module CPL: Module générique pour les épreuves du roi.
module CPL
  ( Formula (..),
    World,
    genAllWorlds,
    testGenAllWorlds,
    sat,
    testSat,
    findWorlds,
    testFindWorlds,
    testAll,
  )
where

-- Définition des types World et Formula.

-- ----------------------------------------------------------------------------

-- | Type World pour représenter un monde.
type World = [String]

-- | Type Formula pour représenter des formules booléennes.
data Formula
  = -- | La constante True.
    T
  | -- | La constante False.
    F
  | -- | Variable propositionnelle avec un nom.
    Var String
  | -- | Négation d'une formule.
    Not Formula
  | -- | Conjonction de deux formules.
    And Formula Formula
  | -- | Disjonction de deux formules.
    Or Formula Formula
  | -- | Implication logique.
    Imp Formula Formula
  | -- | Équivalence logique.
    Eqv Formula Formula
  deriving (Show)

-- ----------------------------------------------------------------------------

-- Fonctions pour manipuler les mondes et les formules.

-- ----------------------------------------------------------------------------

-- | Génère tous les mondes possibles à partir d'un monde de variables.
genAllWorlds :: World -> [World]
genAllWorlds [] = [[]]
genAllWorlds (x : xs) = combine (genAllWorlds xs)
  where
    -- Fonction auxiliaire pour combiner deux listes de mondes.
    combine w = map (\l -> x : l) w ++ w

-- | Vérifie si une variable est présente dans un monde donné.
contains :: String -> World -> Bool
contains _ [] = False
contains s (x : xs)
  | x == s = True
  | otherwise = contains s xs

-- | Évalue une formule dans un monde donné.
sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat w (Var s) = contains s w
sat w (Not phi) = not (sat w phi)
sat w (And phi psi) = sat w phi && sat w psi
sat w (Or phi psi) = sat w phi || sat w psi
sat w (Imp phi psi) = not (sat w phi) || sat w psi
sat w (Eqv phi psi) = sat w (Imp phi psi) && sat w (Imp psi phi)

-- | Fusionne deux mondes en supprimant les doublons.
merge :: World -> World -> World
merge [] w = w
merge w [] = w
merge (x : xs) w
  | contains x w = merge xs w
  | otherwise = x : merge xs w

-- | Extrait toutes les variables d'une formule en évitant les doublons.
extractVar :: Formula -> World
extractVar T = []
extractVar F = []
extractVar (Var s) = [s]
extractVar (Not phi) = extractVar phi
extractVar (And phi psi) = merge (extractVar phi) (extractVar psi)
extractVar (Or phi psi) = merge (extractVar phi) (extractVar psi)
extractVar (Imp phi psi) = merge (extractVar phi) (extractVar psi)
extractVar (Eqv phi psi) = merge (extractVar phi) (extractVar psi)

-- | Trouve tous les mondes dans lesquels une formule donnée est vraie.
findWorlds :: Formula -> [World]
findWorlds phi = filter (\w -> sat w phi) (genAllWorlds (extractVar phi))

-- ----------------------------------------------------------------------------

-- Tests pour les fonctions de manipulation des mondes et des formules.

-- ----------------------------------------------------------------------------

-- | Booléens indiquant le succès ou l'échec de la fonction genAllWorlds.
testGenAllWorlds :: [Bool]
testGenAllWorlds =
  [ genAllWorlds [] == [[]],
    genAllWorlds ["p1"] == [["p1"], []],
    genAllWorlds ["p1", "p2"] == [["p1", "p2"], ["p1"], ["p2"], []],
    genAllWorlds ["p1", "p2", "p3"]
      == [ ["p1", "p2", "p3"],
           ["p1", "p2"],
           ["p1", "p3"],
           ["p1"],
           ["p2", "p3"],
           ["p2"],
           ["p3"],
           []
         ]
  ]

-- | Booléens indiquant le succès ou l'échec de la fonction contains.
testContains :: [Bool]
testContains =
  [ not (contains "p1" []),
    contains "p1" ["p1", "p2", "p3"],
    not (contains "p4" ["p1", "p2", "p3"])
  ]

-- | Booléens indiquant le succès ou l'échec de la fonction sat.
testSat :: [Bool]
testSat =
  [ sat ["p1", "p2"] T,
    not (sat ["p1", "p2"] F),
    sat ["p1", "p2"] (And (Var "p1") (Var "p2")),
    not (sat ["p1", "p2"] (And (Var "p1") (Var "t1")))
  ]

-- | Booléens indiquant le succès ou l'échec de la fonction merge.
testMerge :: [Bool]
testMerge =
  [ merge ["p1", "p2"] [] == ["p1", "p2"],
    merge [] ["t1", "t2"] == ["t1", "t2"],
    merge ["p1", "p2"] ["p2", "p3"] == ["p1", "p2", "p3"],
    merge ["p1", "p2"] ["t1", "t2"] == ["p1", "p2", "t1", "t2"]
  ]

-- | Booléens indiquant le succès ou l'échec de la fonction extractVar.
testExtractVar :: [Bool]
testExtractVar =
  [ extractVar T == [],
    extractVar F == extractVar T,
    extractVar (And (Not (Var "p1")) F) == ["p1"],
    extractVar (Eqv (Var "p1") T) == ["p1"],
    extractVar (Imp (Or (Var "p1") (Var "t1")) (Var "t1")) == ["p1", "t1"]
  ]

-- | Booléens indiquant le succès ou l'échec de la fonction findWorlds.
testFindWorlds :: [Bool]
testFindWorlds =
  [ findWorlds T == [[]],
    findWorlds F == [],
    findWorlds (And (Var "p1") (Var "t2")) == [["p1", "t2"]],
    findWorlds
      ( And
          (Not (And (Var "p1") (Var "t1")))
          (Not (And (Var "p2") (Var "t2")))
      )
      == [ ["p1", "p2"],
           ["p1", "t2"],
           ["p1"],
           ["t1", "p2"],
           ["t1", "t2"],
           ["t1"],
           ["p2"],
           ["t2"],
           []
         ],
    findWorlds
      ( Or
          (And (Not (Var "p1")) (Var "p2"))
          (And (Var "p1") (Not (Var "p2")))
      )
      == [["p1"], ["p2"]]
  ]

-- | Vérifie si tous les éléments d'une Booléens sont True.
test :: [Bool] -> Bool
test l = foldr (&&) True l

-- | Exécute tous les tests et renvoie un message de réussite ou d'échec.
testAll :: [Char]
testAll
  | test
      [ test testGenAllWorlds,
        test testContains,
        test testSat,
        test testMerge,
        test testExtractVar,
        test testFindWorlds
      ] =
      "Success"
  | otherwise = "Fail"

-- ----------------------------------------------------------------------------