module CPL (
    Formula(..),
    World,
    genAllWorlds,
    testGenAllWorlds,
    sat,
    testSat,
    findWorlds,
    testFindWorlds
) where

-- Définition du type World pour représenter les mondes possibles
type World = [[Char]]

-- Définition du type Formula pour représenter des formules booléennes
data Formula = T
             | F
             | Var [Char]
             | And Formula Formula
             | Not Formula
             | Or Formula Formula
             | Imp Formula Formula
             | Eqv Formula Formula
             deriving (Show)


-- Génération de tous les mondes possibles pour une liste de noms de variables propositionnelles
genAllWorlds :: World -> [World]
genAllWorlds [] = [[]]
genAllWorlds (x:xs) = (map (x:) (genAllWorlds xs)) ++ genAllWorlds xs


testGenAllWorlds :: [Bool]
testGenAllWorlds = [
    genAllWorlds ["p1", "p2"] == [["p1","p2"],["p1"],["p2"],[]]]
    

dansListe :: World -> [Char] -> Bool
dansListe [] _ = False
dansListe (x:xs) y
  | y == x = True
  | otherwise = dansListe xs y

sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat w (Var v) = (dansListe w v)
sat w (And phi psi) = (sat w phi) && (sat w psi)
sat w (Not phi) = not(sat w phi)
sat w (Or phi psi) =  (sat w phi) || (sat w psi)
sat w (Imp phi psi) = not(sat w phi) || (sat w psi)
sat w (Eqv phi psi) = (sat w (Imp phi psi)) && (sat w (Imp psi phi))

testSat :: [Bool]
testSat = [
    (sat ["p1", "p2"] T) == True,
    (sat ["p1", "p2"] F) == False,
    (sat ["p1", "p2"] (And (Var "p1") (Var "p2"))) == True]



enlever :: World -> World
enlever [] = []
enlever (x:xs)
  | dansListe xs x = enlever xs
  | otherwise = x:(enlever xs)


extrait :: Formula -> World
extrait T = []
extrait F = []
extrait (Var v) = [v]
extrait (Not phi) = extrait phi
extrait (And phi psi) = extrait phi ++ extrait psi
extrait (Or phi psi) = extrait phi ++ extrait psi
extrait (Imp phi psi) = extrait phi ++ extrait psi
extrait (Eqv phi psi) = extrait phi ++ extrait psi

extraitElemets :: Formula -> World
extraitElemets phi = enlever (extrait phi)

findWorlds :: Formula -> [World]
findWorlds phi = filter (\w -> sat w phi) (genAllWorlds (extraitElemets phi))


testFindWorlds :: [Bool]
testFindWorlds = [
    findWorlds (Var "p1") == [["p1"]],
    findWorlds (And (Var "p1") (Var "t1")) == [["p1","t1"]]]

test :: [Bool] -> Bool
test [] = True
test (x:xs) = x && test xs

testAll :: [Char]
testAll
  | (test(testGenAllWorlds)) && (test(testSat)) && (test(testFindWorlds)) = "Success !"
  | otherwise = "Fail !"