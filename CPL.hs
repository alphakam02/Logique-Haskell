module CPL (Formula (..),
  world,
  genllWorlds,
  testGenAllWorlds,
  sat,
  testSat,
  findWorlds,
  testFindWorlds,
  testAll) where
data Formula = T
  |F
  |Var [char]
  |And Formula Formula
  |Not Formula
  |Or Formula Formula
  |Imp Formula
  |Eqv Formula
  deriving (show)

constraint :: Formula
constraint 
     



