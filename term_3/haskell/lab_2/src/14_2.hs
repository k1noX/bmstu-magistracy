-- FiveSidedDie представляет собой пятигранную кость
data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5
  deriving (Show, Eq, Ord, Enum, Bounded)

class (Eq a, Ord a, Bounded a, Enum a) => Die a where
  rollValue :: a -> Int
  allSides :: [a]
  allSides = [minBound .. maxBound]

instance Die FiveSidedDie where
  rollValue side = fromEnum side + 1
