data Color = Red | Green | Blue
  deriving (Enum)

instance Eq Color where
  c1 == c2 = fromEnum c1 == fromEnum c2

instance Ord Color where
  compare c1 c2 = compare (fromEnum c1) (fromEnum c2)
