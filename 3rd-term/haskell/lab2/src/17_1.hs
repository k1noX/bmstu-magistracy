import Data.Semigroup (Semigroup)

-- Color - перечисление цветов
data Color = Clear | Red | Yellow | Blue | Green | Purple | Orange
  deriving (Enum)

instance Eq Color where
  c1 == c2 = fromEnum c1 == fromEnum c2

instance Ord Color where
  compare c1 c2 = compare (fromEnum c1) (fromEnum c2)

combineColors :: Color -> Color -> Color
combineColors Clear c = c
combineColors c Clear = c
combineColors Red Blue = Purple
combineColors Blue Red = Purple
combineColors Red Yellow = Orange
combineColors Yellow Red = Orange
combineColors Yellow Blue = Green
combineColors Blue Yellow = Green
combineColors c1 c2
  | c1 == c2 = c1
  | otherwise = error "Unknown color combination"

instance Semigroup Color where
  (<>) = combineColors

instance Monoid Color where
  mempty = Clear
