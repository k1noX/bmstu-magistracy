import Data.Semigroup

newtype Events = Events [String]
  deriving (Show, Eq)

newtype Probs = Probs [Double]
  deriving (Show, Eq)

instance Semigroup Events where
  Events a <> Events b = Events (a ++ b)

instance Monoid Events where
  mempty = Events []

instance Semigroup Probs where
  Probs a <> Probs b = Probs (a ++ b)

instance Monoid Probs where
  mempty = Probs []
