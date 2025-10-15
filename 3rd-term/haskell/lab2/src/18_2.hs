import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen
    deriving (Show, Eq, Ord)

organCatalog :: [Organ]
organCatalog = [Heart, Heart, Brain, Spleen, Kidney, Kidney, Kidney]

organInventory :: Map.Map Organ Int
organInventory = Map.fromListWith (+) [(organ, 1) | organ <- organCatalog]
