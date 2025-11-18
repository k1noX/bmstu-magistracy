import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen
    deriving (Show, Eq, Ord)

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents id = Map.lookup id catalog

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers drawers = length [() | Nothing <- drawers]
