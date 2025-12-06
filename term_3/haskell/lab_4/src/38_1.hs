import Data.Char (isDigit)

isValidIntStr :: String -> Bool
isValidIntStr s = not (null s) && all isDigit s

safeReadInt :: String -> Int
safeReadInt = read

addStrInts :: String -> String -> Either String Int
addStrInts s1 s2
  | valid1 && valid2 = Right (safeReadInt s1 + safeReadInt s2)
  | not valid1 && not valid2 = Left "Neither value could be parsed"
  | not valid1 = Left "First value could not be parsed"
  | otherwise = Left "Second value could not be parsed"
  where
    valid1 = isValidIntStr s1
    valid2 = isValidIntStr s2
