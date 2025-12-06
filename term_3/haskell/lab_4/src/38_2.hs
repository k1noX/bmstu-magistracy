safeSucc :: Int -> Maybe Int
safeSucc x
  | x == maxBound = Nothing
  | otherwise     = Just (x + 1)

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

safeLast :: Int -> [a] -> Either String a
safeLast limit xs
  | null xs          = Left "Empty list"
  | lengthGT xs limit = Left "List too long"
  | otherwise        = Right (last xs)
  where
    lengthGT :: [b] -> Int -> Bool
    lengthGT _  n | n < 0 = True
    lengthGT [] _         = False
    lengthGT (_:ys) n     = lengthGT ys (n - 1)
