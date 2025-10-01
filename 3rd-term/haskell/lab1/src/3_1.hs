-- | Пример использования: collapsedAdd 1 1
collapsedAdd = \y -> \x -> y + x

-- | Пример использования: sumSquareOrSquareSum 1 1
sumSquareOrSquareSum = \x -> \y -> (\sumSquare squareSum ->
    if sumSquare > squareSum
    then sumSquare
    else squareSum) (x ^ 2 + y ^ 2) ((x + y) ^ 2)
