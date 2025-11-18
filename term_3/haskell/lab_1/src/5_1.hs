ifEven f n =
    if even n
        then f n
        else n

-- | Пример использования: `ifEvenInc(3)` -> 3; `ifEvenInc(4)` -> 5
ifEvenInc    = ifEven (+1)

-- | Пример использования: `ifEvenDouble(3)` -> 3; `ifEvenDouble(4)` -> 8
ifEvenDouble = ifEven (*2)

-- | Пример использования: `ifEvenSquare(3)` -> 3; `ifEvenSquare(4)` -> 16
ifEvenSquare = ifEven (^2)
