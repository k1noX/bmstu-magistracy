-- | binaryPartialApplication - функция, которая позволяет замыкать первый аргумент в функции из двух аргументов
binaryPartialApplication f x = \y -> f x y

ifEven f n =
    if even n
        then f n
        else n

-- | Пример использования: `ifEvenInc(3)` -> 3; `ifEvenInc(4)` -> 5
ifEvenInc    = binaryPartialApplication ifEven (+1)

-- | Пример использования: `ifEvenDouble(3)` -> 3; `ifEvenDouble(4)` -> 8
ifEvenDouble = binaryPartialApplication ifEven (*2)

-- | Пример использования: `ifEvenSquare(3)` -> 3; `ifEvenSquare(4)` -> 16
ifEvenSquare = binaryPartialApplication ifEven (^2)

