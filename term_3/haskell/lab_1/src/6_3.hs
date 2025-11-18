-- | inFirstHalf проверяет находится ли элемент в первой половине списка.
-- Пример использования: `inFirstHalf 5 [1..10]` -> `True`, `inFirstHalf 5 [1..9]` -> `False`
inFirstHalf x xs = x `elem` firstHalf
    where
        halfLen = length xs `div` 2
        firstHalf = take halfLen xs
