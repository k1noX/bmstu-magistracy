-- | myReverse возвращает список с инвертированным порядком следования объектов.
-- Пример использования: `myReverse [1..5]` -> `[5,4,3,2,1]`
myReverse (x:xs) = case length xs of
    0 -> [x]
    r -> myReverse xs ++ [x]
