-- | myGCD выполняет поиск наибольшего общего делителя.
-- Пример использования: `myGCD 100 32` -> 4
myGCD a b = case a `mod` b of
    0 -> b
    r -> myGCD b r
