-- | Пример использования: counter 1
counter x =
    (\x ->
        (\x -> x) (x + 1)
    ) (x + 1)
