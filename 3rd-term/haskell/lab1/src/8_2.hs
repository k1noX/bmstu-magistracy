-- | fastFib реализация быстрого подсчёта чисел Фибоначчи.Applicative
-- Пример использования: `fastFib 1 1 5` -> 8
fastFib n1 n2 1 = n2
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)
