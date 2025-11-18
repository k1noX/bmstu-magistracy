-- | subseq - выбирает подпоследовательность в списке.
-- Пример использования: `subseq 2 5 [1..10]" -> [3,4,5]
subseq lBound rBound d =
    take (rBound - lBound) (drop lBound d)

