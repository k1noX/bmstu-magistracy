-- | myRepeat - попытка повторения `repeat`.
-- Пример использования: `take 6 (myRepeat 'a')`
myRepeat x = cycle [x]
