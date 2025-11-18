-- cycleSucc берёт следующее, возвращая после последнего значения минимальное
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound then minBound else succ n
