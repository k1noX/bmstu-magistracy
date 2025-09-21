-- | Пример использования: `calcNBased 1`
calcNBased n =
    if even n
        then n - 2
    else 3 * n + 1
