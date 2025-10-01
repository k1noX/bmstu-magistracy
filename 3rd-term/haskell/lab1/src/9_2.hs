import Data.Char (toLower, isAlpha)

-- | isPalindrome проверяет, является ли строка палиндромом с учётом заглавных символов и пробелов.
-- Пример использования: `isPalindrome("А роза упала на лапу Азора")` -> True
isPalindrome s = cleaned == reverse cleaned
    where
        cleaned = map toLower (filter isAlpha s)
