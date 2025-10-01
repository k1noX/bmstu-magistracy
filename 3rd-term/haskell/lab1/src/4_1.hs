import Data.List

-- | Пример использования: sortBy compareLastNames names
compareLastNames name1 name2 =
    if cmpLastNames == EQ
        then compare firstName1 firstName2
    else cmpLastNames
    where
        lastName1 = snd name1
        lastName2 = snd name2
        firstName1 = fst name1
        firstName2 = fst name2
        cmpLastNames = compare lastName1 lastName2


-- ого, да это же Joy Division
names = [
    ("Ian", "Curtis"),
    ("Steven", "Morris"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook")]
