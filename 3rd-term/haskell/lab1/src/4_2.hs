addressLetter name location = nameText ++ " - " ++ location
    where nameText = fst name ++ " " ++ snd name

sfOffice name =
    if lastName < "L"
        then
            nameText ++
            " - PO Box 1234, San Francisco, CA 94111"
        else
            nameText ++
            " - PO Box 1010, San Francisco, CA 94109"
    where
        lastName = snd name
        nameText = fst name ++ " " ++ lastName

nyOffice name =
        nameText ++
        ": PO Box 789, New York, NY 10013"
    where
        nameText = fst name ++ " " ++ snd name

renoOffice name =
    nameText ++
        " - PO Box 456, Reno, NV 89523"
    where nameText = snd name

-- | Функция, определнённая по заданию, для офиса в Колумбии
columbiaOffice name =
    "Highly respected "
    ++ fst name
    ++ " "
    ++ snd name
    ++ " - PO Box 1601, Columbia, SC 29201"

-- | Пример использования: `getLocationFunction "columbia" ("Nikita", "Safronov")`
getLocationFunction location =
    case location of
        "ny" -> nyOffice
        "sf" -> sfOffice
        "reno" -> renoOffice
        "columbia" -> columbiaOffice
        _ -> (
            \name ->
                fst name ++ " " ++ snd name)

