cup ml = \message -> message ml

aCup = cup 180
getMl aCup = aCup (\ml -> ml)

drink aCup mlDrank =
    if mlDiff >= 0
        then cup mlDiff
    else cup 0
    where
        ml = getMl aCup
        mlDiff = ml - mlDrank

isEmpty aCup = getMl aCup == 0

