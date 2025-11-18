daysInMonth :: [Int]
daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

calendarDates :: [(Int, Int)]
calendarDates = [ (month, day)
                | month <- [1..12]
                , day <- [1..daysInMonth !! (month - 1)]
                ]
