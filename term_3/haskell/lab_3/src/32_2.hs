daysInMonth :: [Int]
daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

calendarDates :: [(Int, Int)]
calendarDates = do
  month <- [1..12]
  day <- [1..daysInMonth !! (month - 1)]
  return (month, day)
