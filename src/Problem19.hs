module Problem19 (result) where

years = [1901..2000]
leapYears = filter (\x -> rem x 4 == 0) years
nonLeapYears = filter (\x -> rem x 4 /= 0) years
dayCount = 366 * length leapYears + 365 * length nonLeapYears

febDays isLeapYear
  | isLeapYear = 29
  | otherwise = 28

months isLeapYear = [
  ("january", 31),
  ("february", febDays isLeapYear),
  ("march", 31),
  ("april", 30),
  ("may", 31),
  ("june", 30),
  ("july", 31),
  ("august", 31),
  ("september", 30),
  ("october", 31),
  ("november", 30),
  ("december", 31)]

weekdays = concat (repeat ["mon", "tue", "wed", "thu", "fri", "sat", "sun"])

monthWeekDays = result
  where
    year = 1901
    isLeapYear = rem year 4 == 0
    month = take 1 (months isLeapYear)
    result = month

result = monthWeekDays
