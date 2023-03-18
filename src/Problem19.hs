module Problem19 (result) where
import Data.Time (parseTimeOrError, defaultTimeLocale, UTCTime (UTCTime, utctDay))
import Data.Time.Calendar.WeekDate (toWeekDate)

years = [1900..2000]
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

isLeapYear year = rem year 4 == 0 && (rem year 100 /= 0 || rem year 400 == 0)
yearMonths = concatMap (\year -> map (year,) (months (isLeapYear year))) years

monthWeekDays = result
  where
    go [] monthWeekDays weekdays = monthWeekDays
    go (month:months) monthWeekDays weekdays = go months monthWeekDays' weekdays'
      where
        (year, (monthName, dayCount)) = month
        monthWeekDays' = (year, monthName, take dayCount weekdays) : monthWeekDays
        weekdays' = drop dayCount weekdays
    result = go yearMonths [] weekdays

data DayOfWeek = Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday
  deriving (Eq, Read, Show)

instance Enum DayOfWeek where
  toEnum 0 = Sunday
  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum 7 = Sunday -- Yes, twice.
  toEnum n = toEnum (n `mod` 7)

  fromEnum Monday = 1
  fromEnum Tuesday = 2
  fromEnum Wednesday = 3
  fromEnum Thursday = 4
  fromEnum Friday = 5
  fromEnum Saturday = 6
  fromEnum Sunday = 7

weekday dateStr = ((\(_,_,d) -> d) . toWeekDate . utctDay) (parseTimeOrError True defaultTimeLocale "%Y-%m-%d" dateStr :: UTCTime)
filtered = filter (\(year, monthName, month) -> year > 1900 && head month == "sun") monthWeekDays

result = length filtered
