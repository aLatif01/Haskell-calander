import Data.Time.Calendar.WeekDate
import Data.Time

data Weekdays
    = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Eq, Enum, Bounded)

data Months
    = January | February | March     | April   | May      | June
    | July    | August   | September | October | November | December
    deriving (Enum, Eq, Show)

weekday = formatTime defaultTimeLocale "%a"

firstOfMonth :: (Int, Integer) -> Weekdays
firstOfMonth (a,b)
  | theDay == "Sun" = Sunday
  | theDay == "Mon"  = Monday
  | theDay == "Tue" = Tuesday
  | theDay == "Wed" = Wednesday
  | theDay == "Thu" = Thursday
  | theDay == "Fri" = Friday
  | theDay == "Sat" = Saturday
  where
    theDay = weekday (fromGregorian b a 01)

numberOfDays :: Months -> Int
numberOfDays x
  | x == January = 31
  | x == February = 28
  | x == March = 31
  | x == April = 30
  | x == May = 31
  | x == June = 30
  | x == July = 31
  | x == August = 31
  | x == September = 30
  | x == October = 31
  | x == November = 30
  | x == December = 31
  | otherwise = 28

monthToInt :: Months -> Int
monthToInt = ( +1) . fromEnum

next :: (Eq a, Enum a, Bounded a) => a -> a
next x | x == maxBound = minBound
       | otherwise     = succ x

pad :: Int -> String
pad day = case show day of
    [x] -> [' ', x]
    xs  -> xs

month :: Months -> Integer -> String
month m x = "\n   " ++ show m ++ " " ++ show x ++ "\n" ++ week ++ monthStart Sunday
  where

    startDay = firstOfMonth(monthToInt m, x)
    maxDay = numberOfDays m
    week = "Su Mo Tu We Th Fr Sa\n"

    monthStart currDay | startDay == currDay = days startDay 1
                       | otherwise           = "   " ++ monthStart (next currDay)

    days Sunday    x | x > maxDay = "\n"
    days _         x | x > maxDay = "\n\n"
    days Saturday  x              = pad x ++ "\n" ++ days  Sunday    (succ x)
    days day       x              = pad x ++ " "  ++ days (next day) (succ x)

--printCal = month October 2019
--cal = putStr printCal

cal :: Months -> Integer -> IO ()
cal m x = putStr(month m x)
