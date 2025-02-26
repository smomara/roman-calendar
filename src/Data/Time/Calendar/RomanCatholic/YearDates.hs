module Data.Time.Calendar.RomanCatholic.YearDates where

import Data.Time (Day, DayOfWeek (Sunday), addDays, dayOfWeek, fromGregorian)
import Data.Time.Calendar.Easter (gregorianEaster)
import Data.Time.Calendar.RomanCatholic.Types (YearDates (..))

computeYearDates :: Integer -> YearDates
computeYearDates year =
  YearDates
    { adventStart = findNextSunday (fromGregorian year 11 27)
    , christmasStart = fromGregorian year 12 25
    , baptismDate = findNextSunday (fromGregorian year 1 6)
    , lentStart = addDays (-46) easter
    , holyWeekStart = addDays (-7) easter
    , triduumStart = addDays (-3) easter
    , easterDate = easter
    , pentecostDate = addDays 49 easter
    , ordinaryTime2Start = addDays 50 easter
    , christKingDate = addDays (-7) (findNextSunday (fromGregorian (year + 1) 11 27))
    }
 where
  easter = gregorianEaster year

findNextSunday :: Day -> Day
findNextSunday date = case dayOfWeek date of
  Sunday -> date
  _ -> addDays (fromIntegral $ (7 - fromEnum (dayOfWeek date)) `mod` 7) date
