module Data.Time.Calendar.RomanCatholic.Season where

import Data.Time (Day, toGregorian)
import Data.Time.Calendar.RomanCatholic.Types

inRange :: Day -> Day -> Day -> Bool
inRange a b day = day >= a && day < b

getLiturgicalSeason :: YearDates -> Day -> LiturgicalSeason
getLiturgicalSeason dates day
  | inRange (adventStart dates) (christmasStart dates) day = Advent
  | inChristmasSeason = Christmas
  | inRange (baptismDate dates) (lentStart dates) day = OrdinaryTime1
  | inRange (lentStart dates) (holyWeekStart dates) day = Lent
  | inRange (holyWeekStart dates) (triduumStart dates) day = HolyWeek
  | inRange (triduumStart dates) (easterDate dates) day = Triduum
  | inRange (easterDate dates) (ordinaryTime2Start dates) day = Easter
  | otherwise = OrdinaryTime2
 where
  (_, month, _) = toGregorian day
  inChristmasSeason =
    (month == 12 && day >= christmasStart dates)
      || (month == 1 && day <= baptismDate dates)
