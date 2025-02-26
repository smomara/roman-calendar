module Data.Time.Calendar.RomanCatholic.Season where

import Data.Time (Day, toGregorian)
import Data.Time.Calendar.RomanCatholic.Types

inRange :: Day -> Day -> Day -> Bool
inRange start end day = day >= start && day < end

getLiturgicalSeason :: YearDates -> Day -> LiturgicalSeason
getLiturgicalSeason dates day
  | inRange (adventStart dates) (christmasStart dates) day = Advent
  | inChristmasSeason dates day = Christmas
  | inRange (baptismDate dates) (lentStart dates) day = OrdinaryTime1
  | inRange (lentStart dates) (holyWeekStart dates) day = Lent
  | inRange (holyWeekStart dates) (triduumStart dates) day = HolyWeek
  | inRange (triduumStart dates) (easterDate dates) day = Triduum
  | inRange (easterDate dates) (ordinaryTime2Start dates) day = Easter
  | otherwise = OrdinaryTime2
  where
    inChristmasSeason ds d =
      let (_, month, _) = toGregorian d
      in (month == 12 && d >= christmasStart ds) ||
         (month == 1 && d < baptismDate ds)
