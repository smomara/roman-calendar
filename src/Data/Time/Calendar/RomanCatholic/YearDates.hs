module Data.Time.Calendar.RomanCatholic.YearDates where

import Data.Time (Day, DayOfWeek (Sunday), addDays, dayOfWeek, fromGregorian)
import Data.Time.Calendar.Easter (gregorianEaster)
import Data.Time.Calendar.RomanCatholic.Types (YearDates (..))

computeYearDates :: Integer -> YearDates
computeYearDates year =
  YearDates
    { adventStart = calculateAdventStart year
    , christmasStart = fromGregorian year 12 25
    , baptismDate =
        let epiphany = fromGregorian year 1 6
            daysToAdd = case dayOfWeek epiphany of
              Sunday -> 7
              _ -> 7 - fromEnum (dayOfWeek epiphany)
         in addDays (fromIntegral daysToAdd) epiphany
    , lentStart = addDays (-46) easter
    , holyWeekStart = addDays (-7) easter
    , triduumStart = addDays (-3) easter
    , easterDate = easter
    , pentecostDate = addDays 49 easter
    , ordinaryTime2Start = addDays 50 easter
    , christKingDate = calculateChristKingDate year
    }
 where
  easter = gregorianEaster year

  calculateAdventStart :: Integer -> Day
  calculateAdventStart y =
    let base = fromGregorian y 11 27
        daysToAdd = case dayOfWeek base of
          Sunday -> 0
          _ -> 7 - fromEnum (dayOfWeek base)
     in addDays (fromIntegral daysToAdd) base

  calculateChristKingDate :: Integer -> Day
  calculateChristKingDate = addDays (-7) . calculateAdventStart . (+ 1)
