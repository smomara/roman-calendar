module Data.Time.Calendar.RomanCatholic (
  Celebration (..),
  LiturgicalColor (..),
  LiturgicalSeason (..),
  CelebrationInfo (..),
  liturgicalSeason,
  celebrationInfo,
) where

import Data.Time (Day, DayOfWeek (..), addDays, dayOfWeek, fromGregorian, toGregorian)
import Data.Time.Calendar.Easter (gregorianEaster)

data Celebration
  = Solemnity String
  | Feast String
  | Memorial String
  | OptionalMemorial String
  | Commemoration String
  | Weekday String
  deriving (Show, Eq, Ord)

data LiturgicalColor = White | Red | Green | Violet | Rose
  deriving (Show, Eq)

data LiturgicalSeason
  = Advent
  | Christmas
  | OrdinaryTime1
  | Lent
  | HolyWeek
  | Triduum
  | Easter
  | OrdinaryTime2
  deriving (Show, Eq)

data CelebrationInfo = CelebrationInfo
  { celebrationType :: Celebration
  , celebrationColor :: LiturgicalColor
  , celebrationSeason :: LiturgicalSeason
  }
  deriving (Show, Eq)

data YearDates = YearDates
  { adventStart :: Day
  , christmasStart :: Day
  , baptismDate :: Day
  , lentStart :: Day
  , holyWeekStart :: Day
  , triduumStart :: Day
  , easterDate :: Day
  , pentecostDate :: Day
  , ordinaryTime2Start :: Day
  }

yearDates :: Integer -> YearDates
yearDates year =
  YearDates
    { adventStart = calculateAdventStart year
    , christmasStart = fromGregorian year 12 25
    , baptismDate = calculateBaptismDate year
    , lentStart = addDays (-46) easter
    , holyWeekStart = addDays (-7) easter
    , triduumStart = addDays (-3) easter
    , easterDate = easter
    , pentecostDate = addDays 49 easter
    , ordinaryTime2Start = addDays 1 (addDays 49 easter)
    }
 where
  easter = gregorianEaster year

calculateAdventStart :: Integer -> Day
calculateAdventStart year =
  let base = fromGregorian year 11 27
      daysToAdd = case dayOfWeek base of
        Sunday -> 0
        _ -> 7 - fromEnum (dayOfWeek base)
   in addDays (fromIntegral daysToAdd) base

calculateBaptismDate :: Integer -> Day
calculateBaptismDate year =
  let epiphany = fromGregorian year 1 6
      daysToAdd = case dayOfWeek epiphany of
        Sunday -> 7
        _ -> 7 - fromEnum (dayOfWeek epiphany)
   in addDays (fromIntegral daysToAdd) epiphany

liturgicalSeason :: Day -> LiturgicalSeason
liturgicalSeason day
  | day `inRange` (adventStart dates, christmasStart dates) = Advent
  | (month == 12 && day >= christmasStart dates) || (month == 1 && day <= baptismDate (yearDates nextYear)) = Christmas
  | day `inRange` (baptismDate dates, lentStart dates) = OrdinaryTime1
  | day `inRange` (lentStart dates, holyWeekStart dates) = Lent
  | day `inRange` (holyWeekStart dates, triduumStart dates) = HolyWeek
  | day `inRange` (triduumStart dates, easterDate dates) = Triduum
  | day `inRange` (easterDate dates, ordinaryTime2Start dates) = Easter
  | otherwise = OrdinaryTime2
 where
  (currentYear, month, _) = toGregorian day
  dates = yearDates currentYear
  nextYear = currentYear + 1
  inRange d (start, end) = d >= start && d < end

celebrationInfo :: Day -> CelebrationInfo
celebrationInfo = undefined -- TODO
