module Data.Time.Calendar.RomanCatholic.Types where

import Data.Time (Day)

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
  , christKingDate :: Day
  }
  deriving (Show)

data CelebrationRule = CelebrationRule
  { matchesDay :: YearDates -> Day -> Bool
  , celebration :: YearDates -> Day -> Celebration
  , color :: LiturgicalColor
  , precedence :: Int
  }
