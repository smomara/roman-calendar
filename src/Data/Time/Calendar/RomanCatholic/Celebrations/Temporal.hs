module Data.Time.Calendar.RomanCatholic.Celebrations.Temporal where

import Data.Time (Day, DayOfWeek (Sunday), addDays, dayOfWeek, diffDays, toGregorian)
import Data.Time.Calendar.RomanCatholic.Season (getLiturgicalSeason)
import Data.Time.Calendar.RomanCatholic.Types

weekNumber :: YearDates -> Day -> LiturgicalSeason -> Int
weekNumber dates day season = 1 + fromIntegral (diffDays day startDate) `div` 7
 where
  startDate = case season of
    Advent -> adventStart dates
    Christmas -> christmasStart dates
    Lent -> lentStart dates
    HolyWeek -> holyWeekStart dates
    Triduum -> triduumStart dates
    Easter -> easterDate dates
    OrdinaryTime1 -> baptismDate dates
    OrdinaryTime2 -> ordinaryTime2Start dates

temporalCelebrations :: [CelebrationRule]
temporalCelebrations =
  concat
    [ adventCelebrations
    , christmasCelebrations
    , ordinaryTime1Celebrations
    , lentCelebrations
    , holyWeekCelebrations
    , triduumCelebrations
    , easterCelebrations
    , ordinaryTime2Celebrations
    ]

adventCelebrations :: [CelebrationRule]
adventCelebrations =
  [ sundayRule Advent (Solemnity . showAdventSunday) Violet 2
  , weekdayRule Advent Violet 13
  ]

christmasCelebrations :: [CelebrationRule]
christmasCelebrations =
  [ dateBasedRule christmasStart 0 "The Nativity of the Lord (Christmas)" Solemnity White 2
  , dateBasedRule christmasStart 1 "Second Day in the Octave of Christmas" Solemnity White 9
  , dateBasedRule christmasStart 2 "Third Day in the Octave of Christmas" Solemnity White 9
  , dateBasedRule christmasStart 3 "Fourth Day in the Octave of Christmas" Solemnity White 9
  , dateBasedRule christmasStart 4 "Fifth Day in the Octave of Christmas" Solemnity White 9
  , dateBasedRule christmasStart 5 "Sixth Day in the Octave of Christmas" Solemnity White 9
  , dateBasedRule christmasStart 6 "Seventh Day in the Octave of Christmas" Solemnity White 9
  , sundayRule Christmas (const $ Feast "Holy Family") White 5
  , fixedDateRule 1 6 "Epiphany" Solemnity White 3
  , weekdayRule Christmas White 13
  , dateBasedRule baptismDate 0 "Baptism of the Lord" Feast White 5
  ]

ordinaryTime1Celebrations :: [CelebrationRule]
ordinaryTime1Celebrations =
  [ sundayRule OrdinaryTime1 (Solemnity . showOrdinarySunday) Green 6
  , weekdayRule OrdinaryTime1 Green 13
  ]

lentCelebrations :: [CelebrationRule]
lentCelebrations =
  [ dateBasedRule lentStart 0 "Ash Wednesday" Solemnity Violet 2
  , dateBasedRule lentStart 1 "Thursday after Ash Wednesday" Weekday Violet 9
  , dateBasedRule lentStart 2 "Friday after Ash Wednesday" Weekday Violet 9
  , dateBasedRule lentStart 3 "Saturday after Ash Wednesday" Weekday Violet 9
  , sundayRule Lent (Solemnity . showLentSunday) Violet 2
  , weekdayRule Lent Violet 13
  ]

holyWeekCelebrations :: [CelebrationRule]
holyWeekCelebrations =
  [ sundayRule HolyWeek (const $ Solemnity "Palm Sunday") Red 2
  , weekdayRule HolyWeek Violet 2
  ]

triduumCelebrations :: [CelebrationRule]
triduumCelebrations =
  [ dateBasedRule triduumStart 0 "Holy Thursday" Solemnity White 1
  , dateBasedRule triduumStart 1 "Good Friday" Solemnity Red 1
  , dateBasedRule triduumStart 2 "Holy Saturday" Solemnity Violet 1
  , dateBasedRule triduumStart 2 "Easter Vigil" Solemnity White 1
  ]

easterCelebrations :: [CelebrationRule]
easterCelebrations =
  [ dateBasedRule easterDate 0 "Easter Sunday" Solemnity White 2
  , dateBasedRule easterDate 1 "Monday in the Octave of Easter" Weekday White 2
  , dateBasedRule easterDate 2 "Tuesday in the Octave of Easter" Weekday White 2
  , dateBasedRule easterDate 3 "Wednesday in the Octave of Easter" Weekday White 2
  , dateBasedRule easterDate 4 "Thursday in the Octave of Easter" Weekday White 2
  , dateBasedRule easterDate 5 "Friday in the Octave of Easter" Weekday White 2
  , dateBasedRule easterDate 6 "Saturday in the Octave of Easter" Weekday White 2
  , dateBasedRule easterDate 7 "Second Sunday of Easter (Divine Mercy Sunday)" Solemnity White 2
  , sundayRule Easter (Solemnity . showEasterSunday) White 2
  , weekdayRule Easter White 13
  , dateBasedRule easterDate 40 "Ascension" Solemnity White 3
  , dateBasedRule easterDate 49 "Pentecost Sunday" Solemnity Red 3
  , dateBasedRule easterDate 50 "The Blessed Virgin Mary, Mother of the Church" Memorial White 10
  ]

ordinaryTime2Celebrations :: [CelebrationRule]
ordinaryTime2Celebrations =
  [ dateBasedRule easterDate 56 "Trinity Sunday" Solemnity White 3
  , dateBasedRule easterDate 60 "Corpus Christi" Solemnity White 3
  , dateBasedRule easterDate 61 "Sacred Heart of Jesus" Solemnity White 3
  , dateBasedRule easterDate 62 "Immaculate Heart of Mary" OptionalMemorial White 10
  , sundayRule OrdinaryTime2 (Solemnity . showOrdinarySunday) Green 6
  , weekdayRule OrdinaryTime2 Green 13
  , dateBasedRule christKingDate 0 "Christ The King" Solemnity White 3
  ]

fixedDateRule :: Int -> Int -> String -> (String -> Celebration) -> LiturgicalColor -> Int -> CelebrationRule
fixedDateRule month day name celebType col rank =
  CelebrationRule
    { matchesDay = \(_, d) ->
        let (_, m, dm) = toGregorian d in m == month && dm == day
    , celebration = \_ _ -> celebType name
    , color = col
    , precedence = rank
    }

dateBasedRule :: (YearDates -> Day) -> Int -> String -> (String -> Celebration) -> LiturgicalColor -> Int -> CelebrationRule
dateBasedRule dateFunc offset name celebType col rank =
  CelebrationRule
    { matchesDay = \(dates, d) -> d == addDays (fromIntegral offset) (dateFunc dates)
    , celebration = \_ _ -> celebType name
    , color = col
    , precedence = rank
    }

sundayRule :: LiturgicalSeason -> (Int -> Celebration) -> LiturgicalColor -> Int -> CelebrationRule
sundayRule season mkCeleb col rank =
  CelebrationRule
    { matchesDay = \(dates, d) -> dayOfWeek d == Sunday && getLiturgicalSeason dates d == season
    , celebration = \dates d -> mkCeleb $ weekNumber dates d season
    , color = col
    , precedence = rank
    }

weekdayRule :: LiturgicalSeason -> LiturgicalColor -> Int -> CelebrationRule
weekdayRule season col rank =
  CelebrationRule
    { matchesDay = \(dates, d) -> getLiturgicalSeason dates d == season && dayOfWeek d /= Sunday
    , celebration = \_ d -> Weekday $ show (dayOfWeek d) ++ " in " ++ showSeason season
    , color = col
    , precedence = rank
    }

showSeason :: LiturgicalSeason -> String
showSeason OrdinaryTime1 = "Ordinary Time"
showSeason OrdinaryTime2 = "Ordinary Time"
showSeason HolyWeek = "Holy Week"
showSeason s = show s

showAdventSunday :: Int -> String
showAdventSunday n = "Sunday " ++ show n ++ " of Advent"

showLentSunday :: Int -> String
showLentSunday n = "Sunday " ++ show n ++ " of Lent"

showEasterSunday :: Int -> String
showEasterSunday n = "Sunday " ++ show n ++ " of Easter"

showOrdinarySunday :: Int -> String
showOrdinarySunday n = "Sunday " ++ show n ++ " in Ordinary Time"
