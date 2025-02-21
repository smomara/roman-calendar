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
  [ -- Advent Season
    sundayRule Advent (Solemnity . showAdventSunday) Violet 2
  , weekdayRule Advent Violet 13
  , -- Christmas Season
    christmasBasedRule 0 "The Nativity of the Lord (Christmas)" White 2
  , christmasBasedRule 1 "Second Day in the Octave of Christmas" White 9
  , christmasBasedRule 2 "Third Day in the Octave of Christmas" White 9
  , christmasBasedRule 2 "Fourth Day in the Octave of Christmas" White 9
  , christmasBasedRule 2 "Fifth Day in the Octave of Christmas" White 9
  , christmasBasedRule 2 "Sixth Day in the Octave of Christmas" White 9
  , christmasBasedRule 2 "Seventh Day in the Octave of Christmas" White 9
  , sundayRule Christmas (const $ Feast "Holy Family") White 5
  , fixedDateRule 1 6 "Epiphany" White 3
  , weekdayRule Christmas White 13
  , baptismBasedRule 0 "Baptism of the Lord" White 5
  , -- Ordinary Time (First Part)
    sundayRule OrdinaryTime1 (Solemnity . showOrdinarySunday) Green 6
  , weekdayRule OrdinaryTime1 Green 13
  , -- Lent Season
    lentBasedRule 0 "Ash Wednesday" Violet 2
  , lentBasedRule 1 "Thursday after Ash Wednesday" Violet 9
  , lentBasedRule 2 "Friday after Ash Wednesday" Violet 9
  , lentBasedRule 3 "Saturday after Ash Wednesday" Violet 9
  , sundayRule Lent (Solemnity . showLentSunday) Violet 2
  , weekdayRule Lent Violet 13
  , -- Holy Week Season
    sundayRule HolyWeek (const $ Solemnity "Palm Sunday") Red 2
  , weekdayRule HolyWeek Violet 2
  , -- Triduum Season
    triduumBasedRule 0 "Holy Thursday" White 1
  , triduumBasedRule 1 "Good Friday" Red 1
  , triduumBasedRule 2 "Holy Saturday" Violet 1
  , triduumBasedRule 2 "Easter Vigil" White 1
  , -- Easter Season
    easterBasedRule 0 "Easter Sunday" Solemnity White 2
  , easterBasedRule 1 "Monday in the Octave of Easter" Weekday White 2
  , easterBasedRule 2 "Tuesday in the Octave of Easter" Weekday White 2
  , easterBasedRule 3 "Wednesday in the Octave of Easter" Weekday White 2
  , easterBasedRule 4 "Thursday in the Octave of Easter" Weekday White 2
  , easterBasedRule 5 "Friday in the Octave of Easter" Weekday White 2
  , easterBasedRule 6 "Saturday in the Octave of Easter" Weekday White 2
  , easterBasedRule 7 "Second Sunday of Easter (Divine Mercy Sunday)" Solemnity White 2
  , sundayRule Easter (Solemnity . showEasterSunday) White 2
  , weekdayRule Easter White 13
  , easterBasedRule 40 "Ascension" Solemnity White 3
  , easterBasedRule 49 "Pentecost Sunday" Solemnity Red 3
  , easterBasedRule 50 "The Blessed Virgin Mary" Solemnity White 3
  , -- Ordinary Time (Second Part)
    easterBasedRule 56 "Trinity Sunday" Solemnity White 3
  , easterBasedRule 60 "Corpus Christi" Solemnity White 3
  , easterBasedRule 61 "Sacred Heart of Jesus" Solemnity White 3
  , easterBasedRule 62 "Immaculate Heart of Mary" OptionalMemorial White 10
  , sundayRule OrdinaryTime2 (Solemnity . showOrdinarySunday) Green 6
  , weekdayRule OrdinaryTime2 Green 13
  , christKingBasedRule 0 "Christ The King" White 3
  ]
 where
  christmasBasedRule offset name =
    CelebrationRule
      (\yd d -> d == addDays offset (christmasStart yd))
      (\_ _ -> Solemnity name)

  baptismBasedRule offset name =
    CelebrationRule
      (\yd d -> d == addDays offset (baptismDate yd))
      (\_ _ -> Feast name)

  lentBasedRule offset name =
    CelebrationRule
      (\yd d -> d == addDays offset (lentStart yd))
      (\_ _ -> Solemnity name)

  triduumBasedRule offset name =
    CelebrationRule
      (\yd d -> d == addDays offset (triduumStart yd))
      (\_ _ -> Solemnity name)

  easterBasedRule offset name cType =
    CelebrationRule
      (\yd d -> d == addDays offset (easterDate yd))
      (\_ _ -> cType name)

  christKingBasedRule offset name =
    CelebrationRule
      (\yd d -> d == addDays offset (christKingDate yd))
      (\_ _ -> Solemnity name)

  sundayRule season mkCeleb =
    CelebrationRule
      (\yd d -> dayOfWeek d == Sunday && getLiturgicalSeason yd d == season)
      (\yd d -> mkCeleb $ weekNumber yd d season)

  fixedDateRule month day name =
    CelebrationRule
      (\_ d -> let (_, m, dm) = toGregorian d in m == month && dm == day)
      (\_ _ -> Solemnity name)

  showSeason :: LiturgicalSeason -> String
  showSeason OrdinaryTime1 = "Ordinary Time"
  showSeason OrdinaryTime2 = "Ordinary Time"
  showSeason HolyWeek = "Holy Week"
  showSeason s = show s

  weekdayRule season =
    CelebrationRule
      (\yd d -> getLiturgicalSeason yd d == season && dayOfWeek d /= Sunday)
      (\_ d -> Weekday $ show (dayOfWeek d) ++ " in " ++ showSeason season)

  showAdventSunday n = "Sunday " ++ show n ++ " of Advent"
  showLentSunday n = "Sunday " ++ show n ++ " of Lent"
  showEasterSunday n = "Sunday " ++ show n ++ " of Easter"
  showOrdinarySunday n = "Sunday " ++ show n ++ " in Ordinary Time"
