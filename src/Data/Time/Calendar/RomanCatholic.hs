module Data.Time.Calendar.RomanCatholic (
  module Data.Time.Calendar.RomanCatholic.Types,
  getCelebrationInfo,
) where

import Data.List (find, sortOn)
import Data.Time (Day, toGregorian)
import Data.Time.Calendar.RomanCatholic.Celebrations.Sanctoral (sanctoralCelebrations)
import Data.Time.Calendar.RomanCatholic.Celebrations.Temporal (temporalCelebrations)
import Data.Time.Calendar.RomanCatholic.Season (getLiturgicalSeason)
import Data.Time.Calendar.RomanCatholic.Types
import Data.Time.Calendar.RomanCatholic.YearDates (computeYearDates)

getCelebrationInfo :: Day -> CelebrationInfo
getCelebrationInfo day =
  let yearDates = computeYearDates ((\(y, _, _) -> y) $ toGregorian day)
      season = getLiturgicalSeason yearDates day
      allRules = sortOn precedence (temporalCelebrations ++ sanctoralCelebrations)
   in case find (\rule -> matchesDay rule yearDates day) allRules of
        Just rule ->
          CelebrationInfo
            (celebration rule yearDates day)
            (color rule)
            season
        Nothing ->
          CelebrationInfo
            (Weekday "Weekday")
            (defaultColor season)
            season
 where
  defaultColor season = case season of
    Advent -> Violet
    Lent -> Violet
    Triduum -> Violet
    Easter -> White
    Christmas -> White
    _ -> Green
