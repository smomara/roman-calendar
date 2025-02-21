module Data.Time.Calendar.RomanCatholic (
  module Data.Time.Calendar.RomanCatholic.Types,
  getCelebrationInfo,
) where

import Data.List (find, partition, sortOn)
import Data.Time (Day, toGregorian)
import Data.Time.Calendar.RomanCatholic.Celebrations.Sanctoral (sanctoralCelebrations)
import Data.Time.Calendar.RomanCatholic.Celebrations.Temporal (temporalCelebrations)
import Data.Time.Calendar.RomanCatholic.Season (getLiturgicalSeason)
import Data.Time.Calendar.RomanCatholic.Types
import Data.Time.Calendar.RomanCatholic.YearDates (computeYearDates)

getCelebrationInfo :: Day -> Maybe CelebrationInfo
getCelebrationInfo day =
  let season = getLiturgicalSeason yearDates day
      allRules = sortOn precedence $ temporalCelebrations ++ sanctoralCelebrations
      (optionalRules, otherRules) = partition isOptionalMemorial allRules
      primaryRule = find (matchesDayForYear day) otherRules
      optionalCelebrations = case primaryRule of
        Just rule -> filter ((< precedence rule) . precedence) $ filter (matchesDayForYear day) optionalRules
        Nothing -> filter (matchesDayForYear day) optionalRules
   in case primaryRule of
        Just rule ->
          Just $
            CelebrationInfo
              (celebration rule yearDates day)
              (color rule)
              season
              (map (getCelebrationWithColor day) optionalCelebrations)
        Nothing -> Nothing
 where
  yearDates = let (year, _, _) = toGregorian day in computeYearDates year
  isOptionalMemorial rule = case celebration rule yearDates day of
    OptionalMemorial _ -> True
    _ -> False
  matchesDayForYear d rule = matchesDay rule yearDates d
  getCelebrationWithColor d rule = (celebration rule yearDates d, color rule)
