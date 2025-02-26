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
getCelebrationInfo day = do
  let season = getLiturgicalSeason yearDates day
      allRules = sortOn precedence $ temporalCelebrations ++ sanctoralCelebrations
      (optionalRules, otherRules) = partition isOptionalMemorial allRules

  primaryRule <- find (`matchesDay` (yearDates, day)) otherRules

  let optionalCelebrations =
        filter
          ( \r ->
              matchesDay r (yearDates, day)
                && precedence r < precedence primaryRule
          )
          optionalRules

  return $
    CelebrationInfo
      { celebrationType = celebration primaryRule yearDates day
      , celebrationColor = color primaryRule
      , celebrationSeason = season
      , optionalMemorials = map (\r -> (celebration r yearDates day, color r)) optionalCelebrations
      }
 where
  (year, _, _) = toGregorian day
  yearDates = computeYearDates year
  isOptionalMemorial rule =
    case celebration rule yearDates day of
      OptionalMemorial _ -> True
      _ -> False
