module OpenSolid.Core.Interval
  ( endpoints
  , width
  , interpolated
  , median
  , contains
  , overlaps
  , singleton
  ) where


import OpenSolid.Core exposing (..)


singleton: Float -> Interval
singleton value =
  Interval value value


endpoints: Interval -> (Float, Float)
endpoints interval =
  (interval.lowerBound, interval.upperBound)


width: Interval -> Float
width interval =
  interval.upperBound - interval.lowerBound


interpolated: Float -> Interval -> Float
interpolated parameter interval =
  interval.lowerBound + parameter * width interval


median: Interval -> Float
median =
  interpolated 0.5


contains: Float -> Interval -> Bool
contains value interval =
  interval.lowerBound <= value && value <= interval.upperBound


overlaps: Interval -> Interval -> Bool
overlaps other interval =
  interval.lowerBound <= other.upperBound && interval.upperBound >= other.lowerBound
