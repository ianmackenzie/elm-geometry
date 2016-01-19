module OpenSolid.Core.Interval
  ( lowerBound
  , upperBound
  , endpoints
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


lowerBound: Interval -> Float
lowerBound (Interval lower upper) =
  lower


upperBound: Interval -> Float
upperBound (Interval lower upper) =
  upper


endpoints: Interval -> (Float, Float)
endpoints (Interval lower upper) =
  (lower, upper)


width: Interval -> Float
width (Interval lower upper) =
  upper - lower


interpolated: Float -> Interval -> Float
interpolated parameter (Interval lower upper) =
  lower + parameter * (upper - lower)


median: Interval -> Float
median =
  interpolated 0.5


contains: Float -> Interval -> Bool
contains value (Interval lower upper) =
  lower <= value && value <= upper


overlaps: Interval -> Interval -> Bool
overlaps (Interval otherLower otherUpper) (Interval lower upper) =
  lower <= otherUpper && upper >= otherLower
