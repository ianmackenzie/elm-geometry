module OpenSolid.Core.Interval
  ( empty
  , whole
  , singleton
  , hull
  , hullOf
  , intersection
  , fromEndpoints
  , endpoints
  , isEmpty
  , isWhole
  , isFinite
  , width
  , interpolated
  , midpoint
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)


positiveInfinity =
  1 / 0


negativeInfinity =
  -1 / 0


empty: Interval
empty =
  Interval (0 / 0) (0 / 0)


whole: Interval
whole =
  Interval negativeInfinity positiveInfinity


singleton: Float -> Interval
singleton value =
  Interval value value


hull: Interval -> Interval -> Interval
hull (Interval firstLower firstUpper as first) (Interval secondLower secondUpper as second) =
  if isEmpty first then
    second
  else if isEmpty second then
    first
  else
    Interval (min firstLower secondLower) (max firstUpper secondUpper)


hullOf: List Interval -> Interval
hullOf =
  List.foldl hull empty


intersection: Interval -> Interval -> Interval
intersection first second =
  if isEmpty first || isEmpty second then
    empty
  else
    let
      (Interval firstLower firstUpper) = first
      (Interval secondLower secondUpper) = second
      lowerBound = max firstLower secondLower
      upperBound = min firstUpper secondUpper
    in
      if lowerBound <= upperBound then Interval lowerBound upperBound else empty


fromEndpoints: (Float, Float) -> Interval
fromEndpoints (lowerBound, upperBound) =
  Interval lowerBound upperBound


endpoints: Interval -> (Float, Float)
endpoints (Interval lowerBound upperBound) =
  (lowerBound, upperBound)


isEmpty: Interval -> Bool
isEmpty (Interval lowerBound upperBound) =
  isNaN lowerBound && isNaN upperBound


isWhole: Interval -> Bool
isWhole (Interval lowerBound upperBound) =
  lowerBound == negativeInfinity && upperBound == positiveInfinity


isFinite: Interval -> Bool
isFinite (Interval lowerBound upperBound) =
  negativeInfinity < lowerBound && upperBound < positiveInfinity


width: Interval -> Float
width (Interval lowerBound upperBound) =
  upperBound - lowerBound


interpolated: Float -> Interval -> Float
interpolated parameter (Interval lowerBound upperBound) =
  lowerBound + parameter * (upperBound - lowerBound)


midpoint: Interval -> Float
midpoint =
  interpolated 0.5


contains: Interval -> Interval -> Bool
contains (Interval otherLower otherUpper) (Interval lowerBound upperBound) =
  lowerBound <= otherLower && otherUpper <= upperBound


overlaps: Interval -> Interval -> Bool
overlaps (Interval otherLower otherUpper) (Interval lowerBound upperBound) =
  lowerBound <= otherUpper && upperBound >= otherLower
