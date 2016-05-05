module OpenSolid.Core.Interval
  ( empty
  , whole
  , singleton
  , hull
  , hullOf
  , fromEndpoints
  , endpoints
  , isEmpty
  , isWhole
  , isFinite
  , width
  , interpolated
  , midpoint
  , contains
  , isInside
  , overlaps
  ) where


import OpenSolid.Core exposing (..)


positiveInfinity =
  1 / 0


negativeInfinity =
  -1 / 0


notANumber =
  0 / 0


empty: Interval
empty =
  Interval notANumber notANumber


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
interpolated parameter (Interval lowerBound upperBound as interval) =
  if isFinite interval then
    -- Fast path
    lowerBound + parameter * width interval
  else if isNaN parameter || isEmpty interval then
    notANumber
  else if upperBound == negativeInfinity then
    -- Interval is singleton negative infinity
    if parameter <= 1 then negativeInfinity else notANumber
  else if lowerBound == positiveInfinity then
    -- Interval is singleton positive infinity
    if parameter >= 0 then positiveInfinity else notANumber
  else if lowerBound > negativeInfinity then
    -- Interval has finite lower bound, infinite upper bound
    if parameter < 0 then
      negativeInfinity
    else if parameter > 0 then
      positiveInfinity
    else
      lowerBound
  else if upperBound < positiveInfinity then
    -- Interval has finite upper bound, infinite lower bound
    if parameter < 1 then
      negativeInfinity
    else if parameter > 1 then
      positiveInfinity
    else
      upperBound
  else
    -- Interval is the whole interval
    if parameter <= 0 then
      negativeInfinity
    else if parameter >= 1 then
      positiveInfinity
    else
      notANumber


midpoint: Interval -> Float
midpoint =
  interpolated 0.5


contains: Float -> Interval -> Bool
contains value (Interval lowerBound upperBound) =
  lowerBound <= value && value <= upperBound


isInside: Interval -> Interval -> Bool
isInside (Interval otherLower otherUpper) (Interval lowerBound upperBound) =
  otherLower <= lowerBound && upperBound <= otherUpper


overlaps: Interval -> Interval -> Bool
overlaps (Interval firstLower firstUpper) (Interval secondLower secondUpper) =
  firstLower <= secondUpper && secondLower <= firstUpper
