module OpenSolid.Core.Interval
  ( empty
  , whole
  , singleton
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
  , overlaps
  , hull
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Scalar as Scalar


empty: Interval
empty =
  Interval Scalar.notANumber Scalar.notANumber


whole: Interval
whole =
  Interval Scalar.negativeInfinity Scalar.positiveInfinity


singleton: Float -> Interval
singleton value =
  Interval value value


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
  lowerBound == Scalar.negativeInfinity && upperBound == Scalar.positiveInfinity


isFinite: Interval -> Bool
isFinite (Interval lowerBound upperBound) =
  lowerBound > Scalar.negativeInfinity && upperBound < Scalar.positiveInfinity


width: Interval -> Float
width (Interval lowerBound upperBound) =
  upperBound - lowerBound


interpolated: Float -> Interval -> Float
interpolated parameter (Interval lowerBound upperBound as interval) =
  if isFinite interval then
    -- Fast path
    lowerBound + parameter * width interval
  else if isNaN parameter || isEmpty interval then
    Scalar.notANumber
  else if upperBound == Scalar.negativeInfinity then
    -- Interval is singleton negative infinity
    if parameter <= 1 then Scalar.negativeInfinity else Scalar.notANumber
  else if lowerBound == Scalar.positiveInfinity then
    -- Interval is singleton positive infinity
    if parameter >= 0 then Scalar.positiveInfinity else Scalar.notANumber
  else if lowerBound > Scalar.negativeInfinity then
    -- Interval has finite lower bound, infinite upper bound
    if parameter < 0 then
      Scalar.negativeInfinity
    else if parameter > 0 then
      Scalar.positiveInfinity
    else
      lowerBound
  else if upperBound < Scalar.positiveInfinity then
    -- Interval has finite upper bound, infinite lower bound
    if parameter < 1 then
      Scalar.negativeInfinity
    else if parameter > 1 then
      Scalar.positiveInfinity
    else
      upperBound
  else
    -- Interval is the whole interval
    if parameter <= 0 then
      Scalar.negativeInfinity
    else if parameter >= 1 then
      Scalar.positiveInfinity
    else
      Scalar.notANumber


midpoint: Interval -> Float
midpoint =
  interpolated 0.5


contains: Float -> Interval -> Bool
contains value interval =
  Scalar.isInside interval value


overlaps: Interval -> Interval -> Bool
overlaps (Interval firstLower firstUpper) (Interval secondLower secondUpper) =
  firstLower <= secondUpper && secondLower <= firstUpper


hull: Interval -> Interval -> Interval
hull (Interval firstLower firstUpper as first) (Interval secondLower secondUpper as second) =
  if isEmpty first then
    second
  else if isEmpty second then
    first
  else
    Interval (min secondLower firstLower) (max secondUpper firstUpper)
