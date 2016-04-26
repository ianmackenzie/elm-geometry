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


import List
import OpenSolid.Core exposing (..)
import OpenSolid.Core.Scalar as Scalar


empty: Interval
empty =
  Interval Scalar.nan Scalar.nan


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
fromEndpoints (lower, upper) =
  Interval lower upper


endpoints: Interval -> (Float, Float)
endpoints interval =
  (interval.lowerBound, interval.upperBound)


isEmpty: Interval -> Bool
isEmpty interval =
  isNaN interval.lowerBound && isNaN interval.upperBound


isWhole: Interval -> Bool
isWhole interval =
  interval.lowerBound == Scalar.negativeInfinity && interval.upperBound == Scalar.positiveInfinity


isFinite: Interval -> Bool
isFinite interval =
  interval.lowerBound > Scalar.negativeInfinity && interval.upperBound < Scalar.positiveInfinity


width: Interval -> Float
width interval =
  interval.upperBound - interval.lowerBound


interpolated: Float -> Interval -> Float
interpolated parameter interval =
  if isFinite interval then
    -- Fast path
    interval.lowerBound + parameter * width interval
  else if isNaN parameter || isEmpty interval then
    Scalar.nan
  else if interval.upperBound == Scalar.negativeInfinity then
    -- Interval is singleton negative infinity
    if parameter <= 1 then Scalar.negativeInfinity else Scalar.nan
  else if interval.lowerBound == Scalar.positiveInfinity then
    -- Interval is singleton positive infinity
    if parameter >= 0 then Scalar.positiveInfinity else Scalar.nan
  else if interval.lowerBound > Scalar.negativeInfinity then
    -- Interval has finite lower bound, infinite upper bound
    if parameter < 0 then
      Scalar.negativeInfinity
    else if parameter > 0 then
      Scalar.positiveInfinity
    else
      interval.lowerBound
  else if interval.upperBound < Scalar.positiveInfinity then
    -- Interval has finite upper bound, infinite lower bound
    if parameter < 1 then
      Scalar.negativeInfinity
    else if parameter > 1 then
      Scalar.positiveInfinity
    else
      interval.upperBound
  else
    -- Interval is the whole interval
    if parameter <= 0 then
      Scalar.negativeInfinity
    else if parameter >= 1 then
      Scalar.positiveInfinity
    else
      Scalar.nan


midpoint: Interval -> Float
midpoint =
  interpolated 0.5


contains: Float -> Interval -> Bool
contains value interval =
  interval.lowerBound <= value && value <= interval.upperBound


overlaps: Interval -> Interval -> Bool
overlaps other interval =
  interval.lowerBound <= other.upperBound && interval.upperBound >= other.lowerBound


hull: Interval -> Interval -> Interval
hull other interval =
  if isEmpty other then
    interval
  else if isEmpty interval then
    other
  else
    Interval (min interval.lowerBound other.lowerBound) (max interval.upperBound other.upperBound)
