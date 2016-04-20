module OpenSolid.Core.Interval
  ( singleton
  , hullOf
  , width
  , interpolated
  , midpoint
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)


singleton: Float -> Interval
singleton value =
  Interval value value


hullOf: Float -> Float -> Interval
hullOf firstValue secondValue =
  if firstValue <= secondValue then
    Interval firstValue secondValue
  else
    Interval secondValue firstValue


width: Interval -> Float
width interval =
  interval.upperBound - interval.lowerBound


interpolated: Float -> Interval -> Float
interpolated parameter interval =
  interval.lowerBound + parameter * width interval


midpoint: Interval -> Float
midpoint =
  interpolated 0.5


contains: Float -> Interval -> Bool
contains value interval =
  interval.lowerBound <= value && value <= interval.upperBound


overlaps: Interval -> Interval -> Bool
overlaps other interval =
  interval.lowerBound <= other.upperBound && interval.upperBound >= other.lowerBound
