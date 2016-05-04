module OpenSolid.Core.Scalar
  ( notANumber
  , positiveInfinity
  , negativeInfinity
  , hull
  , hullOf
  , isZeroWithin
  ) where


import OpenSolid.Core exposing (..)


notANumber: Float
notANumber =
  0 / 0


positiveInfinity: Float
positiveInfinity =
  1 / 0


negativeInfinity: Float
negativeInfinity =
  -1 / 0


hull: Float -> Float -> Interval
hull firstValue secondValue =
  if firstValue <= secondValue then
    Interval firstValue secondValue
  else
    Interval secondValue firstValue


hullOf: List Float -> Interval
hullOf values =
  let
    minValue = Maybe.withDefault notANumber (List.minimum values)
    maxValue = Maybe.withDefault notANumber (List.maximum values)
  in
    Interval minValue maxValue


isZeroWithin: Float -> Float -> Bool
isZeroWithin tolerance value =
  -tolerance <= value && value <= tolerance
