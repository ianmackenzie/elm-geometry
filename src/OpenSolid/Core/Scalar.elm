module OpenSolid.Core.Scalar
  ( nan
  , positiveInfinity
  , negativeInfinity
  , hullOf
  , isZeroWithin
  , hull
  ) where


import List
import Maybe
import OpenSolid.Core exposing (..)


nan: Float
nan =
  0 / 0


positiveInfinity: Float
positiveInfinity =
  1 / 0


negativeInfinity: Float
negativeInfinity =
  -1 / 0


hullOf: List Float -> Interval
hullOf values =
  let
    minValue = Maybe.withDefault nan (List.minimum values)
    maxValue = Maybe.withDefault nan (List.maximum values)
  in
    Interval minValue maxValue


isZeroWithin: Float -> Float -> Bool
isZeroWithin tolerance value =
  -tolerance <= value && value <= tolerance


hull: Float -> Float -> Interval
hull firstValue secondValue =
  if firstValue <= secondValue then
    Interval firstValue secondValue
  else
    Interval secondValue firstValue
