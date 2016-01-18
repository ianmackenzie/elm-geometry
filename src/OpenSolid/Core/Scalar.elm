module OpenSolid.Core.Scalar
  ( isZero
  , isNotZero
  , hull
  ) where


import OpenSolid.Core exposing (..)


isZero: Float -> Float -> Bool
isZero tolerance value =
  -tolerance <= value && value <= tolerance


isNotZero: Float -> Float -> Bool
isNotZero tolerance value =
  value < -tolerance || tolerance < value


hull: Float -> Float -> Interval
hull firstValue secondValue =
  if firstValue <= secondValue then
    Interval firstValue secondValue
  else
    Interval secondValue firstValue
