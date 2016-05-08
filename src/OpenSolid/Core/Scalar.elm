module OpenSolid.Core.Scalar (isZeroWithin, isInside, hull, hullOf) where

{-| Various convenience functions for dealing with `Float` values.

@docs isZeroWithin, hull, hullOf
-}

import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


{-| Check if a value is equal to zero, to within a given tolerance.

    Scalar.isZeroWithin 1e-3 0.0005 == True
    Scalar.isZeroWithin 1e-6 0.0005 == False

Note that the tolerance is given first, to allow convenient partial application:

    List.filter (Scalar.isZeroWithin 1e-3) [0.0005, 0.005, 0.05, 0.0009] == [0.0005, 0.0009]
-}
isZeroWithin : Float -> Float -> Bool
isZeroWithin tolerance value =
  -tolerance <= value && value <= tolerance


{-| Check if a value is in a given interval.

    Scalar.isInside (Interval 0 2) 1 == True
    Scalar.isInside (Interval 0 2) 2 == True
    Scalar.isInside (Interval 0 2) 3 == False
    List.filter (Scalar.isInside (Interval 2.1 5.9)) [0..10] == [3, 4, 5]
-}
isInside : Interval -> Float -> Bool
isInside (Interval lowerBound upperBound) value =
  lowerBound <= value && value <= upperBound


{-| Construct an `Interval` containing the two given values.

    Scalar.hull 2 3 == Interval 2 3
    Scalar.hull 4 0 == Interval 0 4
-}
hull : Float -> Float -> Interval
hull firstValue secondValue =
  if firstValue <= secondValue then
    Interval firstValue secondValue
  else
    Interval secondValue firstValue


{-| Construct an `Interval` containing all of the given values.

    Scalar.hullOf [3, 1, 2, 5, 4] == Interval 1 5
    Scalar.hullOf [2] == Interval 2 2
    Interval.isEmpty (Scalar.hullOf []) == True
-}
hullOf : List Float -> Interval
hullOf values =
  Maybe.withDefault Interval.empty (Maybe.map2 Interval (List.minimum values) (List.maximum values))
