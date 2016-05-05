module OpenSolid.Core.Scalar
  ( notANumber
  , positiveInfinity
  , negativeInfinity
  , hull
  , hullOf
  , isZeroWithin
  ) where


{-| Various convenience values and functions for dealing with `Float` values.

# Values

@docs positiveInfinity, negativeInfinity, notANumber

# Functions

@docs isZeroWithin, hull, hullOf

-}


import OpenSolid.Core exposing (..)


{-| The constant positive infinity value.

    isInfinite Scalar.positiveInfinity == True
-}
positiveInfinity: Float
positiveInfinity =
  1 / 0


{-| The constant negative infinity value.

    isInfinite Scalar.negativeInfinity == True
-}
negativeInfinity: Float
negativeInfinity =
  -1 / 0


{-| The constant NaN value.

    isNaN Scalar.notANumber == True
-}
notANumber: Float
notANumber =
  0 / 0


{-| Check if a value is equal to zero, to within a given tolerance.

    Scalar.isZeroWithin 1e-3 0.0005 == True
    Scalar.isZeroWithin 1e-6 0.0005 == False

Note that the tolerance is given first, to allow convenient partial application:

    List.filter (Scalar.isZeroWithin 1e-3) [0.0005, 0.005, 0.05, 0.0009] == [0.0005, 0.0009]

-}
isZeroWithin: Float -> Float -> Bool
isZeroWithin tolerance value =
  -tolerance <= value && value <= tolerance


{-| Construct an `Interval` containing the two given values.

    Scalar.hull 2 3 == Interval 2 3
    Scalar.hull 4 0 == Interval 0 4

-}
hull: Float -> Float -> Interval
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
hullOf: List Float -> Interval
hullOf values =
  let
    minValue = Maybe.withDefault notANumber (List.minimum values)
    maxValue = Maybe.withDefault notANumber (List.maximum values)
  in
    Interval minValue maxValue
