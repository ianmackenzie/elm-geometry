module OpenSolid.Core.Interval
  ( empty
  , whole
  , singleton
  , hull
  , hullOf
  , intersection
  , fromEndpoints
  , endpoints
  , lowerBound
  , upperBound
  , isEmpty
  , width
  , interpolate
  , midpoint
  , contains
  , overlaps
  ) where


{-| Functionality for creating and manipulating `Interval` values.

# Constants

@docs empty, whole

# Creating intervals

Note that intervals can also be created directly (for example `Interval 2 3`) if the order of the
two values is known. `Scalar.hull` can be used to construct an interval from values given in
arbitrary order.

@docs singleton, hull, hullOf, intersection

# Conversions

@docs endpoints, fromEndpoints

-}


import OpenSolid.Core exposing (..)


positiveInfinity =
  1 / 0


negativeInfinity =
  -1 / 0


{-| The empty interval (contains no values).

Since it is internally implemented using NaN (not-a-number) values which do not compare equal to
themselves, direct comparisons against `Interval.empty` may not work properly; instead of

    interval == Interval.empty

you should use

    Interval.isEmpty interval

-}
empty: Interval
empty =
  Interval (0 / 0) (0 / 0)


{-| The whole interval (contains all values from negative infinity to positive infinity). -}
whole: Interval
whole =
  Interval negativeInfinity positiveInfinity


{-| Construct an interval with both lower and upper bounds equal to the given value.

    Interval.singleton 3 == Interval 3 3

-}
singleton: Float -> Interval
singleton value =
  Interval value value


{-| Construct an interval that contains both of the given intervals.

    Interval.hull (Interval 2 5) (Interval 4 8) == Interval 2 8
    Interval.hull (Interval 1 2) (Interval 9 10) == Interval 1 10
    Interval.hull (Interval.empty) (Interval 2 3) == Interval 2 3

-}
hull: Interval -> Interval -> Interval
hull (Interval firstLower firstUpper as first) (Interval secondLower secondUpper as second) =
  if isEmpty first then
    second
  else if isEmpty second then
    first
  else
    Interval (min firstLower secondLower) (max firstUpper secondUpper)


{-| Construct an interval that contains all of the given intervals.

    Interval.hullOf [Interval 2 3, Interval 4 8, Interval 1 2] == Interval 1 8
    Interval.hullOf [Interval.empty, Interval.singleton 3] == Interval 3 3
    Interval.isEmpty (Interval.hullOf []) == True

-}
hullOf: List Interval -> Interval
hullOf =
  List.foldl hull empty


{-| Construct an interval from the intersection of two other intervals. The result will contain all
values that are common to the two given intervals. If the two given intervals do not overlap, the
result will be empty.

    Interval.intersection (Interval 2 5) (Interval 3 6) == Interval 3 5
    Interval.isEmpty (Interval.intersection (Interval 2 3) (Interval 4 5)) == True
    Interval.intersection Interval.whole (Interval 2 3) == Interval 2 3
    Interval.isEmpty (Interval.intersection Interval.empty (Interval 2 3)) == True

-}
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


{-| Convert a pair of endpoints to an interval. Note that the endpoints must be in the correct
order!

-}
fromEndpoints: (Float, Float) -> Interval
fromEndpoints (lowerBound, upperBound) =
  Interval lowerBound upperBound


{-| Extract the lower bound of an interval. -}
lowerBound: Interval -> Float
lowerBound (Interval lower _) =
  lower


{-| Extract the upper bound of an interval. -}
upperBound: Interval -> Float
upperBound (Interval _ upper) =
  upper


{-| Convert an interval to a pair of endpoints. -}
endpoints: Interval -> (Float, Float)
endpoints (Interval lowerBound upperBound) =
  (lowerBound, upperBound)


{-| Check if an interval is empty (contains no values).

    Interval.isEmpty Interval.empty == True
    Interval.isEmpty (Interval 2 3) == False
    Interval.isEmpty (Interval.singleton 1) == False

-}
isEmpty: Interval -> Bool
isEmpty (Interval lowerBound upperBound) =
  isNaN lowerBound && isNaN upperBound


{-| Find the width of an interval.

    Interval.width (Interval 3 5) == 2
    isInfinite (Interval.width Interval.whole) == True
    isNaN (Interval.width Interval.empty) == True
    Interval.width (Interval.singleton 1) == 0

-}
width: Interval -> Float
width (Interval lowerBound upperBound) =
  upperBound - lowerBound


interpolate: Float -> Interval -> Float
interpolate parameter (Interval lowerBound upperBound) =
  lowerBound + parameter * (upperBound - lowerBound)


midpoint: Interval -> Float
midpoint =
  interpolate 0.5


contains: Interval -> Interval -> Bool
contains (Interval otherLower otherUpper) (Interval lowerBound upperBound) =
  lowerBound <= otherLower && otherUpper <= upperBound


overlaps: Interval -> Interval -> Bool
overlaps (Interval otherLower otherUpper) (Interval lowerBound upperBound) =
  lowerBound <= otherUpper && upperBound >= otherLower
