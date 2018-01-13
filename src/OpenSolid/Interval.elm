module OpenSolid.Interval
    exposing
        ( Interval
        , contains
        , cos
        , extrema
        , hull
        , hullOf
        , interpolate
        , intersection
        , intersects
        , isContainedIn
        , maxValue
        , midpoint
        , minValue
        , sin
        , singleton
        , width
        , with
        )

{-|

@docs Interval


# Constructors

@docs with, singleton, hull, intersection, hullOf


# Properties

@docs extrema, minValue, maxValue, midpoint, width


# Interpolation

@docs interpolate


# Arithmetic

@docs sin, cos


# Queries

@docs contains, intersects, isContainedIn

-}

import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Scalar as Scalar


{-| Represents a bounded interval with a minimum and maximum value, for example
the interval from 3 to 5.
-}
type alias Interval =
    Internal.Interval


{-| Construct an interval with the given minimum and maximum values. They should
be provided in the correct order but will be flipped if necessary to result in a
valid interval.

    exampleInterval =
        Interval.with { minValue = -1, maxValue = 5 }

-}
with : { minValue : Float, maxValue : Float } -> Interval
with extrema =
    if extrema.minValue <= extrema.maxValue then
        Internal.Interval extrema
    else
        Internal.Interval
            { minValue = extrema.maxValue
            , maxValue = extrema.minValue
            }


{-| Construct a zero-width interval containing a single value.

    Interval.singleton 3
    --> Interval.with { minValue = 3, maxValue = 3 }

-}
singleton : Float -> Interval
singleton value =
    Internal.Interval { minValue = value, maxValue = value }


{-| Construct an interval containing both of the given intervals.

    firstInterval =
        Interval.with { minValue = 1, maxValue = 2 }

    secondInterval =
        Interval.with { minValue = 3, maxValue = 6 }

    Interval.hull firstInterval secondInterval
    --> Interval.with { minValue = 1, maxValue = 6 }

-}
hull : Interval -> Interval -> Interval
hull firstInterval secondInterval =
    Internal.Interval
        { minValue = min (minValue firstInterval) (minValue secondInterval)
        , maxValue = max (maxValue firstInterval) (maxValue secondInterval)
        }


{-| Attempt to construct an interval containing all the values common to both
given intervals. If the intervals do not intersect, returns `Nothing`.

    firstInterval =
        Interval.with { minValue = 1, maxValue = 3 }

    secondInterval =
        Interval.with { minValue = 2, maxValue = 5 }

    thirdInterval =
        Interval.with { minValue = 4, maxValue = 7 }

    Interval.intersection firstInterval secondInterval
    --> Just (Interval.with { minValue = 2, maxValue = 3 })

    Interval.intersection firstInterval thirdInterval
    --> Nothing

If the two intervals just touch, a singleton interval will be returned:

    Interval.intersection
        (Interval.with { minValue = 1, maxValue = 2 })
        (Interval.with { minValue = 2, maxValue = 3 })
    --> Just (Interval.with { minValue = 2, maxValue = 2})

-}
intersection : Interval -> Interval -> Maybe Interval
intersection firstInterval secondInterval =
    let
        resultMin =
            max (minValue firstInterval) (minValue secondInterval)

        resultMax =
            min (maxValue firstInterval) (maxValue secondInterval)
    in
    if resultMin <= resultMax then
        Just (Internal.Interval { minValue = resultMin, maxValue = resultMax })
    else
        Nothing


{-| Construct an interval containing all of the intervals in the given list. If
the list is empty, returns `Nothing`.

    Interval.hullOf
        [ Interval.singleton 2
        , Interval.with { minValue = 3, maxValue = 4 }
        ]
    --> Just (Interval.with { minValue = 2, maxValue = 4 })

    Interval.hullOf []
    --> Nothing

-}
hullOf : List Interval -> Maybe Interval
hullOf intervals =
    case intervals of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Get the minimum and maximum values of an interval as a single record. Useful
when combined with destructuring:

    { minValue, maxValue } =
        Interval.extrema exampleInterval


    --> minValue = -1
    --> maxValue = 5

-}
extrema : Interval -> { minValue : Float, maxValue : Float }
extrema (Internal.Interval extrema) =
    extrema


{-| Get the minimum value of an interval.

    Interval.minValue exampleInterval
    --> -1

-}
minValue : Interval -> Float
minValue (Internal.Interval { minValue }) =
    minValue


{-| Get the maximum value of an interval.

    Interval.maxValue exampleInterval
    --> 5

-}
maxValue : Interval -> Float
maxValue (Internal.Interval { maxValue }) =
    maxValue


{-| Get the midpoint of an interval.

    Interval.midpoint exampleInterval
    --> 2

-}
midpoint : Interval -> Float
midpoint (Internal.Interval { minValue, maxValue }) =
    minValue + 0.5 * (maxValue - minValue)


{-| Get the width of an interval.

    Interval.width exampleInterval
    --> 6

-}
width : Interval -> Float
width (Internal.Interval { minValue, maxValue }) =
    maxValue - minValue


{-| Interpolate an interval from its minimum to its maximum value; a value of
0.0 corresponds to the minimum value of the interval, a value of 0.5 corresponds
to its midpoint and a value of 1.0 corresponds to its maximum value. Values less
than 0.0 or greater than 1.0 can be used to extrapolate.

    Interval.interpolate 0 exampleInterval
    --> -1

    Interval.interpolate 0.75 exampleInterval
    --> 3.5

    Interval.interpolate -0.5 exampleInterval
    --> -4

-}
interpolate : Float -> Interval -> Float
interpolate t (Internal.Interval { minValue, maxValue }) =
    Scalar.interpolateFrom minValue maxValue t


{-| Check if an interval contains a given value.

    Interval.contains 0 exampleInterval
    --> True

    Interval.contains 10 exampleInterval
    --> False

The minimum and maximum values of an interval are considered to be contained in
the interval, although if you are relying on this then there is a good chance
your code is vulnerable to numerical roundoff!

    Interval.contains -1 exampleInterval
    --> True

-}
contains : Float -> Interval -> Bool
contains value (Internal.Interval { minValue, maxValue }) =
    minValue <= value && value <= maxValue


{-| Check if two intervals touch or overlap (have any values in common).

    Interval.with { minValue = 0, maxValue = 10 }
        |> Interval.intersects exampleInterval
    --> True

    Interval.with { minValue = 10, maxValue = 20 }
        |> Interval.intersects exampleInterval
    --> False

Intervals that just touch each other are considered to intersect (this is
consistent with `intersection` which will return a zero-width interval for the
intersection of two just-touching intervals):

    Interval.with { minValue = -3, maxValue = -1 }
        |> Interval.intersects exampleInterval
    --> True

As with `contains`, though, if you are relying on this then there is a good
chance your code is vulnerable to numerical roundoff!

-}
intersects : Interval -> Interval -> Bool
intersects firstInterval secondInterval =
    (minValue firstInterval <= maxValue secondInterval)
        && (maxValue firstInterval >= minValue secondInterval)


{-| Check if the second interval is fully contained in the first.

    -- Note that here exampleInterval is actually the
    -- *second* interval due to how |> works
    exampleInterval
        |> Interval.isContainedIn
            (Interval.with
                { minValue = 0
                , maxValue = 10
                }
            )
    --> False

    exampleInterval
        |> Interval.isContainedIn
            (Interval.with
                { minValue = -10
                , maxValue = 10
                }
            )
    --> True

-}
isContainedIn : Interval -> Interval -> Bool
isContainedIn firstInterval secondInterval =
    (minValue secondInterval >= minValue firstInterval)
        && (maxValue secondInterval <= maxValue firstInterval)


{-| Check if the interval is a singleton.
-}
isSingleton : Interval -> Bool
isSingleton (Internal.Interval { minValue, maxValue }) =
    minValue == maxValue


{-| Shift the interval by some amount in the positive direction.

    exampleInterval
        |> Interval.shiftBy 3
    --> Interval.with { minValue = 2, maxValue = 8 }

-}
shiftBy : Float -> Interval -> Interval
shiftBy delta (Internal.Interval { minValue, maxValue }) =
    Internal.Interval
        { minValue = minValue + delta
        , maxValue = maxValue + delta
        }


{-| Get the image of sin(x) applied on the interval.

    Interval.with { minValue = -pi / 2, maxValue = pi / 2}
        |> Interval.sin
    --> Interval.with { minValue = -1, maxValue = 1 }

-}
sin : Interval -> Interval
sin ((Internal.Interval { minValue, maxValue }) as interval) =
    if isSingleton interval then
        singleton (Basics.sin minValue)
    else
        let
            ( includesMin, includesMax ) =
                sinIncludesMinMax interval

            newMin =
                if includesMin then
                    -1
                else
                    min (Basics.sin minValue) (Basics.sin maxValue)

            newMax =
                if includesMax then
                    1
                else
                    max (Basics.sin minValue) (Basics.sin maxValue)
        in
        Internal.Interval
            { minValue = newMin
            , maxValue = newMax
            }


{-| Get the image of cos(x) applied on the interval.

    Interval.with { minValue = -pi / 2, maxValue = pi / 2}
        |> Interval.cos
    --> Interval.with { minValue = 0, maxValue = 1 }

-}
cos : Interval -> Interval
cos ((Internal.Interval { minValue, maxValue }) as interval) =
    if isSingleton interval then
        singleton (Basics.cos minValue)
    else
        let
            ( includesMin, includesMax ) =
                cosIncludesMinMax interval

            newMin =
                if includesMin then
                    -1
                else
                    min (Basics.cos minValue) (Basics.cos maxValue)

            newMax =
                if includesMax then
                    1
                else
                    max (Basics.cos minValue) (Basics.cos maxValue)
        in
        Internal.Interval
            { minValue = newMin
            , maxValue = newMax
            }


{-| cos(x - pi/2) = sin(x), therefore if cos(interval - pi/2) includes
the maximum/minimum, that means sin(interval) includes the maximum/minimum
accordingly.
-}
sinIncludesMinMax : Interval -> ( Bool, Bool )
sinIncludesMinMax interval =
    interval |> shiftBy (-pi / 2) |> cosIncludesMinMax


{-| cos(x + pi) = -cos(x), therefore if cos(interval + pi) includes the maximum,
that means cos(interval) includes the minimum.
-}
cosIncludesMinMax : Interval -> ( Bool, Bool )
cosIncludesMinMax interval =
    ( interval |> shiftBy pi |> cosIncludesMax
    , interval |> cosIncludesMax
    )


{-| The maximum of cos(x) is x = 2 pi * k for every integer k.
If `minValue` and `maxValue` are in different branches
(meaning diffrent values of k), then the interval must pass through
2 pi * k, which means the interval must include the maximum value.
-}
cosIncludesMax : Interval -> Bool
cosIncludesMax (Internal.Interval { minValue, maxValue }) =
    let
        minBranch =
            floor <| minValue / (2 * pi)

        maxBranch =
            floor <| maxValue / (2 * pi)
    in
    minBranch /= maxBranch
