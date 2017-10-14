module OpenSolid.Interval
    exposing
        ( Interval
        , contains
        , extrema
        , hull
        , hullOf
        , intersection
        , intersects
        , isContainedIn
        , maxValue
        , midpoint
        , minValue
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


# Queries

@docs contains, intersects, isContainedIn

-}


{-| Represents a bounded interval with a minimum and maximum value, for example
the interval from 3 to 5.
-}
type Interval
    = Interval { minValue : Float, maxValue : Float }


{-| Construct an interval with the given minimum and maximum values. They should
be provided in the correct order but will be flipped if necessary to result in a
valid interval.

    exampleInterval =
        Interval.with { minValue = -1, maxValue = 5 }

-}
with : { minValue : Float, maxValue : Float } -> Interval
with extrema =
    if extrema.minValue <= extrema.maxValue then
        Interval extrema
    else
        Interval { minValue = extrema.maxValue, maxValue = extrema.minValue }


{-| Construct a zero-width interval containing a single value.

    Interval.singleton 3
    --> Interval.with { minValue = 3, maxValue = 3 }

-}
singleton : Float -> Interval
singleton value =
    Interval { minValue = value, maxValue = value }


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
    Interval
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
        Just (Interval { minValue = resultMin, maxValue = resultMax })
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
extrema (Interval extrema) =
    extrema


{-| Get the minimum value of an interval.

    Interval.minValue exampleInterval
    --> -1

-}
minValue : Interval -> Float
minValue (Interval { minValue }) =
    minValue


{-| Get the maximum value of an interval.

    Interval.maxValue exampleInterval
    --> 5

-}
maxValue : Interval -> Float
maxValue (Interval { maxValue }) =
    maxValue


{-| Get the midpoint of an interval.

    Interval.midpoint exampleInterval
    --> 2

-}
midpoint : Interval -> Float
midpoint (Interval { minValue, maxValue }) =
    minValue + 0.5 * (maxValue - minValue)


{-| Get the width of an interval.

    Interval.width exampleInterval
    --> 6

-}
width : Interval -> Float
width (Interval { minValue, maxValue }) =
    maxValue - minValue


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
contains value (Interval { minValue, maxValue }) =
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


isSingleton : Interval -> Bool
isSingleton (Interval { minValue, maxValue }) =
    minValue == maxValue


shift : Float -> Interval -> Interval
shift delta (Interval { minValue, maxValue }) =
    Interval { minValue = minValue + delta, maxValue = maxValue + delta }


sin : Interval -> Interval
sin ((Interval { minValue, maxValue }) as interval) =
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
            Interval
                { minValue = newMin
                , maxValue = newMax
                }


cos : Interval -> Interval
cos ((Interval { minValue, maxValue }) as interval) =
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
            Interval
                { minValue = newMin
                , maxValue = newMax
                }


sinIncludesMinMax : Interval -> ( Bool, Bool )
sinIncludesMinMax interval =
    interval |> shift (-pi / 2) |> cosIncludesMinMax


cosIncludesMinMax : Interval -> ( Bool, Bool )
cosIncludesMinMax interval =
    ( interval |> shift pi |> cosIncludesMax
    , interval |> cosIncludesMax
    )


cosIncludesMax : Interval -> Bool
cosIncludesMax (Interval { minValue, maxValue }) =
    let
        minBranch =
            floor <| minValue / (2 * pi)

        maxBranch =
            floor <| maxValue / (2 * pi)
    in
        minBranch /= maxBranch
