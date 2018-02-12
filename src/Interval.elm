module Interval
    exposing
        ( Interval
        , aggregate
        , containingValues
        , contains
        , cos
        , endpoints
        , from
        , fromEndpoints
        , hull
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
        )

{-|

@docs Interval


# Constructors

@docs singleton, fromEndpoints, from, containingValues, aggregate, hull, intersection


# Properties

@docs endpoints, minValue, maxValue, midpoint, width


# Interpolation

@docs interpolate


# Arithmetic

@docs sin, cos


# Queries

@docs contains, intersects, isContainedIn

-}

import Scalar


{-| Represents a finite, bounded interval with a minimum and maximum value, for
example the interval from 3 to 5.
-}
type Interval
    = Interval ( Float, Float )


{-| Construct a zero-width interval containing a single value.

    Interval.singleton 3
    --> Interval.fromEndpoints ( 3, 3 )

-}
singleton : Float -> Interval
singleton value =
    Interval ( value, value )


{-| Construct an interval from its endpoints (the minimum and maximum values of
the interval).

    rgbRange =
        Interval.fromEndpoints ( 0, 255 )

    alphaRange =
        Interval.fromEndpoints ( 0, 1 )

The two values should be given in order but will be swapped if
necessary to ensure a valid interval is returned:

    Interval.endpoints (Interval.fromEndpoints ( 3, 2 ))
    --> ( 2, 3 )

-}
fromEndpoints : ( Float, Float ) -> Interval
fromEndpoints endpoints =
    let
        ( firstValue, secondValue ) =
            endpoints
    in
    if firstValue <= secondValue then
        Interval endpoints
    else
        Interval ( secondValue, firstValue )


{-| Construct an interval containing the two given values (which can be provided
in either order).

    Interval.endpoints (Interval.from 2 5)
    --> ( 2, 5 )

    Interval.endpoints (Interval.from 5 2)
    --> ( 2, 5 )

-}
from : Float -> Float -> Interval
from firstValue secondValue =
    if firstValue <= secondValue then
        Interval ( firstValue, secondValue )
    else
        Interval ( secondValue, firstValue )


{-| Construct an interval containing all values in the given list. If the list
is empty, returns `Nothing`.

    Interval.containingValues [ 2, 1, 3 ]
    --> Just (Interval.from 1 3)

    Interval.containingValues [ -3 ]
    --> Just (Interval.singleton -3)

    Interval.containingValues []
    --> Nothing

-}
containingValues : List Float -> Maybe Interval
containingValues values =
    Maybe.map2 from (List.minimum values) (List.maximum values)


{-| Construct an interval containing both of the given intervals.

    firstInterval =
        Interval.from 1 2

    secondInterval =
        Interval.from 3 6

    Interval.hull firstInterval secondInterval
    --> Interval.from 1 6

-}
hull : Interval -> Interval -> Interval
hull firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    Interval ( min min1 min2, max max1 max2 )


{-| Attempt to construct an interval containing all the values common to both
given intervals. If the intervals do not intersect, returns `Nothing`.

    Interval.intersection
        (Interval.from 1 3)
        (Interval.from 2 5)
    --> Just (Interval.from 2 3)

    Interval.intersection
        (Interval.from 1 3)
        (Interval.from 4 7)
    --> Nothing

If the two intervals just touch, a singleton interval will be returned:

    Interval.intersection
        (Interval.from 1 3)
        (Interval.from 3 5)
    --> Just (Interval.singleton 3)

-}
intersection : Interval -> Interval -> Maybe Interval
intersection firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval

        minValue =
            max min1 min2

        maxValue =
            min max1 max2
    in
    if minValue <= maxValue then
        Just (Interval ( minValue, maxValue ))
    else
        Nothing


{-| Construct an interval containing all of the intervals in the given list. If
the list is empty, returns `Nothing`.

    Interval.aggregate
        [ Interval.singleton 2
        , Interval.from 3 4
        ]
    --> Just (Interval.from 2 4)

    Interval.aggregate []
    --> Nothing

-}
aggregate : List Interval -> Maybe Interval
aggregate intervals =
    case intervals of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Get the endpoints of an interval (its minimum and maximum values) as a
tuple. The first value will always be less than or equal to the second.

    ( minValue, maxValue ) =
        Interval.endpoints someInterval

For any interval,

    Interval.endpoints interval

is equivalent to (but more efficient than)

    ( Interval.minValue interval
    , Interval.maxValue interval
    )

-}
endpoints : Interval -> ( Float, Float )
endpoints (Interval endpoints) =
    endpoints


{-| Get the minimum value of an interval.

    Interval.minValue (Interval.from 1 3)
    --> 1

-}
minValue : Interval -> Float
minValue interval =
    Tuple.first (endpoints interval)


{-| Get the maximum value of an interval.

    Interval.maxValue (Interval.from 1 3)
    --> 3

-}
maxValue : Interval -> Float
maxValue interval =
    Tuple.second (endpoints interval)


{-| Get the midpoint of an interval.

    Interval.midpoint (Interval.from 1 5)
    --> 3

-}
midpoint : Interval -> Float
midpoint interval =
    let
        ( minValue, maxValue ) =
            endpoints interval
    in
    minValue + 0.5 * (maxValue - minValue)


{-| Get the width of an interval.

    Interval.width (Interval.from 1 5)
    --> 4

-}
width : Interval -> Float
width interval =
    let
        ( minValue, maxValue ) =
            endpoints interval
    in
    maxValue - minValue


{-| Interpolate between an interval's endpoints; a value of 0.0 corresponds to
the minimum value of the interval, a value of 0.5 corresponds to its midpoint
and a value of 1.0 corresponds to its maximum value. Values less than 0.0 or
greater than 1.0 can be used to extrapolate.

    Interval.interpolate (Interval.from -1 5) 0
    --> -1

    Interval.interpolate (Interval.from -1 5) 0.75
    --> 3.5

    Interval.interpolate (Interval.from -1 5) -0.5
    --> -4

-}
interpolate : Interval -> Float -> Float
interpolate interval t =
    let
        ( minValue, maxValue ) =
            endpoints interval
    in
    Scalar.interpolateFrom minValue maxValue t


{-| Check if an interval contains a given value.

    Interval.contains 0 (Interval.from -1 3)
    --> True

    Interval.contains 5 (Interval.from -1 3)
    --> False

The minimum and maximum values of an interval are considered to be contained in
the interval, although if you are relying on this then there is a good chance
your code is vulnerable to numerical roundoff!

    Interval.contains 3 (Interval.from -1 3)
    --> True

-}
contains : Float -> Interval -> Bool
contains value interval =
    let
        ( minValue, maxValue ) =
            endpoints interval
    in
    minValue <= value && value <= maxValue


{-| Check if two intervals touch or overlap (have any values in common).

    Interval.from -5 5
        |> Interval.intersects (Interval.from 0 10)
    --> True

    Interval.from -5 5
        |> Interval.intersects (Interval.from 10 20)
    --> False

Intervals that just touch each other are considered to intersect (this is
consistent with `intersection` which will return a zero-width interval for the
intersection of two just-touching intervals):

    Interval.from -5 5
        |> Interval.intersects (Interval.from 5 10)
    --> True

As with `contains`, though, if you are relying on this then there is a good
chance your code is vulnerable to numerical roundoff!

-}
intersects : Interval -> Interval -> Bool
intersects firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    min1 <= max2 && max1 >= min2


{-| Check if the second interval is fully contained in the first.

    Interval.from -5 5
        |> Interval.isContainedIn (Interval.from 0 10)
    --> False

    Interval.from -5 5
        |> Interval.isContainedIn (Interval.from -10 10)
    --> True

Be careful with the argument order! If not using the `|>` operator, the second
example would be written as:

    Interval.isContainedIn (Interval.from -10 10)
        (Interval.from -5 5)
    --> True

-}
isContainedIn : Interval -> Interval -> Bool
isContainedIn firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    min1 <= min2 && max2 <= max1


{-| Check if the interval is a singleton (the minimum and maximum values are the
same).

    Interval.isSingleton (Interval.fromEndpoints ( 2, 2 ))
    --> True

    Interval.isSingleton (Interval.fromEndpoints ( 2, 3 ))
    --> False

-}
isSingleton : Interval -> Bool
isSingleton interval =
    let
        ( minValue, maxValue ) =
            endpoints interval
    in
    minValue == maxValue


{-| Add the given amount to both endpoints of the given interval.

    Interval.shiftBy 3 (Interval.from -1 5)
    --> Interval.from 2 8

-}
shiftBy : Float -> Interval -> Interval
shiftBy delta interval =
    let
        ( minValue, maxValue ) =
            endpoints interval
    in
    Interval ( minValue + delta, maxValue + delta )


{-| Get the image of sin(x) applied on the interval.

    Interval.sin (Interval.from 0 (degrees 45))
    --> Interval.from 0 0.7071

    Interval.sin (Interval.from 0 pi)
    --> Interval.from 0 1

-}
sin : Interval -> Interval
sin interval =
    if isSingleton interval then
        singleton (Basics.sin (minValue interval))
    else
        let
            ( includesMin, includesMax ) =
                sinIncludesMinMax interval

            ( minValue, maxValue ) =
                endpoints interval

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
        fromEndpoints ( newMin, newMax )


{-| Get the image of cos(x) applied on the interval.

    Interval.cos (Interval.from 0 (degrees 45))
    --> Interval.from 0.7071 1

    Interval.cos (Interval.from 0 pi)
    --> Interval.from -1 1

-}
cos : Interval -> Interval
cos interval =
    if isSingleton interval then
        singleton (Basics.cos (minValue interval))
    else
        let
            ( includesMin, includesMax ) =
                cosIncludesMinMax interval

            ( minValue, maxValue ) =
                endpoints interval

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
        fromEndpoints ( newMin, newMax )


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
cosIncludesMax interval =
    let
        ( minValue, maxValue ) =
            endpoints interval

        minBranch =
            floor <| minValue / (2 * pi)

        maxBranch =
            floor <| maxValue / (2 * pi)
    in
    minBranch /= maxBranch
