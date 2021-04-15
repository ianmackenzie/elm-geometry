--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module VectorBoundingBox2d exposing
    ( VectorBoundingBox2d
    , singleton, xy, fromIntervals, from, between
    , hull2, hull3, hull, hullN, hullOf, hullOfN
    , aggregate2, aggregate3, aggregate, aggregateN, aggregateOf, aggregateOfN
    , xInterval, yInterval, intervals, length
    , contains, isContainedIn, intersects, intersection
    , expandBy
    , interpolate
    , at, at_
    , multiplyBy, divideBy, half, twice
    , plus, plusBoundingBox, minus, difference, minusBoundingBox, times, product, timesUnitless, timesInterval, intervalProduct, timesUnitlessInterval
    , randomVector
    )

{-| A `VectorBoundingBox2d` is a version of a `BoundingBox2d` that contains
vectors instead of points. All functions behave like their `BoundingBox2d`
counterparts; see the [`BoundingBox2d` docs and examples](BoundingBox2d) for
details.

@docs VectorBoundingBox2d


# Constructors

@docs singleton, xy, fromIntervals, from, between


## Hull

Functions for building bounding boxes containing several vectors.

@docs hull2, hull3, hull, hullN, hullOf, hullOfN


## Aggregation

Functions for combining several bounding boxes into one bounding box that
contains all of the input boxes.

@docs aggregate2, aggregate3, aggregate, aggregateN, aggregateOf, aggregateOfN


# Properties

@docs xInterval, yInterval, intervals, length


# Queries

@docs contains, isContainedIn, intersects, intersection


# Transformations

@docs expandBy


# Interpolation

@docs interpolate


# Unit conversions

@docs at, at_


# Arithmetic

@docs multiplyBy, divideBy, half, twice
@docs plus, plusBoundingBox, minus, difference, minusBoundingBox, times, product, timesUnitless, timesInterval, intervalProduct, timesUnitlessInterval


# Random vector generation

@docs randomVector

-}

import BoundingBox2d exposing (BoundingBox2d)
import Float.Extra as Float
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Quantity exposing (Product, Quantity(..), Rate, Unitless)
import Quantity.Extra as Quantity
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import Vector2d exposing (Vector2d)


{-| -}
type alias VectorBoundingBox2d units coordinates =
    Types.VectorBoundingBox2d units coordinates


{-| Construct a bounding box containing the two vectors.
-}
hull2 : Vector2d units coordinates -> Vector2d units coordinates -> VectorBoundingBox2d units coordinates
hull2 firstVector secondVector =
    let
        (Types.Vector2d v1) =
            firstVector

        (Types.Vector2d v2) =
            secondVector

        x1 =
            v1.x

        y1 =
            v1.y

        x2 =
            v2.x

        y2 =
            v2.y
    in
    Types.VectorBoundingBox2d
        { minX = min x1 x2
        , maxX = max x1 x2
        , minY = min y1 y2
        , maxY = max y1 y2
        }


{-| Construct a zero-width bounding box containing a single vector.
-}
singleton : Vector2d units coordinates -> VectorBoundingBox2d units coordinates
singleton vector =
    let
        (Types.Vector2d { x, y }) =
            vector
    in
    Types.VectorBoundingBox2d
        { minX = x
        , maxX = x
        , minY = y
        , maxY = y
        }


{-| Construct a bounding box from separate X and Y [intervals](https://package.elm-lang.org/packages/ianmackenzie/elm-units-interval/latest/).
-}
xy : Interval Float units -> Interval Float units -> VectorBoundingBox2d units coordinates
xy givenXInterval givenYInterval =
    let
        ( Quantity minX, Quantity maxX ) =
            Interval.endpoints givenXInterval

        ( Quantity minY, Quantity maxY ) =
            Interval.endpoints givenYInterval
    in
    Types.VectorBoundingBox2d
        { minX = minX
        , maxX = maxX
        , minY = minY
        , maxY = maxY
        }


{-| Construct a bounding box from a pair of X and Y intervals.
-}
fromIntervals :
    ( Interval Float units, Interval Float units )
    -> VectorBoundingBox2d units coordinates
fromIntervals ( givenXInterval, givenYInterval ) =
    xy givenXInterval givenYInterval


{-| Given a point and a bounding box, compute the vector bounding box containing
all possible vectors from that point to any point in the bounding box.
-}
from :
    Point2d units coordinates
    -> BoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
from start end =
    let
        (Types.Point2d p) =
            start

        (Types.BoundingBox2d b) =
            end
    in
    Types.VectorBoundingBox2d
        { minX = b.minX - p.x
        , maxX = b.maxX - p.x
        , minY = b.minY - p.y
        , maxY = b.maxY - p.y
        }


{-| Given two bounding boxes, compute the vector bounding box containing all
possible vectors from a point in the first box to a point in the second box.
-}
between :
    BoundingBox2d units coordinates
    -> BoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
between start end =
    let
        (Types.BoundingBox2d b1) =
            start

        (Types.BoundingBox2d b2) =
            end
    in
    Types.VectorBoundingBox2d
        { minX = b2.minX - b2.maxX
        , maxX = b2.maxX - b2.minX
        , minY = b2.minY - b2.maxY
        , maxY = b2.maxY - b2.minY
        }


{-| Find the bounding box containing one or more input vectors:

    VectorBoundingBox2d.hull v1 [ v2, v3, v4 ]

See also [`hullN`](#hullN).

-}
hull : Vector2d units coordinates -> List (Vector2d units coordinates) -> VectorBoundingBox2d units coordinates
hull first rest =
    let
        (Types.Vector2d { x, y }) =
            first
    in
    hullHelp x x y y rest


hullHelp : Float -> Float -> Float -> Float -> List (Vector2d units coordinates) -> VectorBoundingBox2d units coordinates
hullHelp currentMinX currentMaxX currentMinY currentMaxY vectors =
    case vectors of
        next :: rest ->
            let
                (Types.Vector2d { x, y }) =
                    next
            in
            hullHelp
                (min x currentMinX)
                (max x currentMaxX)
                (min y currentMinY)
                (max y currentMaxY)
                rest

        [] ->
            Types.VectorBoundingBox2d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                }


{-| Like [`hull`](#hull), but lets you work on any kind of item as long as a
vector can be extracted from it.
-}
hullOf : (a -> Vector2d units coordinates) -> a -> List a -> VectorBoundingBox2d units coordinates
hullOf getVector first rest =
    let
        (Types.Vector2d { x, y }) =
            getVector first
    in
    hullOfHelp x x y y getVector rest


hullOfHelp : Float -> Float -> Float -> Float -> (a -> Vector2d units coordinates) -> List a -> VectorBoundingBox2d units coordinates
hullOfHelp currentMinX currentMaxX currentMinY currentMaxY getVector list =
    case list of
        next :: rest ->
            let
                (Types.Vector2d { x, y }) =
                    getVector next
            in
            hullOfHelp
                (min x currentMinX)
                (max x currentMaxX)
                (min y currentMinY)
                (max y currentMaxY)
                getVector
                rest

        [] ->
            Types.VectorBoundingBox2d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                }


{-| Build a bounding box that contains all three of the given vectors;

    VectorBoundingBox2d.hull3 v1 v2 v3

is equivalent to

    VectorBoundingBox2d.hull v1 [ v2, v3 ]

but is more efficient.

-}
hull3 : Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates -> VectorBoundingBox2d units coordinates
hull3 firstVector secondVector thirdVector =
    let
        (Types.Vector2d v1) =
            firstVector

        (Types.Vector2d v2) =
            secondVector

        (Types.Vector2d v3) =
            thirdVector

        x1 =
            v1.x

        y1 =
            v1.y

        x2 =
            v2.x

        y2 =
            v2.y

        x3 =
            v3.x

        y3 =
            v3.y
    in
    Types.VectorBoundingBox2d
        { minX = min (min x1 x2) x3
        , maxX = max (max x1 x2) x3
        , minY = min (min y1 y2) y3
        , maxY = max (max y1 y2) y3
        }


{-| Construct a bounding box containing all _N_ vectors in the given list. If the
list is empty, returns `Nothing`. If you know you have at least one point, you
can use [`hull`](#hull) instead.
-}
hullN : List (Vector2d units coordinates) -> Maybe (VectorBoundingBox2d units coordinates)
hullN vectors =
    case vectors of
        first :: rest ->
            Just (hull first rest)

        [] ->
            Nothing


{-| Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
-}
hullOfN : (a -> Vector2d units coordinates) -> List a -> Maybe (VectorBoundingBox2d units coordinates)
hullOfN getVector items =
    case items of
        first :: rest ->
            Just (hullOf getVector first rest)

        [] ->
            Nothing


{-| Find the bounding box containing one or more input boxes; works much like
[`hull`](#hull). See also [`aggregateN`](#aggregateN).
-}
aggregate : VectorBoundingBox2d units coordinates -> List (VectorBoundingBox2d units coordinates) -> VectorBoundingBox2d units coordinates
aggregate first rest =
    let
        (Types.VectorBoundingBox2d b1) =
            first
    in
    aggregateHelp b1.minX b1.maxX b1.minY b1.maxY rest


aggregateHelp : Float -> Float -> Float -> Float -> List (VectorBoundingBox2d units coordinates) -> VectorBoundingBox2d units coordinates
aggregateHelp currentMinX currentMaxX currentMinY currentMaxY boxes =
    case boxes of
        next :: rest ->
            let
                (Types.VectorBoundingBox2d b) =
                    next
            in
            aggregateHelp
                (min b.minX currentMinX)
                (max b.maxX currentMaxX)
                (min b.minY currentMinY)
                (max b.maxY currentMaxY)
                rest

        [] ->
            Types.VectorBoundingBox2d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                }


{-| Like [`aggregate`](#aggregate), but lets you work on any kind of item as
long as a bounding box can be extracted from it.
-}
aggregateOf : (a -> VectorBoundingBox2d units coordinates) -> a -> List a -> VectorBoundingBox2d units coordinates
aggregateOf getBoundingBox first rest =
    let
        (Types.VectorBoundingBox2d b1) =
            getBoundingBox first
    in
    aggregateOfHelp b1.minX b1.maxX b1.minY b1.maxY getBoundingBox rest


aggregateOfHelp : Float -> Float -> Float -> Float -> (a -> VectorBoundingBox2d units coordinates) -> List a -> VectorBoundingBox2d units coordinates
aggregateOfHelp currentMinX currentMaxX currentMinY currentMaxY getBoundingBox items =
    case items of
        next :: rest ->
            let
                (Types.VectorBoundingBox2d b) =
                    getBoundingBox next
            in
            aggregateOfHelp
                (min b.minX currentMinX)
                (max b.maxX currentMaxX)
                (min b.minY currentMinY)
                (max b.maxY currentMaxY)
                getBoundingBox
                rest

        [] ->
            Types.VectorBoundingBox2d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                }


{-| Build a bounding box that contains both given bounding boxes.
-}
aggregate2 : VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates
aggregate2 firstBox secondBox =
    let
        (Types.VectorBoundingBox2d b1) =
            firstBox

        (Types.VectorBoundingBox2d b2) =
            secondBox
    in
    Types.VectorBoundingBox2d
        { minX = min b1.minX b2.minX
        , maxX = max b1.maxX b2.maxX
        , minY = min b1.minY b2.minY
        , maxY = max b1.maxY b2.maxY
        }


{-| Build a bounding box that contains all three of the given bounding boxes;

    VectorBoundingBox2d.aggregate3 b1 b2 b3

is equivalent to

    VectorBoundingBox2d.aggregate b1 [ b2, b3 ]

but is more efficient.

-}
aggregate3 : VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates
aggregate3 firstBox secondBox thirdBox =
    let
        (Types.VectorBoundingBox2d b1) =
            firstBox

        (Types.VectorBoundingBox2d b2) =
            secondBox

        (Types.VectorBoundingBox2d b3) =
            thirdBox
    in
    Types.VectorBoundingBox2d
        { minX = min b1.minX (min b2.minX b3.minX)
        , maxX = max b1.maxX (max b2.maxX b3.maxX)
        , minY = min b1.minY (min b2.minY b3.minY)
        , maxY = max b1.maxY (max b2.maxY b3.maxY)
        }


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`. If you know you have at least one bounding
box, you can use [`aggregate`](#aggregate) instead.
-}
aggregateN : List (VectorBoundingBox2d units coordinates) -> Maybe (VectorBoundingBox2d units coordinates)
aggregateN boxes =
    case boxes of
        first :: rest ->
            Just (aggregate first rest)

        [] ->
            Nothing


{-| Combination of [`aggregateOf`](#aggregateOf) and [`aggregateN`](#aggregateN).
-}
aggregateOfN : (a -> VectorBoundingBox2d units coordinates) -> List a -> Maybe (VectorBoundingBox2d units coordinates)
aggregateOfN getBoundingBox items =
    case items of
        first :: rest ->
            Just (aggregateOf getBoundingBox first rest)

        [] ->
            Nothing


{-| Convert a bounding box from one units type to another, by providing a
conversion factor given as a rate of change of destination units with respect to
source units.
-}
at : Quantity Float (Rate units2 units1) -> VectorBoundingBox2d units1 coordinates -> VectorBoundingBox2d units2 coordinates
at rate boundingBox =
    let
        (Quantity r) =
            rate

        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    if r >= 0 then
        Types.VectorBoundingBox2d
            { minX = r * b.minX
            , maxX = r * b.maxX
            , minY = r * b.minY
            , maxY = r * b.maxY
            }

    else
        Types.VectorBoundingBox2d
            { minX = r * b.maxX
            , maxX = r * b.minX
            , minY = r * b.maxY
            , maxY = r * b.minY
            }


{-| Convert a bounding box from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> VectorBoundingBox2d units1 coordinates -> VectorBoundingBox2d units2 coordinates
at_ rate boundingBox =
    at (Quantity.inverse rate) boundingBox


{-| Multiply (scale) a bounding box by the given value.
-}
multiplyBy : Float -> VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates
multiplyBy scale boundingBox =
    let
        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    if scale >= 0 then
        Types.VectorBoundingBox2d
            { minX = scale * b.minX
            , maxX = scale * b.maxX
            , minY = scale * b.minY
            , maxY = scale * b.maxY
            }

    else
        Types.VectorBoundingBox2d
            { minX = scale * b.maxX
            , maxX = scale * b.minX
            , minY = scale * b.maxY
            , maxY = scale * b.minY
            }


{-| Divide a bounding box by the given value.
-}
divideBy : Float -> VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates
divideBy scale boundingBox =
    multiplyBy (1 / scale) boundingBox


{-| Equivalent to `multiplyBy 0.5` but shorter and slightly faster.
-}
half : VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates
half boundingBox =
    let
        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    Types.VectorBoundingBox2d
        { minX = 0.5 * b.minX
        , maxX = 0.5 * b.maxX
        , minY = 0.5 * b.minY
        , maxY = 0.5 * b.maxY
        }


{-| Equivalent to `multiplyBy 2` but shorter and slightly faster.
-}
twice : VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates
twice boundingBox =
    let
        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    Types.VectorBoundingBox2d
        { minX = 2 * b.minX
        , maxX = 2 * b.maxX
        , minY = 2 * b.minY
        , maxY = 2 * b.maxY
        }


{-| Add a vector to a bounding box.
-}
plus :
    Vector2d units coordinates
    -> VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
plus vector boundingBox =
    let
        (Types.Vector2d v) =
            vector

        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    Types.VectorBoundingBox2d
        { minX = b.minX + v.x
        , maxX = b.maxX + v.x
        , minY = b.minY + v.y
        , maxY = b.maxY + v.y
        }


{-| Add two bounding boxes together.
-}
plusBoundingBox :
    VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
plusBoundingBox secondBoundingBox firstBoundingBox =
    let
        (Types.VectorBoundingBox2d b2) =
            secondBoundingBox

        (Types.VectorBoundingBox2d b1) =
            firstBoundingBox
    in
    Types.VectorBoundingBox2d
        { minX = b1.minX + b2.minX
        , maxX = b1.maxX + b2.maxX
        , minY = b1.minY + b2.minY
        , maxY = b1.maxY + b2.maxY
        }


{-| Subtract a vector from a bounding box. Note the argument order; to compute
`box - vector` you would write

    box |> VectorBoundingBox2d.minus vector

which is equivalent to

    VectorBoundingBox2d.minus vector box

-}
minus :
    Vector2d units coordinates
    -> VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
minus vector boundingBox =
    let
        (Types.Vector2d v) =
            vector

        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    Types.VectorBoundingBox2d
        { minX = b.minX - v.x
        , maxX = b.maxX - v.x
        , minY = b.minY - v.y
        , maxY = b.maxY - v.y
        }


{-| Subtract a bounding box from a vector (the opposite of [`minus`](#minus)).
To compute `vector - box` you would write

    VectorBoundingBox2d.difference vector box

-}
difference :
    Vector2d units coordinates
    -> VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
difference vector boundingBox =
    let
        (Types.Vector2d v) =
            vector

        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    Types.VectorBoundingBox2d
        { minX = v.x - b.maxX
        , maxX = v.x - b.minX
        , minY = v.y - b.maxY
        , maxY = v.y - b.minY
        }


{-| Subtract one bounding box from another. Note the argument order; to compute
`firstBox - secondBox` you would write

    firstBox
        |> VectorBoundingBox2d.minusBoundingBox secondBox

which is equivalent to

    VectorBoundingBox2d.minusBoundingBox secondBox firstBox

-}
minusBoundingBox :
    VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
minusBoundingBox secondBoundingBox firstBoundingBox =
    let
        (Types.VectorBoundingBox2d b2) =
            secondBoundingBox

        (Types.VectorBoundingBox2d b1) =
            firstBoundingBox
    in
    Types.VectorBoundingBox2d
        { minX = b1.minX - b2.maxX
        , maxX = b1.maxX - b2.minX
        , minY = b1.minY - b2.maxY
        , maxY = b1.maxY - b2.minY
        }


unsafeMultiply :
    Quantity Float units1
    -> VectorBoundingBox2d units2 coordinates
    -> VectorBoundingBox2d units3 coordinates
unsafeMultiply quantity boundingBox =
    let
        (Quantity a) =
            quantity

        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    if a >= 0 then
        Types.VectorBoundingBox2d
            { minX = b.minX * a
            , maxX = b.maxX * a
            , minY = b.minY * a
            , maxY = b.maxY * a
            }

    else
        Types.VectorBoundingBox2d
            { minX = b.maxX * a
            , maxX = b.minX * a
            , minY = b.maxY * a
            , maxY = b.minY * a
            }


{-| Multiply a vector bounding box by a scalar quantity, resulting in a vector
bounding box with units `Product vectorUnits scalarUnits`. (To the compiler
`Product a b` and `Product b a` are different unit types, so sometimes you will
have to swap from `product` to `times` or vice versa to make the types work
out.)
-}
times :
    Quantity Float scalarUnits
    -> VectorBoundingBox2d vectorUnits coordinates
    -> VectorBoundingBox2d (Product vectorUnits scalarUnits) coordinates
times quantity boundingBox =
    unsafeMultiply quantity boundingBox


{-| Multiply a scalar quantity and a vector bounding box, resulting in a vector
bounding box with units `Product scalarUnits vectorUnits`.
-}
product :
    Quantity Float scalarUnits
    -> VectorBoundingBox2d vectorUnits coordinates
    -> VectorBoundingBox2d (Product scalarUnits vectorUnits) coordinates
product quantity boundingBox =
    unsafeMultiply quantity boundingBox


{-| Multiply a vector bounding box by a unitless quantity, leaving the units
unchanged.
-}
timesUnitless :
    Quantity Float Unitless
    -> VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
timesUnitless quantity boundingBox =
    unsafeMultiply quantity boundingBox


unsafeIntervalMultiply :
    Interval Float units1
    -> VectorBoundingBox2d units2 coordinates
    -> VectorBoundingBox2d units3 coordinates
unsafeIntervalMultiply interval boundingBox =
    let
        ( Quantity minA, Quantity maxA ) =
            Interval.endpoints interval

        (Types.VectorBoundingBox2d { minX, maxX, minY, maxY }) =
            boundingBox

        x1 =
            minA * minX

        x2 =
            minA * maxX

        x3 =
            maxA * minX

        x4 =
            maxA * maxX

        y1 =
            minA * minY

        y2 =
            minA * maxY

        y3 =
            maxA * minY

        y4 =
            maxA * maxY
    in
    Types.VectorBoundingBox2d
        { minX = min (min (min x1 x2) x3) x4
        , maxX = max (max (max x1 x2) x3) x4
        , minY = min (min (min y1 y2) y3) y4
        , maxY = max (max (max y1 y2) y3) y4
        }


{-| Multiply a vector bounding box by an interval, resulting in a vector
bounding box with units `Product vectorUnits scalarUnits`.
-}
timesInterval :
    Interval Float scalarUnits
    -> VectorBoundingBox2d vectorUnits coordinates
    -> VectorBoundingBox2d (Product vectorUnits scalarUnits) coordinates
timesInterval interval boundingBox =
    unsafeIntervalMultiply interval boundingBox


{-| Multiply an interval and a vector bounding box, resulting in a vector
bounding box with units `Product scalarUnits vectorUnits`.
-}
intervalProduct :
    Interval Float scalarUnits
    -> VectorBoundingBox2d vectorUnits coordinates
    -> VectorBoundingBox2d (Product scalarUnits vectorUnits) coordinates
intervalProduct interval boundingBox =
    unsafeIntervalMultiply interval boundingBox


{-| Multiply a vector bounding box by a unitless interval, leaving the units
unchanged.
-}
timesUnitlessInterval :
    Interval Float Unitless
    -> VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
timesUnitlessInterval interval boundingBox =
    unsafeIntervalMultiply interval boundingBox


{-| Get the range of X values contained by a bounding box.
-}
xInterval : VectorBoundingBox2d units coordinates -> Interval Float units
xInterval boundingBox =
    let
        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    Interval.from (Quantity b.minX) (Quantity b.maxX)


{-| Get the range of Y values contained by a bounding box.
-}
yInterval : VectorBoundingBox2d units coordinates -> Interval Float units
yInterval boundingBox =
    let
        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    Interval.from (Quantity b.minY) (Quantity b.maxY)


{-| Convert a bounding box to a pair of X and Y intervals.
-}
intervals : VectorBoundingBox2d units coordinates -> ( Interval Float units, Interval Float units )
intervals boundingBox =
    ( xInterval boundingBox, yInterval boundingBox )


{-| Get the range of lengths of vectors contained in a given bounding box.
-}
length : VectorBoundingBox2d units coordinates -> Interval Float units
length boundingBox =
    let
        ( Quantity xMin, Quantity xMax ) =
            Interval.endpoints (Interval.abs (xInterval boundingBox))

        ( Quantity yMin, Quantity yMax ) =
            Interval.endpoints (Interval.abs (yInterval boundingBox))

        minLength =
            Quantity (sqrt (xMin * xMin + yMin * yMin))

        maxLength =
            Quantity (sqrt (xMax * xMax + yMax * yMax))
    in
    Interval.from minLength maxLength


{-| Check if a bounding box contains a particular point.
-}
contains : Vector2d units coordinates -> VectorBoundingBox2d units coordinates -> Bool
contains vector boundingBox =
    let
        (Types.Vector2d { x, y }) =
            vector

        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    (x >= b.minX) && (x <= b.maxX) && (y >= b.minY) && (y <= b.maxY)


{-| Test if two boxes touch or overlap at all (have any points in common);

    VectorBoundingBox2d.intersects firstBox secondBox

is equivalent to

    VectorBoundingBox2d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

-}
intersects : VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates -> Bool
intersects other boundingBox =
    let
        (Types.VectorBoundingBox2d b2) =
            other

        (Types.VectorBoundingBox2d b1) =
            boundingBox
    in
    (b1.minX <= b2.maxX) && (b1.maxX >= b2.minX) && (b1.minY <= b2.maxY) && (b1.maxY >= b2.minY)


overlappingByAtLeast : Quantity Float units -> VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates -> Bool
overlappingByAtLeast tolerance firstBox secondBox =
    let
        (Types.VectorBoundingBox2d b1) =
            firstBox

        (Types.VectorBoundingBox2d b2) =
            secondBox

        xOverlap =
            min b1.maxX b2.maxX - max b1.minX b2.minX

        yOverlap =
            min b1.maxY b2.maxY - max b1.minY b2.minY

        clampedTolerance =
            max 0 (Quantity.unwrap tolerance)
    in
    (xOverlap >= clampedTolerance) && (yOverlap >= clampedTolerance)


separatedByAtLeast : Quantity Float units -> VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates -> Bool
separatedByAtLeast tolerance firstBox secondBox =
    let
        (Types.VectorBoundingBox2d b1) =
            firstBox

        (Types.VectorBoundingBox2d b2) =
            secondBox

        xSeparation =
            max b1.minX b2.minX - min b1.maxX b2.maxX

        ySeparation =
            max b1.minY b2.minY - min b1.maxY b2.maxY

        clampedTolerance =
            max 0 (Quantity.unwrap tolerance)
    in
    if (xSeparation > 0) && (ySeparation > 0) then
        xSeparation * xSeparation + ySeparation * ySeparation >= clampedTolerance * clampedTolerance

    else if xSeparation > 0 then
        xSeparation >= clampedTolerance

    else if ySeparation > 0 then
        ySeparation >= clampedTolerance

    else if xSeparation == 0 && ySeparation == 0 then
        clampedTolerance == 0

    else
        False


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).
-}
isContainedIn :
    VectorBoundingBox2d units coordinates
    -> VectorBoundingBox2d units coordinates
    -> Bool
isContainedIn other boundingBox =
    let
        (Types.VectorBoundingBox2d b2) =
            other

        (Types.VectorBoundingBox2d b1) =
            boundingBox
    in
    (b2.minX <= b1.minX) && (b1.maxX <= b2.maxX) && (b2.minY <= b1.minY) && (b1.maxY <= b2.maxY)


{-| Attempt to build a bounding box that contains all vectors common to both
given bounding boxes. If the given boxes do not intersect, returns `Nothing`.

If two boxes just touch along an edge or at a corner, they are still considered
to have an intersection, even though that intersection will have zero area (at
least one of its dimensions will be zero).

-}
intersection : VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates -> Maybe (VectorBoundingBox2d units coordinates)
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        let
            (Types.VectorBoundingBox2d b1) =
                firstBox

            (Types.VectorBoundingBox2d b2) =
                secondBox
        in
        Just
            (Types.VectorBoundingBox2d
                { minX = max b1.minX b2.minX
                , maxX = min b1.maxX b2.maxX
                , minY = max b1.minY b2.minY
                , maxY = min b1.maxY b2.maxY
                }
            )

    else
        Nothing


{-| Expand the given bounding box in all directions by the given offset.
Negative offsets will be treated as positive (the absolute value will be used),
so the resulting box will always be at least as large as the original.
-}
expandBy : Quantity Float units -> VectorBoundingBox2d units coordinates -> VectorBoundingBox2d units coordinates
expandBy amount boundingBox =
    let
        (Quantity dGiven) =
            amount

        d =
            abs dGiven

        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    Types.VectorBoundingBox2d
        { minX = b.minX - d
        , minY = b.minY - d
        , maxX = b.maxX + d
        , maxY = b.maxY + d
        }


{-| Interpolate within a bounding box based on parameter values which range from
0 to 1.
-}
interpolate : VectorBoundingBox2d units coordinates -> Float -> Float -> Vector2d units coordinates
interpolate boundingBox u v =
    let
        (Types.VectorBoundingBox2d b) =
            boundingBox
    in
    Types.Vector2d
        { x = Float.interpolateFrom b.minX b.maxX u
        , y = Float.interpolateFrom b.minY b.maxY v
        }


{-| Create a [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for vectors within a given bounding box.
-}
randomVector : VectorBoundingBox2d units coordinates -> Generator (Vector2d units coordinates)
randomVector boundingBox =
    let
        parameterValue =
            Random.float 0 1
    in
    Random.map2 (interpolate boundingBox) parameterValue parameterValue
