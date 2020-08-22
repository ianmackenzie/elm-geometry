--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module BoundingBox2d exposing
    ( BoundingBox2d
    , from, fromExtrema, withDimensions, singleton, xy, fromIntervals
    , union, intersection
    , hull, hull3, hullN, hullOf, hullOfN
    , aggregate, aggregate3, aggregateN, aggregateOf, aggregateOfN
    , extrema
    , minX, maxX, minY, maxY, dimensions, midX, midY, centerPoint
    , xInterval, yInterval, intervals
    , contains, isContainedIn, intersects, overlappingByAtLeast, separatedByAtLeast
    , scaleAbout, translateBy, translateIn, expandBy, offsetBy
    , at, at_
    , randomPoint
    )

{-| A `BoundingBox2d` is a rectangular box in 2D defined by its minimum and
maximum X and Y values. It is possible to generate bounding boxes for most
geometric objects; for example, [`Triangle2d.boundingBox`](Triangle2d#boundingBox)
takes a `Triangle2d` and returns a `BoundingBox2d` that contains that triangle.
There are several use cases where it is more efficient to deal with the bounding
box of an object than the object itself, such as:

  - Intersection checking: If (for example) the bounding boxes of a line segment
    and a triangle do not overlap, then the line segment and triangle cannot
    possibly intersect each other. Expensive intersection checking therefore
    only has to be performed for line segments and triangles whose bounding
    boxes _do_ overlap.
  - 2D rendering: When rendering a 2D scene, any object whose bounding box does
    not overlap the viewing area must itself be completely outside the viewing
    area, and therefore does not have to be drawn. This provides a simple form
    of culling.

@docs BoundingBox2d


# Constructors

@docs from, fromExtrema, withDimensions, singleton, xy, fromIntervals


## Booleans

@docs union, intersection


## Hull

Functions for building bounding boxes containing several points.

@docs hull, hull3, hullN, hullOf, hullOfN


## Aggregation

Functions for combining several bounding boxes into one bounding box that
contains all of the input boxes.

@docs aggregate, aggregate3, aggregateN, aggregateOf, aggregateOfN


# Properties

@docs extrema

You can also get these values separately:

@docs minX, maxX, minY, maxY, dimensions, midX, midY, centerPoint

You can also get the X and Y ranges of a bounding box as [intervals](https://package.elm-lang.org/packages/ianmackenzie/elm-units-interval/latest/):

@docs xInterval, yInterval, intervals


# Queries

@docs contains, isContainedIn, intersects, overlappingByAtLeast, separatedByAtLeast


# Transformations

@docs scaleAbout, translateBy, translateIn, expandBy, offsetBy


# Unit conversions

@docs at, at_


# Random point generation

@docs randomPoint

-}

import Direction2d exposing (Direction2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Rate)
import Quantity.Extra as Quantity
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import Vector2d exposing (Vector2d)


{-| -}
type alias BoundingBox2d units coordinates =
    Types.BoundingBox2d units coordinates


{-| Construct a bounding box with the two given points as two of its corners.
The points can be given in any order and don't have to represent the 'primary'
diagonal of the bounding box.

    BoundingBox2d.from
        (Point2d.meters 2 3)
        (Point2d.meters -1 5)
    --> BoundingBox2d.fromExtrema
    -->     { minX = Length.meters -1
    -->     , maxX = Length.meters 2
    -->     , minY = Length.meters 3
    -->     , maxY = Length.meters 5
    -->     }

-}
from : Point2d units coordinates -> Point2d units coordinates -> BoundingBox2d units coordinates
from firstPoint secondPoint =
    let
        x1 =
            Point2d.xCoordinate firstPoint

        y1 =
            Point2d.yCoordinate firstPoint

        x2 =
            Point2d.xCoordinate secondPoint

        y2 =
            Point2d.yCoordinate secondPoint
    in
    Types.BoundingBox2d
        { minX = Quantity.min x1 x2
        , maxX = Quantity.max x1 x2
        , minY = Quantity.min y1 y2
        , maxY = Quantity.max y1 y2
        }


{-| Construct a bounding box from its minimum and maximum X and Y values:

    exampleBox =
        BoundingBox2d.fromExtrema
            { minX = Length.meters 3
            , maxX = Length.meters 8
            , minY = Length.meters 2
            , maxY = Length.meters 6
            }

If the minimum and maximum values are provided in the wrong order (for example
if `minX` is greater than `maxX`), then they will be swapped so that the
resulting bounding box is valid.

-}
fromExtrema :
    { minX : Quantity Float units
    , maxX : Quantity Float units
    , minY : Quantity Float units
    , maxY : Quantity Float units
    }
    -> BoundingBox2d units coordinates
fromExtrema given =
    if
        (given.minX |> Quantity.lessThanOrEqualTo given.maxX)
            && (given.minY |> Quantity.lessThanOrEqualTo given.maxY)
    then
        Types.BoundingBox2d given

    else
        Types.BoundingBox2d
            { minX = Quantity.min given.minX given.maxX
            , maxX = Quantity.max given.minX given.maxX
            , minY = Quantity.min given.minY given.maxY
            , maxY = Quantity.max given.minY given.maxY
            }


{-| Construct a bounding box given its overall dimensions (width and height)
and center point.
-}
withDimensions :
    ( Quantity Float units, Quantity Float units )
    -> Point2d units coordinates
    -> BoundingBox2d units coordinates
withDimensions ( givenWidth, givenHeight ) givenCenterPoint =
    let
        ( x0, y0 ) =
            Point2d.coordinates givenCenterPoint

        halfWidth =
            Quantity.half (Quantity.abs givenWidth)

        halfHeight =
            Quantity.half (Quantity.abs givenHeight)
    in
    Types.BoundingBox2d
        { minX = x0 |> Quantity.minus halfWidth
        , maxX = x0 |> Quantity.plus halfWidth
        , minY = y0 |> Quantity.minus halfHeight
        , maxY = y0 |> Quantity.plus halfHeight
        }


{-| Construct a zero-width bounding box containing a single point.
-}
singleton : Point2d units coordinates -> BoundingBox2d units coordinates
singleton point =
    Types.BoundingBox2d
        { minX = Point2d.xCoordinate point
        , maxX = Point2d.xCoordinate point
        , minY = Point2d.yCoordinate point
        , maxY = Point2d.yCoordinate point
        }


{-| Construct a bounding box from separate X and Y [intervals](https://package.elm-lang.org/packages/ianmackenzie/elm-units-interval/latest/).
-}
xy : Interval Float units -> Interval Float units -> BoundingBox2d units coordinates
xy givenXInterval givenYInterval =
    Types.BoundingBox2d
        { minX = Interval.minValue givenXInterval
        , maxX = Interval.maxValue givenXInterval
        , minY = Interval.minValue givenYInterval
        , maxY = Interval.maxValue givenYInterval
        }


{-| Construct a bounding box from a pair of X and Y intervals.
-}
fromIntervals : ( Interval Float units, Interval Float units ) -> BoundingBox2d units coordinates
fromIntervals ( givenXInterval, givenYInterval ) =
    xy givenXInterval givenYInterval


{-| Find the bounding box containing one or more input points:

    BoundingBox2d.hull p1 [ p2, p3, p4 ]

Often ends up being used within a `case` expression:

    case points of
        [] ->
            -- some default behavior

        first :: rest ->
            let
                boundingBox =
                    BoundingBox2d.hull first rest
            in
            -- normal behavior using 'boundingBox'

See also [`hullN`](#hullN).

-}
hull : Point2d units coordinates -> List (Point2d units coordinates) -> BoundingBox2d units coordinates
hull first rest =
    let
        (Types.Point2d { x, y }) =
            first
    in
    hullHelp x x y y rest


hullHelp : Float -> Float -> Float -> Float -> List (Point2d units coordinates) -> BoundingBox2d units coordinates
hullHelp currentMinX currentMaxX currentMinY currentMaxY points =
    case points of
        next :: rest ->
            let
                (Types.Point2d { x, y }) =
                    next
            in
            hullHelp
                (min x currentMinX)
                (max x currentMaxX)
                (min y currentMinY)
                (max y currentMaxY)
                rest

        [] ->
            Types.BoundingBox2d
                { minX = Quantity currentMinX
                , maxX = Quantity currentMaxX
                , minY = Quantity currentMinY
                , maxY = Quantity currentMaxY
                }


{-| Like [`hull`](#hull), but lets you work on any kind of item as long as a
point can be extracted from it. For example, if you had

    type alias Vertex =
        { id : Int
        , position : Point2d Meters WorldCoordinates
        , color : Color
        }

then you could get the bounding box around several vertices using

    BoundingBox2d.hullOf .position
        firstVertex
        [ secondVertex
        , thirdVertex
        , fourthVertex
        ]

-}
hullOf : (a -> Point2d units coordinates) -> a -> List a -> BoundingBox2d units coordinates
hullOf getPoint first rest =
    let
        (Types.Point2d { x, y }) =
            getPoint first
    in
    hullOfHelp x x y y getPoint rest


hullOfHelp : Float -> Float -> Float -> Float -> (a -> Point2d units coordinates) -> List a -> BoundingBox2d units coordinates
hullOfHelp currentMinX currentMaxX currentMinY currentMaxY getPoint list =
    case list of
        next :: rest ->
            let
                (Types.Point2d { x, y }) =
                    getPoint next
            in
            hullOfHelp
                (min x currentMinX)
                (max x currentMaxX)
                (min y currentMinY)
                (max y currentMaxY)
                getPoint
                rest

        [] ->
            Types.BoundingBox2d
                { minX = Quantity currentMinX
                , maxX = Quantity currentMaxX
                , minY = Quantity currentMinY
                , maxY = Quantity currentMaxY
                }


{-| Build a bounding box that contains all three of the given points;

    BoundingBox2d.hull3 p1 p2 p3

is equivalent to

    BoundingBox2d.hull p1 [ p2, p3 ]

but is more efficient.

-}
hull3 : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> BoundingBox2d units coordinates
hull3 firstPoint secondPoint thirdPoint =
    let
        x1 =
            Point2d.xCoordinate firstPoint

        y1 =
            Point2d.yCoordinate firstPoint

        x2 =
            Point2d.xCoordinate secondPoint

        y2 =
            Point2d.yCoordinate secondPoint

        x3 =
            Point2d.xCoordinate thirdPoint

        y3 =
            Point2d.yCoordinate thirdPoint
    in
    Types.BoundingBox2d
        { minX = Quantity.min x1 (Quantity.min x2 x3)
        , maxX = Quantity.max x1 (Quantity.max x2 x3)
        , minY = Quantity.min y1 (Quantity.min y2 y3)
        , maxY = Quantity.max y1 (Quantity.max y2 y3)
        }


{-| Construct a bounding box containing all _N_ points in the given list. If the
list is empty, returns `Nothing`. If you know you have at least one point, you
can use [`hull`](#hull) instead.
-}
hullN : List (Point2d units coordinates) -> Maybe (BoundingBox2d units coordinates)
hullN points =
    case points of
        first :: rest ->
            Just (hull first rest)

        [] ->
            Nothing


{-| Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
-}
hullOfN : (a -> Point2d units coordinates) -> List a -> Maybe (BoundingBox2d units coordinates)
hullOfN getBoundingBox items =
    case items of
        first :: rest ->
            Just (hullOf getBoundingBox first rest)

        [] ->
            Nothing


{-| Find the bounding box containing one or more input boxes; works much like
[`hull`](#hull). See also [`aggregateN`](#aggregateN).
-}
aggregate : BoundingBox2d units coordinates -> List (BoundingBox2d units coordinates) -> BoundingBox2d units coordinates
aggregate first rest =
    let
        b1 =
            extrema first
    in
    aggregateHelp b1.minX b1.maxX b1.minY b1.maxY rest


aggregateHelp : Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> List (BoundingBox2d units coordinates) -> BoundingBox2d units coordinates
aggregateHelp currentMinX currentMaxX currentMinY currentMaxY boxes =
    case boxes of
        next :: rest ->
            let
                b =
                    extrema next
            in
            aggregateHelp
                (Quantity.min b.minX currentMinX)
                (Quantity.max b.maxX currentMaxX)
                (Quantity.min b.minY currentMinY)
                (Quantity.max b.maxY currentMaxY)
                rest

        [] ->
            Types.BoundingBox2d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                }


{-| Like [`aggregate`](#aggregate), but lets you work on any kind of item as
long as a bounding box can be extracted from it. For example, to get the
bounding box around four triangles:

    BoundingBox2d.aggregateOf Triangle2d.boundingBox
        firstTriangle
        [ secondTriangle
        , thirdTriangle
        , fourthTriangle
        ]

-}
aggregateOf : (a -> BoundingBox2d units coordinates) -> a -> List a -> BoundingBox2d units coordinates
aggregateOf getBoundingBox first rest =
    let
        b1 =
            extrema (getBoundingBox first)
    in
    aggregateOfHelp b1.minX b1.maxX b1.minY b1.maxY getBoundingBox rest


aggregateOfHelp : Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> (a -> BoundingBox2d units coordiantes) -> List a -> BoundingBox2d units coordinates
aggregateOfHelp currentMinX currentMaxX currentMinY currentMaxY getBoundingBox items =
    case items of
        next :: rest ->
            let
                b =
                    extrema (getBoundingBox next)
            in
            aggregateOfHelp
                (Quantity.min b.minX currentMinX)
                (Quantity.max b.maxX currentMaxX)
                (Quantity.min b.minY currentMinY)
                (Quantity.max b.maxY currentMaxY)
                getBoundingBox
                rest

        [] ->
            Types.BoundingBox2d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                }


{-| Build a bounding box that contains both given bounding boxes.

    firstBox =
        BoundingBox2d.from
            (Point2d.meters 1 2)
            (Point2d.meters 4 3)

    secondBox =
        BoundingBox2d.from
            (Point2d.meters -2 4)
            (Point2d.meters 2 5)

    BoundingBox2d.union firstBox secondBox
    --> BoundingBox2d.from
    -->     (Point2d.meters -2 2)
    -->     (Point2d.meters 4 5)

(Note that this is not strictly speaking a 'union' in the precise mathematical
sense.)

-}
union : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
union firstBox secondBox =
    let
        b1 =
            extrema firstBox

        b2 =
            extrema secondBox
    in
    Types.BoundingBox2d
        { minX = Quantity.min b1.minX b2.minX
        , maxX = Quantity.max b1.maxX b2.maxX
        , minY = Quantity.min b1.minY b2.minY
        , maxY = Quantity.max b1.maxY b2.maxY
        }


{-| Build a bounding box that contains all three of the given bounding boxes;

    BoundingBox2d.aggregate3 b1 b2 b3

is equivalent to

    BoundingBox2d.aggregate b1 [ b2, b3 ]

but is more efficient.

-}
aggregate3 : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
aggregate3 firstBox secondBox thirdBox =
    let
        b1 =
            extrema firstBox

        b2 =
            extrema secondBox

        b3 =
            extrema thirdBox
    in
    Types.BoundingBox2d
        { minX = Quantity.min b1.minX (Quantity.min b2.minX b3.minX)
        , maxX = Quantity.max b1.maxX (Quantity.max b2.maxX b3.maxX)
        , minY = Quantity.min b1.minY (Quantity.min b2.minY b3.minY)
        , maxY = Quantity.max b1.maxY (Quantity.max b2.maxY b3.maxY)
        }


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`. If you know you have at least one bounding
box, you can use [`aggregate`](#aggregate) instead.
-}
aggregateN : List (BoundingBox2d units coordinates) -> Maybe (BoundingBox2d units coordinates)
aggregateN boxes =
    case boxes of
        first :: rest ->
            Just (aggregate first rest)

        [] ->
            Nothing


{-| Combination of [`aggregateOf`](#aggregateOf) and [`aggregateN`](#aggregateN).
-}
aggregateOfN : (a -> BoundingBox2d units coordinates) -> List a -> Maybe (BoundingBox2d units coordinates)
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
at : Quantity Float (Rate units2 units1) -> BoundingBox2d units1 coordinates -> BoundingBox2d units2 coordinates
at rate (Types.BoundingBox2d boundingBox) =
    fromExtrema
        { minX = Quantity.at rate boundingBox.minX
        , maxX = Quantity.at rate boundingBox.maxX
        , minY = Quantity.at rate boundingBox.minY
        , maxY = Quantity.at rate boundingBox.maxY
        }


{-| Convert a bounding box from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> BoundingBox2d units1 coordinates -> BoundingBox2d units2 coordinates
at_ rate boundingBox =
    at (Quantity.inverse rate) boundingBox


{-| Get the minimum and maximum X and Y values of a bounding box in a single
record.

    BoundingBox2d.extrema exampleBox
    --> { minX = Length.meters 3
    --> , maxX = Length.meters 8
    --> , minY = Length.meters 2
    --> , maxY = Length.meters 6
    --> }

Can be useful when combined with record destructuring, e.g.

    { minX, maxX, minY, maxY } =
        BoundingBox2d.extrema exampleBox

-}
extrema :
    BoundingBox2d units coordinates
    ->
        { minX : Quantity Float units
        , maxX : Quantity Float units
        , minY : Quantity Float units
        , maxY : Quantity Float units
        }
extrema (Types.BoundingBox2d boundingBoxExtrema) =
    boundingBoxExtrema


{-| -}
minX : BoundingBox2d units coordinates -> Quantity Float units
minX (Types.BoundingBox2d boundingBox) =
    boundingBox.minX


{-| -}
maxX : BoundingBox2d units coordinates -> Quantity Float units
maxX (Types.BoundingBox2d boundingBox) =
    boundingBox.maxX


{-| -}
minY : BoundingBox2d units coordinates -> Quantity Float units
minY (Types.BoundingBox2d boundingBox) =
    boundingBox.minY


{-| -}
maxY : BoundingBox2d units coordinates -> Quantity Float units
maxY (Types.BoundingBox2d boundingBox) =
    boundingBox.maxY


{-| Get the X and Y dimensions (width and height) of a bounding box.

    ( width, height ) =
        BoundingBox2d.dimensions exampleBox

-}
dimensions : BoundingBox2d units coordinates -> ( Quantity Float units, Quantity Float units )
dimensions boundingBox =
    ( maxX boundingBox |> Quantity.minus (minX boundingBox)
    , maxY boundingBox |> Quantity.minus (minY boundingBox)
    )


{-| Get the median (central) X value of a bounding box.
-}
midX : BoundingBox2d units coordinates -> Quantity Float units
midX (Types.BoundingBox2d boundingBox) =
    Quantity.interpolateFrom boundingBox.minX boundingBox.maxX 0.5


{-| Get the median (central) Y value of a bounding box.
-}
midY : BoundingBox2d units coordinates -> Quantity Float units
midY (Types.BoundingBox2d boundingBox) =
    Quantity.interpolateFrom boundingBox.minY boundingBox.maxY 0.5


{-| Get the point at the center of a bounding box.
-}
centerPoint : BoundingBox2d units coordinates -> Point2d units coordinates
centerPoint boundingBox =
    Point2d.xy (midX boundingBox) (midY boundingBox)


{-| Get the range of X values contained by a bounding box.
-}
xInterval : BoundingBox2d units coordinates -> Interval Float units
xInterval boundingBox =
    Interval.from (minX boundingBox) (maxX boundingBox)


{-| Get the range of Y values contained by a bounding box.
-}
yInterval : BoundingBox2d units coordinates -> Interval Float units
yInterval boundingBox =
    Interval.from (minY boundingBox) (maxY boundingBox)


{-| Convert a bounding box to a pair of X and Y intervals.
-}
intervals : BoundingBox2d units coordinates -> ( Interval Float units, Interval Float units )
intervals boundingBox =
    ( xInterval boundingBox, yInterval boundingBox )


{-| Check if a bounding box contains a particular point.
-}
contains : Point2d units coordinates -> BoundingBox2d units coordinates -> Bool
contains point boundingBox =
    (Point2d.xCoordinate point |> Quantity.greaterThanOrEqualTo (minX boundingBox))
        && (Point2d.xCoordinate point |> Quantity.lessThanOrEqualTo (maxX boundingBox))
        && (Point2d.yCoordinate point |> Quantity.greaterThanOrEqualTo (minY boundingBox))
        && (Point2d.yCoordinate point |> Quantity.lessThanOrEqualTo (maxY boundingBox))


{-| Test if two boxes touch or overlap at all (have any points in common);

    BoundingBox2d.intersects firstBox secondBox

is equivalent to

    BoundingBox2d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

-}
intersects : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Bool
intersects other boundingBox =
    (minX boundingBox |> Quantity.lessThanOrEqualTo (maxX other))
        && (maxX boundingBox |> Quantity.greaterThanOrEqualTo (minX other))
        && (minY boundingBox |> Quantity.lessThanOrEqualTo (maxY other))
        && (maxY boundingBox |> Quantity.greaterThanOrEqualTo (minY other))


{-| Check two boxes overlap by at least the given amount. For example, you could
implement a tolerant collision check (one that only returns true if the boxes
overlap by at least a millimeter, and ignores boxes that just barely touch each
other) as

    boxesCollide firstBox secondBox =
        BoundingBox2d.overlappingByAtLeast
            (Length.millimeters 1)
            firstBox
            secondBox

Overlap is defined as the _minimum_ distance one box would have to move so that
it did not touch the other. Boxes that just touch are considered to have an
overlap of zero, so

    BoundingBox2d.overlappingByAtLeast Quantity.zero
        firstBox
        secondBox

will return true even if the two boxes just touch each other.

-}
overlappingByAtLeast : Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Bool
overlappingByAtLeast tolerance firstBox secondBox =
    let
        xOverlap =
            Quantity.min (maxX firstBox) (maxX secondBox)
                |> Quantity.minus
                    (Quantity.max (minX firstBox) (minX secondBox))

        yOverlap =
            Quantity.min (maxY firstBox) (maxY secondBox)
                |> Quantity.minus
                    (Quantity.max (minY firstBox) (minY secondBox))

        clampedTolerance =
            Quantity.max tolerance Quantity.zero
    in
    (xOverlap |> Quantity.greaterThanOrEqualTo clampedTolerance)
        && (yOverlap |> Quantity.greaterThanOrEqualTo clampedTolerance)


{-| Check if two boxes are separated by at least the given amount. For example,
to perform clash detection between some objects, you could use `separatedBy` on
those objects' bounding boxes as a quick check to see if the objects had a gap
of at least 1 cm between them:

    safelySeparated firstBox secondBox =
        BoundingBox2d.separatedByAtLeast
            (Length.centimeters 1)
            firstBox
            secondBox

Separation is defined as the _minimum_ distance one box would have to move so
that it touched the other. (Note that this may be a _diagonal_ distance between
corners.) Boxes that just touch are considered to have a separation of zero, so

    BoundingBox2d.separatedByAtLeast Quantity.zero
        firstBox
        secondBox

will return true even if the two boxes just touch each other.

-}
separatedByAtLeast : Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Bool
separatedByAtLeast tolerance firstBox secondBox =
    let
        clampedTolerance =
            Quantity.max tolerance Quantity.zero

        xSeparation =
            Quantity.max (minX firstBox) (minX secondBox)
                |> Quantity.minus
                    (Quantity.min (maxX firstBox) (maxX secondBox))

        ySeparation =
            Quantity.max (minY firstBox) (minY secondBox)
                |> Quantity.minus
                    (Quantity.min (maxY firstBox) (maxY secondBox))
    in
    if
        (xSeparation |> Quantity.greaterThan Quantity.zero)
            && (ySeparation |> Quantity.greaterThan Quantity.zero)
    then
        Quantity.squared xSeparation
            |> Quantity.plus (Quantity.squared ySeparation)
            |> Quantity.greaterThanOrEqualTo (Quantity.squared clampedTolerance)

    else if xSeparation |> Quantity.greaterThan Quantity.zero then
        xSeparation |> Quantity.greaterThanOrEqualTo clampedTolerance

    else if ySeparation |> Quantity.greaterThan Quantity.zero then
        ySeparation |> Quantity.greaterThanOrEqualTo clampedTolerance

    else if xSeparation == Quantity.zero || ySeparation == Quantity.zero then
        clampedTolerance == Quantity.zero

    else
        False


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).

    outerBox =
        BoundingBox2d.from
            (Point2d.meters 0 0)
            (Point2d.meters 10 10)

    innerBox =
        BoundingBox2d.from
            (Point2d.meters 1 3)
            (Point2d.meters 5 9)

    overlappingBox =
        BoundingBox2d.from
            (Point2d.meters 1 3)
            (Point2d.meters 5 12)

    innerBox |> BoundingBox2d.isContainedIn outerBox
    --> True

    overlappingBox |> BoundingBox2d.isContainedIn outerBox
    --> False

-}
isContainedIn : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Bool
isContainedIn other boundingBox =
    (minX other |> Quantity.lessThanOrEqualTo (minX boundingBox))
        && (maxX boundingBox |> Quantity.lessThanOrEqualTo (maxX other))
        && (minY other |> Quantity.lessThanOrEqualTo (minY boundingBox))
        && (maxY boundingBox |> Quantity.lessThanOrEqualTo (maxY other))


{-| Attempt to build a bounding box that contains all points common to both
given bounding boxes. If the given boxes do not intersect, returns `Nothing`.

    firstBox =
        BoundingBox2d.from
            (Point2d.meters 1 2)
            (Point2d.meters 4 3)

    secondBox =
        BoundingBox2d.from
            (Point2d.meters 2 1)
            (Point2d.meters 5 4)

    BoundingBox2d.intersection firstBox secondBox
    --> Just <|
    -->     BoundingBox2d.from
    -->         (Point2d.meters 2 2)
    -->         (Point2d.meters 4 3)

If two boxes just touch along an edge or at a corner, they are still considered
to have an intersection, even though that intersection will have zero area (at
least one of its dimensions will be zero):

    firstBox =
        BoundingBox2d.from
            (Point2d.meters 0 0)
            (Point2d.meters 1 2)

    secondBox =
        BoundingBox2d.from
            (Point2d.meters 1 1)
            (Point2d.meters 2 3)

    BoundingBox2d.intersection firstBox secondBox
    --> Just <|
    -->     BoundingBox2d.from
    -->         (Point2d.meters 1 1)
    -->         (Point2d.meters 1 2)

-}
intersection : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Maybe (BoundingBox2d units coordinates)
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        Just
            (Types.BoundingBox2d
                { minX = Quantity.max (minX firstBox) (minX secondBox)
                , maxX = Quantity.min (maxX firstBox) (maxX secondBox)
                , minY = Quantity.max (minY firstBox) (minY secondBox)
                , maxY = Quantity.min (maxY firstBox) (maxY secondBox)
                }
            )

    else
        Nothing


{-| Scale a bounding box about a given point by a given scale.
-}
scaleAbout : Point2d units coordinates -> Float -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
scaleAbout point scale boundingBox =
    let
        x0 =
            Point2d.xCoordinate point

        y0 =
            Point2d.yCoordinate point

        scaledMinX =
            Quantity.scaleAbout x0 scale (minX boundingBox)

        scaledMaxX =
            Quantity.scaleAbout x0 scale (maxX boundingBox)

        scaledMinY =
            Quantity.scaleAbout y0 scale (minY boundingBox)

        scaledMaxY =
            Quantity.scaleAbout y0 scale (maxY boundingBox)
    in
    if scale >= 0 then
        Types.BoundingBox2d
            { minX = scaledMinX
            , maxX = scaledMaxX
            , minY = scaledMinY
            , maxY = scaledMaxY
            }

    else
        Types.BoundingBox2d
            { minX = scaledMaxX
            , maxX = scaledMinX
            , minY = scaledMaxY
            , maxY = scaledMinY
            }


{-| Translate a bounding box by a given displacement.
-}
translateBy : Vector2d units coordinates -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
translateBy displacement boundingBox =
    let
        dx =
            Vector2d.xComponent displacement

        dy =
            Vector2d.yComponent displacement
    in
    Types.BoundingBox2d
        { minX = minX boundingBox |> Quantity.plus dx
        , maxX = maxX boundingBox |> Quantity.plus dx
        , minY = minY boundingBox |> Quantity.plus dy
        , maxY = maxY boundingBox |> Quantity.plus dy
        }


{-| Translate a bounding box in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
translateIn direction distance boundingBox =
    translateBy (Vector2d.withLength distance direction) boundingBox


{-| Offsets boundingBox irrespective of the resulting bounding box is valid or
not.
-}
unsafeOffsetBy : Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
unsafeOffsetBy amount boundingBox =
    Types.BoundingBox2d
        { minX = minX boundingBox |> Quantity.minus amount
        , minY = minY boundingBox |> Quantity.minus amount
        , maxY = maxY boundingBox |> Quantity.plus amount
        , maxX = maxX boundingBox |> Quantity.plus amount
        }


{-| Expand or shrink the given bounding box in all the directions by the given
distance. A positive offset will cause the bounding box to expand and a negative
value will cause it to shrink.

    BoundingBox2d.offsetBy (Length.meters -1) exampleBox
    --> Just <|
    -->     BoundingBox2d.fromExtrema
    -->         { minX = Length.meters 4
    -->         , maxX = Length.meters 7
    -->         , minY = Length.meters 3
    -->         , maxY = Length.meters 5
    -->         }

Returns `Nothing` if the offset is negative and large enough to cause the
bounding box to vanish (that is, if the offset is larger than half the height or
half the width of the bounding box, whichever is less):

    BoundingBox2d.offsetBy (Length.meters -3) exampleBox
    --> Nothing

If you only want to expand a bounding box, you can use
[`expandBy`](BoundingBox2d#expandBy) instead (which does not return a `Maybe`).

-}
offsetBy : Quantity Float units -> BoundingBox2d units coordinates -> Maybe (BoundingBox2d units coordinates)
offsetBy amount boundingBox =
    let
        ( width, height ) =
            dimensions boundingBox

        minValidOffset =
            Quantity.multiplyBy -0.5 (Quantity.min width height)
    in
    if amount |> Quantity.greaterThan minValidOffset then
        Just <| unsafeOffsetBy amount boundingBox

    else
        Nothing


{-| Expand the given bounding box in all directions by the given offset:

    BoundingBox2d.expandBy (Length.meters 3) exampleBox
    --> BoundingBox2d.fromExtrema
    -->     { minX = Length.meters 0
    -->     , maxX = Length.meters 11
    -->     , minY = Length.meters -1
    -->     , maxY = Length.meters 9
    -->     }

Negative offsets will be treated as positive (the absolute value will be used),
so the resulting box will always be at least as large as the original. If you
need to be able to contract a bounding box, use
[`offsetBy`](BoundingBox2d#offsetBy) instead.

-}
expandBy : Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
expandBy amount boundingBox =
    unsafeOffsetBy (Quantity.abs amount) boundingBox


{-| Create a [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for points within a given bounding box.
-}
randomPoint : BoundingBox2d units coordinates -> Generator (Point2d units coordinates)
randomPoint boundingBox =
    let
        ( x, y ) =
            intervals boundingBox
    in
    Random.map2 Point2d.xy (Interval.randomValue x) (Interval.randomValue y)
