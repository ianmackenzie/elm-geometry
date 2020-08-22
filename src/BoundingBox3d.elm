--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module BoundingBox3d exposing
    ( BoundingBox3d
    , from, fromExtrema, withDimensions, singleton, xyz, fromIntervals
    , union, intersection
    , hull, hull3, hullN, hullOf, hullOfN
    , aggregate, aggregate3, aggregateN, aggregateOf, aggregateOfN
    , extrema
    , minX, maxX, minY, maxY, minZ, maxZ, dimensions, midX, midY, midZ, centerPoint
    , xInterval, yInterval, zInterval, intervals
    , contains, isContainedIn, intersects, overlappingByAtLeast, separatedByAtLeast
    , scaleAbout, translateBy, translateIn, expandBy, offsetBy
    , at, at_
    , randomPoint
    )

{-| A `BoundingBox3d` is a rectangular box in 3D defined by its minimum and
maximum X, Y and Z values. It is possible to generate bounding boxes for most
geometric objects; for example, [`Triangle3d.boundingBox`](Triangle3d#boundingBox)
takes a `Triangle3d` and returns a `BoundingBox3d` that contains that triangle.
There are several use cases where it is more efficient to deal with the bounding
box of an object than the object itself, such as:

  - Intersection checking: If (for example) the bounding boxes of a line segment
    and a triangle do not overlap, then the line segment and triangle cannot
    possibly intersect each other. Expensive intersection checking therefore
    only has to be performed for line segments and triangles whose bounding
    boxes _do_ overlap.
  - 3D rendering: When rendering a 3D scene, any object whose bounding box is
    not visible must itself be not visible, and therefore does not have to be
    drawn. This provides a simple form of culling.

@docs BoundingBox3d


# Constructors

@docs from, fromExtrema, withDimensions, singleton, xyz, fromIntervals


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

@docs minX, maxX, minY, maxY, minZ, maxZ, dimensions, midX, midY, midZ, centerPoint

You can also get the X, Y and Z ranges of a bounding box as [intervals](https://package.elm-lang.org/packages/ianmackenzie/elm-units-interval/latest/):

@docs xInterval, yInterval, zInterval, intervals


# Queries

@docs contains, isContainedIn, intersects, overlappingByAtLeast, separatedByAtLeast


# Transformations

@docs scaleAbout, translateBy, translateIn, expandBy, offsetBy


# Unit conversions

@docs at, at_


# Random point generation

@docs randomPoint

-}

import Direction3d exposing (Direction3d)
import Geometry.Types as Types
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), Rate)
import Quantity.Extra as Quantity
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import Vector3d exposing (Vector3d)


{-| -}
type alias BoundingBox3d units coordinates =
    Types.BoundingBox3d units coordinates


{-| Construct a bounding box with the two given points as two of its corners.
The points can be given in any order and don't have to represent the 'primary'
diagonal of the bounding box.

    BoundingBox3d.from
        (Point3d.meters 2 1 3)
        (Point3d.meters -1 5 -2)
    --> BoundingBox3d.fromExtrema
    -->     { minX = Length.meters -1
    -->     , maxX = Length.meters 2
    -->     , minY = Length.meters 1
    -->     , maxY = Length.meters 5
    -->     , minZ = Length.meters -2
    -->     , maxZ = Length.meters 3
    -->     }

-}
from : Point3d units coordinates -> Point3d units coordinates -> BoundingBox3d units coordinates
from firstPoint secondPoint =
    let
        x1 =
            Point3d.xCoordinate firstPoint

        y1 =
            Point3d.yCoordinate firstPoint

        z1 =
            Point3d.zCoordinate firstPoint

        x2 =
            Point3d.xCoordinate secondPoint

        y2 =
            Point3d.yCoordinate secondPoint

        z2 =
            Point3d.zCoordinate secondPoint
    in
    Types.BoundingBox3d
        { minX = Quantity.min x1 x2
        , maxX = Quantity.max x1 x2
        , minY = Quantity.min y1 y2
        , maxY = Quantity.max y1 y2
        , minZ = Quantity.min z1 z2
        , maxZ = Quantity.max z1 z2
        }


{-| Construct a bounding box from its minimum and maximum X, Y and Z values:

    exampleBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters -2
            , maxX = Length.meters 2
            , minY = Length.meters 2
            , maxY = Length.meters 5
            , minZ = Length.meters 3
            , maxZ = Length.meters 4
            }

If the minimum and maximum values are provided in the wrong order (for example
if `minX` is greater than `maxX`, then they will be swapped so that the
resulting bounding box is valid.

-}
fromExtrema :
    { minX : Quantity Float units
    , maxX : Quantity Float units
    , minY : Quantity Float units
    , maxY : Quantity Float units
    , minZ : Quantity Float units
    , maxZ : Quantity Float units
    }
    -> BoundingBox3d units coordinates
fromExtrema given =
    if
        (given.minX |> Quantity.lessThanOrEqualTo given.maxX)
            && (given.minY |> Quantity.lessThanOrEqualTo given.maxY)
            && (given.minZ |> Quantity.lessThanOrEqualTo given.maxZ)
    then
        Types.BoundingBox3d given

    else
        Types.BoundingBox3d
            { minX = Quantity.min given.minX given.maxX
            , maxX = Quantity.max given.minX given.maxX
            , minY = Quantity.min given.minY given.maxY
            , maxY = Quantity.max given.minY given.maxY
            , minZ = Quantity.min given.minZ given.maxZ
            , maxZ = Quantity.max given.minZ given.maxZ
            }


{-| Construct a bounding box given its overall dimensions (length, width,
height) and center point.
-}
withDimensions :
    ( Quantity Float units, Quantity Float units, Quantity Float units )
    -> Point3d units coordinates
    -> BoundingBox3d units coordinates
withDimensions ( givenLength, givenWidth, givenHeight ) givenCenterPoint =
    let
        ( x0, y0, z0 ) =
            Point3d.coordinates givenCenterPoint

        halfLength =
            Quantity.half (Quantity.abs givenLength)

        halfWidth =
            Quantity.half (Quantity.abs givenWidth)

        halfHeight =
            Quantity.half (Quantity.abs givenHeight)
    in
    Types.BoundingBox3d
        { minX = x0 |> Quantity.minus halfLength
        , maxX = x0 |> Quantity.plus halfLength
        , minY = y0 |> Quantity.minus halfWidth
        , maxY = y0 |> Quantity.plus halfWidth
        , minZ = z0 |> Quantity.minus halfHeight
        , maxZ = z0 |> Quantity.plus halfHeight
        }


{-| Construct a zero-width bounding box containing a single point.
-}
singleton : Point3d units coordinates -> BoundingBox3d units coordinates
singleton point =
    Types.BoundingBox3d
        { minX = Point3d.xCoordinate point
        , maxX = Point3d.xCoordinate point
        , minY = Point3d.yCoordinate point
        , maxY = Point3d.yCoordinate point
        , minZ = Point3d.zCoordinate point
        , maxZ = Point3d.zCoordinate point
        }


{-| Construct a bounding box from separate X, Y and Z [intervals](https://package.elm-lang.org/packages/ianmackenzie/elm-units-interval/latest/).
-}
xyz :
    Interval Float units
    -> Interval Float units
    -> Interval Float units
    -> BoundingBox3d units coordinates
xyz givenXInterval givenYInterval givenZInterval =
    Types.BoundingBox3d
        { minX = Interval.minValue givenXInterval
        , maxX = Interval.maxValue givenXInterval
        , minY = Interval.minValue givenYInterval
        , maxY = Interval.maxValue givenYInterval
        , minZ = Interval.minValue givenZInterval
        , maxZ = Interval.maxValue givenZInterval
        }


{-| Construct a bounding box from a tuple of X, Y and Z intervals.
-}
fromIntervals :
    ( Interval Float units, Interval Float units, Interval Float units )
    -> BoundingBox3d units coordinates
fromIntervals ( givenXInterval, givenYInterval, givenZInterval ) =
    xyz givenXInterval givenYInterval givenZInterval


{-| Find the bounding box containing one or more input points. You would
generally use this within a `case` expression:

    case points of
        [] ->
            -- some default behavior

        first :: rest ->
            let
                boundingBox =
                    BoundingBox3d.hull first rest
            in
            ...

If you need to handle the case of zero input points, see [`hullN`](#hullN).

-}
hull : Point3d units coordinates -> List (Point3d units coordinates) -> BoundingBox3d units coordinates
hull first rest =
    let
        (Types.Point3d { x, y, z }) =
            first
    in
    hullHelp x x y y z z rest


hullHelp : Float -> Float -> Float -> Float -> Float -> Float -> List (Point3d units coordinates) -> BoundingBox3d units coordinates
hullHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ points =
    case points of
        next :: rest ->
            let
                (Types.Point3d { x, y, z }) =
                    next
            in
            hullHelp
                (min x currentMinX)
                (max x currentMaxX)
                (min y currentMinY)
                (max y currentMaxY)
                (min z currentMinZ)
                (max z currentMaxZ)
                rest

        [] ->
            Types.BoundingBox3d
                { minX = Quantity currentMinX
                , maxX = Quantity currentMaxX
                , minY = Quantity currentMinY
                , maxY = Quantity currentMaxY
                , minZ = Quantity currentMinZ
                , maxZ = Quantity currentMaxZ
                }


{-| Like [`hull`](#hull), but lets you work on any kind of item as long as a
point can be extracted from it. For example, to get the bounding box around the
centroids of four triangles:

    BoundingBox3d.hullOf Triangle3d.centroid
        firstTriangle
        [ secondTriangle
        , thirdTriangle
        , fourthTriangle
        ]

-}
hullOf : (a -> Point3d units coordinates) -> a -> List a -> BoundingBox3d units coordinates
hullOf getPoint first rest =
    let
        (Types.Point3d { x, y, z }) =
            getPoint first
    in
    hullOfHelp x x y y z z getPoint rest


hullOfHelp : Float -> Float -> Float -> Float -> Float -> Float -> (a -> Point3d units coordinates) -> List a -> BoundingBox3d units coordinates
hullOfHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ getPoint list =
    case list of
        next :: rest ->
            let
                (Types.Point3d { x, y, z }) =
                    getPoint next
            in
            hullOfHelp
                (min x currentMinX)
                (max x currentMaxX)
                (min y currentMinY)
                (max y currentMaxY)
                (min z currentMinZ)
                (max z currentMaxZ)
                getPoint
                rest

        [] ->
            Types.BoundingBox3d
                { minX = Quantity currentMinX
                , maxX = Quantity currentMaxX
                , minY = Quantity currentMinY
                , maxY = Quantity currentMaxY
                , minZ = Quantity currentMinZ
                , maxZ = Quantity currentMaxZ
                }


{-| Build a bounding box that contains all three of the given points;

    BoundingBox3d.hull3 p1 p2 p3

is equivalent to

    BoundingBox3d.hull p1 [ p2, p3 ]

but is more efficient.

-}
hull3 : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> BoundingBox3d units coordinates
hull3 firstPoint secondPoint thirdPoint =
    let
        x1 =
            Point3d.xCoordinate firstPoint

        y1 =
            Point3d.yCoordinate firstPoint

        z1 =
            Point3d.zCoordinate firstPoint

        x2 =
            Point3d.xCoordinate secondPoint

        y2 =
            Point3d.yCoordinate secondPoint

        z2 =
            Point3d.zCoordinate secondPoint

        x3 =
            Point3d.xCoordinate thirdPoint

        y3 =
            Point3d.yCoordinate thirdPoint

        z3 =
            Point3d.zCoordinate thirdPoint
    in
    Types.BoundingBox3d
        { minX = Quantity.min x1 (Quantity.min x2 x3)
        , maxX = Quantity.max x1 (Quantity.max x2 x3)
        , minY = Quantity.min y1 (Quantity.min y2 y3)
        , maxY = Quantity.max y1 (Quantity.max y2 y3)
        , minZ = Quantity.min z1 (Quantity.min z2 z3)
        , maxZ = Quantity.max z1 (Quantity.max z2 z3)
        }


{-| Construct a bounding box containing all _N_ points in the given list. If the
list is empty, returns `Nothing`. If you know you have at least one point, you
can use [`hull`](#hull) instead.
-}
hullN : List (Point3d units coordinates) -> Maybe (BoundingBox3d units coordinates)
hullN points =
    case points of
        first :: rest ->
            Just (hull first rest)

        [] ->
            Nothing


{-| Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
-}
hullOfN : (a -> Point3d units coordinates) -> List a -> Maybe (BoundingBox3d units coordinates)
hullOfN getBoundingBox items =
    case items of
        first :: rest ->
            Just (hullOf getBoundingBox first rest)

        [] ->
            Nothing


{-| Find the bounding box containing one or more input boxes; works much
like [`hull`](#hull). If you need to handle the case of zero input boxes, see
[`aggregateN`](#aggregateN).
-}
aggregate : BoundingBox3d units coordinates -> List (BoundingBox3d units coordinates) -> BoundingBox3d units coordinates
aggregate first rest =
    let
        b1 =
            extrema first
    in
    aggregateHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ rest


aggregateHelp : Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> List (BoundingBox3d units coordinates) -> BoundingBox3d units coordinates
aggregateHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ boxes =
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
                (Quantity.min b.minZ currentMinZ)
                (Quantity.max b.maxZ currentMaxZ)
                rest

        [] ->
            Types.BoundingBox3d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                }


{-| Like [`aggregate`](#aggregate), but lets you work on any kind of item as
long as a bounding box can be extracted from it. For example, to get the
bounding box around four triangles:

    BoundingBox3d.aggregateOf Triangle3d.boundingBox
        firstTriangle
        [ secondTriangle
        , thirdTriangle
        , fourthTriangle
        ]

-}
aggregateOf : (a -> BoundingBox3d units coordinates) -> a -> List a -> BoundingBox3d units coordinates
aggregateOf getBoundingBox first rest =
    let
        b1 =
            extrema (getBoundingBox first)
    in
    aggregateOfHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ getBoundingBox rest


aggregateOfHelp : Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> (a -> BoundingBox3d units coordiantes) -> List a -> BoundingBox3d units coordinates
aggregateOfHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ getBoundingBox items =
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
                (Quantity.min b.minZ currentMinZ)
                (Quantity.max b.maxZ currentMaxZ)
                getBoundingBox
                rest

        [] ->
            Types.BoundingBox3d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                }


{-| Build a bounding box that contains both given bounding boxes.

    firstBox =
        BoundingBox3d.from
            (Point3d.meters 1 2 0)
            (Point3d.meters 4 3 5)

    secondBox =
        BoundingBox3d.from
            (Point3d.meters -2 4 -1)
            (Point3d.meters 2 5 0)

    BoundingBox3d.union firstBox secondBox
    --> BoundingBox3d.from
    -->     (Point3d.meters -2 2 -1)
    -->     (Point3d.meters 4 5 5)

(Note that this is not strictly speaking a 'union' in the precise mathematical
sense.)

-}
union : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
union firstBox secondBox =
    let
        b1 =
            extrema firstBox

        b2 =
            extrema secondBox
    in
    Types.BoundingBox3d
        { minX = Quantity.min b1.minX b2.minX
        , maxX = Quantity.max b1.maxX b2.maxX
        , minY = Quantity.min b1.minY b2.minY
        , maxY = Quantity.max b1.maxY b2.maxY
        , minZ = Quantity.min b1.minZ b2.minZ
        , maxZ = Quantity.max b1.maxZ b2.maxZ
        }


{-| Build a bounding box that contains all three of the given bounding boxes;

    BoundingBox3d.aggregate3 b1 b2 b3

is equivalent to

    BoundingBox3d.aggregate b1 [ b2, b3 ]

but is more efficient.

-}
aggregate3 : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
aggregate3 firstBox secondBox thirdBox =
    let
        b1 =
            extrema firstBox

        b2 =
            extrema secondBox

        b3 =
            extrema thirdBox
    in
    Types.BoundingBox3d
        { minX = Quantity.min b1.minX (Quantity.min b2.minX b3.minX)
        , maxX = Quantity.max b1.maxX (Quantity.max b2.maxX b3.maxX)
        , minY = Quantity.min b1.minY (Quantity.min b2.minY b3.minY)
        , maxY = Quantity.max b1.maxY (Quantity.max b2.maxY b3.maxY)
        , minZ = Quantity.min b1.minZ (Quantity.min b2.minZ b3.minZ)
        , maxZ = Quantity.max b1.maxZ (Quantity.max b2.maxZ b3.maxZ)
        }


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`. If you know you have at least one bounding
box, you can use [`aggregate`](#aggregate) instead.
-}
aggregateN : List (BoundingBox3d units coordinates) -> Maybe (BoundingBox3d units coordinates)
aggregateN boxes =
    case boxes of
        first :: rest ->
            Just (aggregate first rest)

        [] ->
            Nothing


{-| Combination of [`aggregateOf`](#aggregateOf) and
[`aggregateN`](#aggregateN).
-}
aggregateOfN : (a -> BoundingBox3d units coordinates) -> List a -> Maybe (BoundingBox3d units coordinates)
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
at : Quantity Float (Rate units2 units1) -> BoundingBox3d units1 coordinates -> BoundingBox3d units2 coordinates
at rate (Types.BoundingBox3d boundingBox) =
    fromExtrema
        { minX = Quantity.at rate boundingBox.minX
        , maxX = Quantity.at rate boundingBox.maxX
        , minY = Quantity.at rate boundingBox.minY
        , maxY = Quantity.at rate boundingBox.maxY
        , minZ = Quantity.at rate boundingBox.minZ
        , maxZ = Quantity.at rate boundingBox.maxZ
        }


{-| Convert a bounding box from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> BoundingBox3d units1 coordinates -> BoundingBox3d units2 coordinates
at_ rate boundingBox =
    at (Quantity.inverse rate) boundingBox


{-| Get the minimum and maximum X, Y and Z values of a bounding box in a single
record.

    BoundingBox3d.extrema exampleBox
    --> { minX = Length.meters -2
    --> , maxX = Length.meters 2
    --> , minY = Length.meters 2
    --> , maxY = Length.meters 5
    --> , minZ = Length.meters 3
    --> , maxZ = Length.meters 4
    --> }

Can be useful when combined with record destructuring, e.g.

    { minX, maxX, minY, maxY, minZ, maxZ } =
        BoundingBox3d.extrema exampleBox

-}
extrema :
    BoundingBox3d units coordinates
    ->
        { minX : Quantity Float units
        , maxX : Quantity Float units
        , minY : Quantity Float units
        , maxY : Quantity Float units
        , minZ : Quantity Float units
        , maxZ : Quantity Float units
        }
extrema (Types.BoundingBox3d boundingBoxExtrema) =
    boundingBoxExtrema


{-| -}
minX : BoundingBox3d units coordinates -> Quantity Float units
minX (Types.BoundingBox3d boundingBox) =
    boundingBox.minX


{-| -}
maxX : BoundingBox3d units coordinates -> Quantity Float units
maxX (Types.BoundingBox3d boundingBox) =
    boundingBox.maxX


{-| -}
minY : BoundingBox3d units coordinates -> Quantity Float units
minY (Types.BoundingBox3d boundingBox) =
    boundingBox.minY


{-| -}
maxY : BoundingBox3d units coordinates -> Quantity Float units
maxY (Types.BoundingBox3d boundingBox) =
    boundingBox.maxY


{-| -}
minZ : BoundingBox3d units coordinates -> Quantity Float units
minZ (Types.BoundingBox3d boundingBox) =
    boundingBox.minZ


{-| -}
maxZ : BoundingBox3d units coordinates -> Quantity Float units
maxZ (Types.BoundingBox3d boundingBox) =
    boundingBox.maxZ


{-| Get the X, Y and Z dimensions (widths) of a bounding box.

    BoundingBox3d.dimensions exampleBox
    --> ( Length.meters 4
    --> , Length.meters 3
    --> , Length.meters 1
    --> )

-}
dimensions : BoundingBox3d units coordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units )
dimensions boundingBox =
    ( maxX boundingBox |> Quantity.minus (minX boundingBox)
    , maxY boundingBox |> Quantity.minus (minY boundingBox)
    , maxZ boundingBox |> Quantity.minus (minZ boundingBox)
    )


{-| Get the median (central) X value of a bounding box.
-}
midX : BoundingBox3d units coordinates -> Quantity Float units
midX (Types.BoundingBox3d boundingBox) =
    Quantity.interpolateFrom boundingBox.minX boundingBox.maxX 0.5


{-| Get the median (central) Y value of a bounding box.
-}
midY : BoundingBox3d units coordinates -> Quantity Float units
midY (Types.BoundingBox3d boundingBox) =
    Quantity.interpolateFrom boundingBox.minY boundingBox.maxY 0.5


{-| Get the median (central) Z value of a bounding box.
-}
midZ : BoundingBox3d units coordinates -> Quantity Float units
midZ (Types.BoundingBox3d boundingBox) =
    Quantity.interpolateFrom boundingBox.minZ boundingBox.maxZ 0.5


{-| Get the point at the center of a bounding box.

    BoundingBox3d.centerPoint exampleBox
    --> Point3d.meters 0 3.5 3.5

-}
centerPoint : BoundingBox3d units coordinates -> Point3d units coordinates
centerPoint boundingBox =
    Point3d.xyz (midX boundingBox) (midY boundingBox) (midZ boundingBox)


{-| Get the range of X values contained by a bounding box.
-}
xInterval : BoundingBox3d units coordinates -> Interval Float units
xInterval boundingBox =
    Interval.from (minX boundingBox) (maxX boundingBox)


{-| Get the range of Y values contained by a bounding box.
-}
yInterval : BoundingBox3d units coordinates -> Interval Float units
yInterval boundingBox =
    Interval.from (minY boundingBox) (maxY boundingBox)


{-| Get the range of Y values contained by a bounding box.
-}
zInterval : BoundingBox3d units coordinates -> Interval Float units
zInterval boundingBox =
    Interval.from (minZ boundingBox) (maxZ boundingBox)


{-| Convert a bounding box to a pair of X and Y intervals.
-}
intervals :
    BoundingBox3d units coordinates
    -> ( Interval Float units, Interval Float units, Interval Float units )
intervals boundingBox =
    ( xInterval boundingBox, yInterval boundingBox, zInterval boundingBox )


{-| Check if a bounding box contains a particular point.
-}
contains : Point3d units coordinates -> BoundingBox3d units coordinates -> Bool
contains point boundingBox =
    (Point3d.xCoordinate point |> Quantity.greaterThanOrEqualTo (minX boundingBox))
        && (Point3d.xCoordinate point |> Quantity.lessThanOrEqualTo (maxX boundingBox))
        && (Point3d.yCoordinate point |> Quantity.greaterThanOrEqualTo (minY boundingBox))
        && (Point3d.yCoordinate point |> Quantity.lessThanOrEqualTo (maxY boundingBox))
        && (Point3d.zCoordinate point |> Quantity.greaterThanOrEqualTo (minZ boundingBox))
        && (Point3d.zCoordinate point |> Quantity.lessThanOrEqualTo (maxZ boundingBox))


{-| Test if two boxes touch or overlap at all (have any points in common);

    BoundingBox3d.intersects firstBox secondBox

is equivalent to

    BoundingBox3d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

-}
intersects : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
intersects other boundingBox =
    (minX boundingBox |> Quantity.lessThanOrEqualTo (maxX other))
        && (maxX boundingBox |> Quantity.greaterThanOrEqualTo (minX other))
        && (minY boundingBox |> Quantity.lessThanOrEqualTo (maxY other))
        && (maxY boundingBox |> Quantity.greaterThanOrEqualTo (minY other))
        && (minZ boundingBox |> Quantity.lessThanOrEqualTo (maxZ other))
        && (maxZ boundingBox |> Quantity.greaterThanOrEqualTo (minZ other))


{-| Check two boxes overlap by at least the given amount. For example, you could
implement a tolerant collision check (one that only returns true if the boxes
overlap by at least a millimeter, and ignores boxes that just barely touch each
other) as

    boxesCollide firstBox secondBox =
        BoundingBox3d.overlappingByAtLeast
            (Length.millimeters 1)
            firstBox
            secondBox

Overlap is defined as the _minimum_ distance one box would have to move so that
it did not touch the other. Boxes that just touch are considered to have an
overlap of zero, so

    BoundingBox3d.overlappingByAtLeast Quantity.zero
        firstBox
        secondBox

will return true even if the two boxes just touch each other.

-}
overlappingByAtLeast : Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
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

        zOverlap =
            Quantity.min (maxZ firstBox) (maxZ secondBox)
                |> Quantity.minus
                    (Quantity.max (minZ firstBox) (minZ secondBox))

        clampedTolerance =
            Quantity.max tolerance Quantity.zero
    in
    (xOverlap |> Quantity.greaterThanOrEqualTo clampedTolerance)
        && (yOverlap |> Quantity.greaterThanOrEqualTo clampedTolerance)
        && (zOverlap |> Quantity.greaterThanOrEqualTo clampedTolerance)


{-| Check if two boxes are separated by at least the given amount. For example,
to perform clash detection between some objects, you could use `separatedBy` on
those objects' bounding boxes as a quick check to see if the objects had a gap
of at least 1 cm between them:

    safelySeparated firstBox secondBox =
        BoundingBox3d.separatedByAtLeast
            (Length.centimeters 1)
            firstBox
            secondBox

Separation is defined as the _minimum_ distance one box would have to move so
that it touched the other. (Note that this may be a _diagonal_ distance between
corners.) Boxes that just touch are considered to have a separation of zero, so

    BoundingBox3d.separatedByAtLeast Quantity.zero
        firstBox
        secondBox

will return true even if the two boxes just touch each other.

-}
separatedByAtLeast : Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
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

        zSeparation =
            Quantity.max (minZ firstBox) (minZ secondBox)
                |> Quantity.minus
                    (Quantity.min (maxZ firstBox) (maxZ secondBox))
    in
    if
        (xSeparation |> Quantity.greaterThanOrEqualTo Quantity.zero)
            || (ySeparation |> Quantity.greaterThanOrEqualTo Quantity.zero)
            || (zSeparation |> Quantity.greaterThanOrEqualTo Quantity.zero)
    then
        let
            dX =
                Quantity.max xSeparation Quantity.zero

            dY =
                Quantity.max ySeparation Quantity.zero

            dZ =
                Quantity.max zSeparation Quantity.zero
        in
        Quantity.squared dX
            |> Quantity.plus (Quantity.squared dY)
            |> Quantity.plus (Quantity.squared dZ)
            |> Quantity.greaterThanOrEqualTo (Quantity.squared clampedTolerance)

    else
        False


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).

    outerBox =
        BoundingBox3d.from
            (Point3d.meters 0 0 0)
            (Point3d.meters 10 10 10)

    innerBox =
        BoundingBox3d.from
            (Point3d.meters 1 3 7)
            (Point3d.meters 5 9 8)

    overlappingBox =
        BoundingBox3d.from
            (Point3d.meters 1 3 7)
            (Point3d.meters 5 12 8)

    innerBox |> BoundingBox3d.isContainedIn outerBox
    --> True

    overlappingBox |> BoundingBox3d.isContainedIn outerBox
    --> False

-}
isContainedIn : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
isContainedIn other boundingBox =
    (minX other |> Quantity.lessThanOrEqualTo (minX boundingBox))
        && (maxX boundingBox |> Quantity.lessThanOrEqualTo (maxX other))
        && (minY other |> Quantity.lessThanOrEqualTo (minY boundingBox))
        && (maxY boundingBox |> Quantity.lessThanOrEqualTo (maxY other))
        && (minZ other |> Quantity.lessThanOrEqualTo (minZ boundingBox))
        && (maxZ boundingBox |> Quantity.lessThanOrEqualTo (maxZ other))


{-| Attempt to build a bounding box that contains all points common to both
given bounding boxes. If the given boxes do not overlap, returns `Nothing`.

    firstBox =
        BoundingBox3d.from
            (Point3d.meters 1 2 5)
            (Point3d.meters 4 3 8)

    secondBox =
        BoundingBox3d.from
            (Point3d.meters 2 1 6)
            (Point3d.meters 5 4 7)

    BoundingBox3d.intersection firstBox secondBox
    --> Just <|
    -->     BoundingBox3d.from
    -->         (Point3d.meters 2 2 6)
    -->         (Point3d.meters 4 3 7)

If two boxes just touch along an edge or at a corner, they are still considered
to have an intersection, even though that intersection will have zero volume (at
least one of its dimensions will be zero):

    firstBox =
        BoundingBox3d.from
            (Point3d.meters 0 0 0)
            (Point3d.meters 1 2 3)

    secondBox =
        BoundingBox3d.from
            (Point3d.meters 1 1 1)
            (Point3d.meters 2 3 4)

    BoundingBox3d.intersection firstBox secondBox
    --> Just <|
    -->     BoundingBox3d.from
    -->         (Point3d.meters 1 1 1)
    -->         (Point3d.meters 1 2 3)

-}
intersection : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Maybe (BoundingBox3d units coordinates)
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        Just
            (Types.BoundingBox3d
                { minX = Quantity.max (minX firstBox) (minX secondBox)
                , maxX = Quantity.min (maxX firstBox) (maxX secondBox)
                , minY = Quantity.max (minY firstBox) (minY secondBox)
                , maxY = Quantity.min (maxY firstBox) (maxY secondBox)
                , minZ = Quantity.max (minZ firstBox) (minZ secondBox)
                , maxZ = Quantity.min (maxZ firstBox) (maxZ secondBox)
                }
            )

    else
        Nothing


{-| Scale a bounding box about a given point by a given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
scaleAbout point scale boundingBox =
    let
        x0 =
            Point3d.xCoordinate point

        y0 =
            Point3d.yCoordinate point

        z0 =
            Point3d.zCoordinate point

        scaledMinX =
            Quantity.scaleAbout x0 scale (minX boundingBox)

        scaledMaxX =
            Quantity.scaleAbout x0 scale (maxX boundingBox)

        scaledMinY =
            Quantity.scaleAbout y0 scale (minY boundingBox)

        scaledMaxY =
            Quantity.scaleAbout y0 scale (maxY boundingBox)

        scaledMinZ =
            Quantity.scaleAbout z0 scale (minZ boundingBox)

        scaledMaxZ =
            Quantity.scaleAbout z0 scale (maxZ boundingBox)
    in
    if scale >= 0 then
        Types.BoundingBox3d
            { minX = scaledMinX
            , maxX = scaledMaxX
            , minY = scaledMinY
            , maxY = scaledMaxY
            , minZ = scaledMinZ
            , maxZ = scaledMaxZ
            }

    else
        Types.BoundingBox3d
            { minX = scaledMaxX
            , maxX = scaledMinX
            , minY = scaledMaxY
            , maxY = scaledMinY
            , minZ = scaledMaxZ
            , maxZ = scaledMinZ
            }


{-| Translate a bounding box by a given displacement.
-}
translateBy : Vector3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
translateBy displacement boundingBox =
    let
        dx =
            Vector3d.xComponent displacement

        dy =
            Vector3d.yComponent displacement

        dz =
            Vector3d.zComponent displacement
    in
    Types.BoundingBox3d
        { minX = minX boundingBox |> Quantity.plus dx
        , maxX = maxX boundingBox |> Quantity.plus dx
        , minY = minY boundingBox |> Quantity.plus dy
        , maxY = maxY boundingBox |> Quantity.plus dy
        , minZ = minZ boundingBox |> Quantity.plus dz
        , maxZ = maxZ boundingBox |> Quantity.plus dz
        }


{-| Translate a bounding box in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
translateIn direction distance boundingBox =
    translateBy (Vector3d.withLength distance direction) boundingBox


{-| Offsets boundingBox irrespective of the resulting bounding box is valid or
not.
-}
unsafeOffsetBy : Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
unsafeOffsetBy amount boundingBox =
    Types.BoundingBox3d
        { minX = minX boundingBox |> Quantity.minus amount
        , minY = minY boundingBox |> Quantity.minus amount
        , minZ = minZ boundingBox |> Quantity.minus amount
        , maxX = maxX boundingBox |> Quantity.plus amount
        , maxY = maxY boundingBox |> Quantity.plus amount
        , maxZ = maxZ boundingBox |> Quantity.plus amount
        }


{-| Expand or shrink the given bounding box in all the directions by the given
distance. A positive offset will cause the bounding box to expand and a negative
value will cause it to shrink.

    BoundingBox3d.offsetBy (Length.meters -0.5) exampleBox
    --> Just <|
    -->     BoundingBox3d.fromExtrema
    -->         { minX = Length.meters -1.5
    -->         , maxX = Length.meters 1.5
    -->         , minY = Length.meters 2.5
    -->         , maxY = Length.meters 4.5
    -->         , minZ = Length.meters 3.5
    -->         , maxZ = Length.meters 3.5
    -->         }

Returns `Nothing` if the offset is negative and large enough to cause the
bounding box to vanish (that is, if the offset is larger than half the height or
half the width of the bounding box, whichever is less):

    BoundingBox3d.offsetBy (Length.meters -1) exampleBox
    --> Nothing

If you only want to expand a bounding box, you can use
[`expandBy`](BoundingBox3d#expandBy) instead (which does not return a `Maybe`).

-}
offsetBy : Quantity Float units -> BoundingBox3d units coordinates -> Maybe (BoundingBox3d units coordinates)
offsetBy amount boundingBox =
    let
        ( width, height, depth ) =
            dimensions boundingBox

        minValidOffset =
            Quantity.multiplyBy -0.5
                (Quantity.min depth (Quantity.min width height))
    in
    if amount |> Quantity.greaterThan minValidOffset then
        Just <| unsafeOffsetBy amount boundingBox

    else
        Nothing


{-| Expand the given bounding box in all directions by the given offset:

    BoundingBox3d.expandBy (Length.meters 3) exampleBox
    --> BoundingBox3d.fromExtrema
    -->     { minX = Length.meters -5
    -->     , maxX = Length.meters 5
    -->     , minY = Length.meters -1
    -->     , maxY = Length.meters 8
    -->     , minZ = Length.meters 0
    -->     , maxZ = Length.meters 7
    -->     }

Negative offsets will be treated as positive (the absolute value will be used),
so the resulting box will always be at least as large as the original. If you
need to be able to contract a bounding box, use
[`offsetBy`](BoundingBox3d#offsetBy) instead.

-}
expandBy : Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
expandBy amount boundingBox =
    unsafeOffsetBy (Quantity.abs amount) boundingBox


{-| Create a [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for points within a given bounding box.
-}
randomPoint : BoundingBox3d units coordinates -> Generator (Point3d units coordinates)
randomPoint boundingBox =
    let
        ( x, y, z ) =
            intervals boundingBox
    in
    Random.map3 Point3d.xyz
        (Interval.randomValue x)
        (Interval.randomValue y)
        (Interval.randomValue z)
