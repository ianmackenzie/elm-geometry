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
    , interpolate
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


# Interpolation

@docs interpolate


# Transformations

@docs scaleAbout, translateBy, translateIn, expandBy, offsetBy


# Unit conversions

@docs at, at_


# Random point generation

@docs randomPoint

-}

import Direction3d exposing (Direction3d)
import Float.Extra as Float
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
        (Types.Point3d p1) =
            firstPoint

        (Types.Point3d p2) =
            secondPoint

        x1 =
            p1.x

        y1 =
            p1.y

        z1 =
            p1.z

        x2 =
            p2.x

        y2 =
            p2.y

        z2 =
            p2.z
    in
    Types.BoundingBox3d
        { minX = min x1 x2
        , maxX = max x1 x2
        , minY = min y1 y2
        , maxY = max y1 y2
        , minZ = min z1 z2
        , maxZ = max z1 z2
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
    let
        (Quantity x1) =
            given.minX

        (Quantity x2) =
            given.maxX

        (Quantity y1) =
            given.minY

        (Quantity y2) =
            given.maxY

        (Quantity z1) =
            given.minZ

        (Quantity z2) =
            given.maxZ
    in
    Types.BoundingBox3d
        { minX = min x1 x2
        , maxX = max x1 x2
        , minY = min y1 y2
        , maxY = max y1 y2
        , minZ = min z1 z2
        , maxZ = max z1 z2
        }


{-| Construct a bounding box given its overall dimensions (length, width,
height) and center point.
-}
withDimensions :
    ( Quantity Float units, Quantity Float units, Quantity Float units )
    -> Point3d units coordinates
    -> BoundingBox3d units coordinates
withDimensions givenDimensions givenCenterPoint =
    let
        (Types.Point3d { x, y, z }) =
            givenCenterPoint

        ( Quantity dx, Quantity dy, Quantity dz ) =
            givenDimensions

        halfDx =
            abs dx / 2

        halfDy =
            abs dy / 2

        halfDz =
            abs dz / 2
    in
    Types.BoundingBox3d
        { minX = x - halfDx
        , maxX = x + halfDx
        , minY = y - halfDy
        , maxY = y + halfDy
        , minZ = z - halfDz
        , maxZ = z + halfDz
        }


{-| Construct a zero-width bounding box containing a single point.
-}
singleton : Point3d units coordinates -> BoundingBox3d units coordinates
singleton point =
    let
        (Types.Point3d { x, y, z }) =
            point
    in
    Types.BoundingBox3d
        { minX = x
        , maxX = x
        , minY = y
        , maxY = y
        , minZ = z
        , maxZ = z
        }


{-| Construct a bounding box from separate X, Y and Z [intervals](https://package.elm-lang.org/packages/ianmackenzie/elm-units-interval/latest/).
-}
xyz :
    Interval Float units
    -> Interval Float units
    -> Interval Float units
    -> BoundingBox3d units coordinates
xyz givenXInterval givenYInterval givenZInterval =
    let
        ( Quantity x1, Quantity x2 ) =
            Interval.endpoints givenXInterval

        ( Quantity y1, Quantity y2 ) =
            Interval.endpoints givenYInterval

        ( Quantity z1, Quantity z2 ) =
            Interval.endpoints givenZInterval
    in
    Types.BoundingBox3d
        { minX = x1
        , maxX = x2
        , minY = y1
        , maxY = y2
        , minZ = z1
        , maxZ = z2
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
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
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
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
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
        (Types.Point3d p1) =
            firstPoint

        (Types.Point3d p2) =
            secondPoint

        (Types.Point3d p3) =
            thirdPoint

        x1 =
            p1.x

        y1 =
            p1.y

        z1 =
            p1.z

        x2 =
            p2.x

        y2 =
            p2.y

        z2 =
            p2.z

        x3 =
            p3.x

        y3 =
            p3.y

        z3 =
            p3.z
    in
    Types.BoundingBox3d
        { minX = min (min x1 x2) x3
        , maxX = max (max x1 x2) x3
        , minY = min (min y1 y2) y3
        , maxY = max (max y1 y2) y3
        , minZ = min (min z1 z2) z3
        , maxZ = max (max z1 z2) z3
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
        (Types.BoundingBox3d b1) =
            first
    in
    aggregateHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ rest


aggregateHelp : Float -> Float -> Float -> Float -> Float -> Float -> List (BoundingBox3d units coordinates) -> BoundingBox3d units coordinates
aggregateHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ boxes =
    case boxes of
        next :: rest ->
            let
                (Types.BoundingBox3d b) =
                    next
            in
            aggregateHelp
                (min b.minX currentMinX)
                (max b.maxX currentMaxX)
                (min b.minY currentMinY)
                (max b.maxY currentMaxY)
                (min b.minZ currentMinZ)
                (max b.maxZ currentMaxZ)
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
        (Types.BoundingBox3d b1) =
            getBoundingBox first
    in
    aggregateOfHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ getBoundingBox rest


aggregateOfHelp : Float -> Float -> Float -> Float -> Float -> Float -> (a -> BoundingBox3d units coordinates) -> List a -> BoundingBox3d units coordinates
aggregateOfHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ getBoundingBox items =
    case items of
        next :: rest ->
            let
                (Types.BoundingBox3d b) =
                    getBoundingBox next
            in
            aggregateOfHelp
                (min b.minX currentMinX)
                (max b.maxX currentMaxX)
                (min b.minY currentMinY)
                (max b.maxY currentMaxY)
                (min b.minZ currentMinZ)
                (max b.maxZ currentMaxZ)
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
        (Types.BoundingBox3d b1) =
            firstBox

        (Types.BoundingBox3d b2) =
            secondBox
    in
    Types.BoundingBox3d
        { minX = min b1.minX b2.minX
        , maxX = max b1.maxX b2.maxX
        , minY = min b1.minY b2.minY
        , maxY = max b1.maxY b2.maxY
        , minZ = min b1.minZ b2.minZ
        , maxZ = max b1.maxZ b2.maxZ
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
        (Types.BoundingBox3d b1) =
            firstBox

        (Types.BoundingBox3d b2) =
            secondBox

        (Types.BoundingBox3d b3) =
            thirdBox
    in
    Types.BoundingBox3d
        { minX = min b1.minX (min b2.minX b3.minX)
        , maxX = max b1.maxX (max b2.maxX b3.maxX)
        , minY = min b1.minY (min b2.minY b3.minY)
        , maxY = max b1.maxY (max b2.maxY b3.maxY)
        , minZ = min b1.minZ (min b2.minZ b3.minZ)
        , maxZ = max b1.maxZ (max b2.maxZ b3.maxZ)
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
at rate boundingBox =
    let
        (Quantity r) =
            rate

        (Types.BoundingBox3d b) =
            boundingBox
    in
    if r >= 0 then
        Types.BoundingBox3d
            { minX = r * b.minX
            , maxX = r * b.maxX
            , minY = r * b.minY
            , maxY = r * b.maxY
            , minZ = r * b.minZ
            , maxZ = r * b.maxZ
            }

    else
        Types.BoundingBox3d
            { minX = r * b.maxX
            , maxX = r * b.minX
            , minY = r * b.maxY
            , maxY = r * b.minY
            , minZ = r * b.maxZ
            , maxZ = r * b.minZ
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
extrema boundingBox =
    let
        (Types.BoundingBox3d b) =
            boundingBox
    in
    { minX = Quantity b.minX
    , maxX = Quantity b.maxX
    , minY = Quantity b.minY
    , maxY = Quantity b.maxY
    , minZ = Quantity b.minZ
    , maxZ = Quantity b.maxZ
    }


{-| -}
minX : BoundingBox3d units coordinates -> Quantity Float units
minX (Types.BoundingBox3d boundingBox) =
    Quantity boundingBox.minX


{-| -}
maxX : BoundingBox3d units coordinates -> Quantity Float units
maxX (Types.BoundingBox3d boundingBox) =
    Quantity boundingBox.maxX


{-| -}
minY : BoundingBox3d units coordinates -> Quantity Float units
minY (Types.BoundingBox3d boundingBox) =
    Quantity boundingBox.minY


{-| -}
maxY : BoundingBox3d units coordinates -> Quantity Float units
maxY (Types.BoundingBox3d boundingBox) =
    Quantity boundingBox.maxY


{-| -}
minZ : BoundingBox3d units coordinates -> Quantity Float units
minZ (Types.BoundingBox3d boundingBox) =
    Quantity boundingBox.minZ


{-| -}
maxZ : BoundingBox3d units coordinates -> Quantity Float units
maxZ (Types.BoundingBox3d boundingBox) =
    Quantity boundingBox.maxZ


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
midX boundingBox =
    let
        (Types.BoundingBox3d b) =
            boundingBox

        x1 =
            b.minX

        x2 =
            b.maxX
    in
    Quantity (x1 + 0.5 * (x2 - x1))


{-| Get the median (central) Y value of a bounding box.
-}
midY : BoundingBox3d units coordinates -> Quantity Float units
midY boundingBox =
    let
        (Types.BoundingBox3d b) =
            boundingBox

        y1 =
            b.minY

        y2 =
            b.maxY
    in
    Quantity (y1 + 0.5 * (y2 - y1))


{-| Get the median (central) Z value of a bounding box.
-}
midZ : BoundingBox3d units coordinates -> Quantity Float units
midZ boundingBox =
    let
        (Types.BoundingBox3d b) =
            boundingBox

        z1 =
            b.minZ

        z2 =
            b.maxZ
    in
    Quantity (z1 + 0.5 * (z2 - z1))


{-| Get the point at the center of a bounding box.

    BoundingBox3d.centerPoint exampleBox
    --> Point3d.meters 0 3.5 3.5

-}
centerPoint : BoundingBox3d units coordinates -> Point3d units coordinates
centerPoint boundingBox =
    let
        (Types.BoundingBox3d b) =
            boundingBox

        x1 =
            b.minX

        x2 =
            b.maxX

        y1 =
            b.minY

        y2 =
            b.maxY

        z1 =
            b.minZ

        z2 =
            b.maxZ
    in
    Types.Point3d
        { x = x1 + 0.5 * (x2 - x1)
        , y = y1 + 0.5 * (y2 - y1)
        , z = z1 + 0.5 * (z2 - z1)
        }


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
    let
        (Types.Point3d { x, y, z }) =
            point

        (Types.BoundingBox3d b) =
            boundingBox
    in
    ((x >= b.minX) && (x <= b.maxX))
        && ((y >= b.minY) && (y <= b.maxY))
        && ((z >= b.minZ) && (z <= b.maxZ))


{-| Test if two boxes touch or overlap at all (have any points in common);

    BoundingBox3d.intersects firstBox secondBox

is equivalent to

    BoundingBox3d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

-}
intersects : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
intersects other boundingBox =
    let
        (Types.BoundingBox3d b2) =
            other

        (Types.BoundingBox3d b1) =
            boundingBox
    in
    ((b1.minX <= b2.maxX) && (b1.maxX >= b2.minX))
        && ((b1.minY <= b2.maxY) && (b1.maxY >= b2.minY))
        && ((b1.minZ <= b2.maxZ) && (b1.maxZ >= b2.minZ))


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
        (Types.BoundingBox3d b1) =
            firstBox

        (Types.BoundingBox3d b2) =
            secondBox

        xOverlap =
            min b1.maxX b2.maxX - max b1.minX b2.minX

        yOverlap =
            min b1.maxY b2.maxY - max b1.minY b2.minY

        zOverlap =
            min b1.maxZ b2.maxZ - max b1.minZ b2.minZ

        (Quantity dGiven) =
            tolerance

        d =
            max dGiven 0
    in
    (xOverlap >= d) && (yOverlap >= d) && (zOverlap >= d)


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
        (Types.BoundingBox3d b1) =
            firstBox

        (Types.BoundingBox3d b2) =
            secondBox

        xSeparation =
            max b1.minX b2.minX - min b1.maxX b2.maxX

        ySeparation =
            max b1.minY b2.minY - min b1.maxY b2.maxY

        zSeparation =
            max b1.minZ b2.minZ - min b1.maxZ b2.maxZ

        (Quantity dGiven) =
            tolerance

        d =
            max dGiven 0
    in
    if (xSeparation >= 0) || (ySeparation >= 0) || (zSeparation >= 0) then
        let
            dX =
                max xSeparation 0

            dY =
                max ySeparation 0

            dZ =
                max zSeparation 0
        in
        dX * dX + dY * dY + dZ * dZ >= d * d

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
    let
        (Types.BoundingBox3d b2) =
            other

        (Types.BoundingBox3d b1) =
            boundingBox
    in
    ((b2.minX <= b1.minX) && (b1.maxX <= b2.maxX))
        && ((b2.minY <= b1.minY) && (b1.maxY <= b2.maxY))
        && ((b2.minZ <= b1.minZ) && (b1.maxZ <= b2.maxZ))


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
        let
            (Types.BoundingBox3d b1) =
                firstBox

            (Types.BoundingBox3d b2) =
                secondBox
        in
        Just
            (Types.BoundingBox3d
                { minX = max b1.minX b2.minX
                , maxX = min b1.maxX b2.maxX
                , minY = max b1.minY b2.minY
                , maxY = min b1.maxY b2.maxY
                , minZ = max b1.minZ b2.minZ
                , maxZ = min b1.maxZ b2.maxZ
                }
            )

    else
        Nothing


{-| Interpolate within a bounding box based on parameter values which range from
0 to 1.
-}
interpolate : BoundingBox3d units coordinates -> Float -> Float -> Float -> Point3d units coordinates
interpolate boundingBox u v w =
    let
        (Types.BoundingBox3d b) =
            boundingBox
    in
    Types.Point3d
        { x = Float.interpolateFrom b.minX b.maxX u
        , y = Float.interpolateFrom b.minY b.maxY v
        , z = Float.interpolateFrom b.minZ b.maxZ w
        }


{-| Scale a bounding box about a given point by a given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
scaleAbout point scale boundingBox =
    let
        (Types.Point3d { x, y, z }) =
            point

        (Types.BoundingBox3d b) =
            boundingBox

        x1 =
            x + scale * (b.minX - x)

        x2 =
            x + scale * (b.maxX - x)

        y1 =
            y + scale * (b.minY - y)

        y2 =
            y + scale * (b.maxY - y)

        z1 =
            z + scale * (b.minZ - z)

        z2 =
            z + scale * (b.maxZ - z)
    in
    if scale >= 0 then
        Types.BoundingBox3d
            { minX = x1
            , maxX = x2
            , minY = y1
            , maxY = y2
            , minZ = z1
            , maxZ = z2
            }

    else
        Types.BoundingBox3d
            { minX = x1
            , maxX = x2
            , minY = y1
            , maxY = y2
            , minZ = z1
            , maxZ = z2
            }


{-| Translate a bounding box by a given displacement.
-}
translateBy : Vector3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
translateBy displacement boundingBox =
    let
        (Types.Vector3d { x, y, z }) =
            displacement

        (Types.BoundingBox3d b) =
            boundingBox
    in
    Types.BoundingBox3d
        { minX = b.minX + x
        , maxX = b.maxX + x
        , minY = b.minY + y
        , maxY = b.maxY + y
        , minZ = b.minZ + z
        , maxZ = b.maxZ + z
        }


{-| Translate a bounding box in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
translateIn direction distance boundingBox =
    translateBy (Vector3d.withLength distance direction) boundingBox


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
        (Quantity d) =
            amount

        (Types.BoundingBox3d b) =
            boundingBox

        x1 =
            b.minX - d

        x2 =
            b.maxX + d

        y1 =
            b.minY - d

        y2 =
            b.maxY + d

        z1 =
            b.minZ - d

        z2 =
            b.maxZ + d
    in
    if (x1 <= x2) && (y1 <= y2) && (z1 <= z2) then
        Just <|
            Types.BoundingBox3d
                { minX = x1
                , maxX = x2
                , minY = y1
                , maxY = y2
                , minZ = z1
                , maxZ = z2
                }

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
    let
        (Quantity dGiven) =
            amount

        d =
            abs dGiven

        (Types.BoundingBox3d b) =
            boundingBox
    in
    Types.BoundingBox3d
        { minX = b.minX - d
        , minY = b.minY - d
        , minZ = b.minZ - d
        , maxY = b.maxY + d
        , maxX = b.maxX + d
        , maxZ = b.maxZ + d
        }


{-| Create a [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for points within a given bounding box.
-}
randomPoint : BoundingBox3d units coordinates -> Generator (Point3d units coordinates)
randomPoint boundingBox =
    let
        parameterValue =
            Random.float 0 1
    in
    Random.map3 (interpolate boundingBox) parameterValue parameterValue parameterValue
