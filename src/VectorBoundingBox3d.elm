--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module VectorBoundingBox3d exposing
    ( VectorBoundingBox3d
    , from, singleton, xyz, fromIntervals
    , union, intersection
    , hull, hull3, hullN, hullOf, hullOfN
    , aggregate, aggregate3, aggregateN, aggregateOf, aggregateOfN
    , xInterval, yInterval, zInterval, intervals, length
    , contains, isContainedIn, intersects
    , expandBy
    , interpolate
    , at, at_
    , randomVector
    )

{-| A `VectorBoundingBox3d` is a version of a `BoundingBox3d` that contains
vectors instead of points. All functions behave like their `BoundingBox3d`
counterparts; see the [`BoundingBox3d` docs and examples](BoundingBox3d) for
details.

@docs VectorBoundingBox3d


# Constructors

@docs from, singleton, xyz, fromIntervals


## Booleans

@docs union, intersection


## Hull

Functions for building bounding boxes containing several vectors.

@docs hull, hull3, hullN, hullOf, hullOfN


## Aggregation

Functions for combining several bounding boxes into one bounding box that
contains all of the input boxes.

@docs aggregate, aggregate3, aggregateN, aggregateOf, aggregateOfN


# Properties

@docs xInterval, yInterval, zInterval, intervals, length


# Queries

@docs contains, isContainedIn, intersects


# Transformations

@docs expandBy


# Interpolation

@docs interpolate


# Unit conversions

@docs at, at_


# Random vector generation

@docs randomVector

-}

import Float.Extra as Float
import Geometry.Types as Types
import Quantity exposing (Quantity(..), Rate)
import Quantity.Extra as Quantity
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import Vector3d exposing (Vector3d)


{-| -}
type alias VectorBoundingBox3d units coordinates =
    Types.VectorBoundingBox3d units coordinates


{-| Construct a bounding box containing the two vectors. The vectors can be
given in any order and don't have to represent the 'primary' diagonal of the
bounding box.
-}
from : Vector3d units coordinates -> Vector3d units coordinates -> VectorBoundingBox3d units coordinates
from firstVector secondVector =
    let
        (Types.Vector3d v1) =
            firstVector

        (Types.Vector3d v2) =
            secondVector

        x1 =
            v1.x

        y1 =
            v1.y

        z1 =
            v1.z

        x2 =
            v2.x

        y2 =
            v2.y

        z2 =
            v2.z
    in
    Types.VectorBoundingBox3d
        { minX = min x1 x2
        , maxX = max x1 x2
        , minY = min y1 y2
        , maxY = max y1 y2
        , minZ = min z1 z2
        , maxZ = max z1 z2
        }


{-| Construct a zero-width bounding box containing a single vector.
-}
singleton : Vector3d units coordinates -> VectorBoundingBox3d units coordinates
singleton vector =
    let
        (Types.Vector3d { x, y, z }) =
            vector
    in
    Types.VectorBoundingBox3d
        { minX = x
        , maxX = x
        , minY = y
        , maxY = y
        , minZ = z
        , maxZ = z
        }


{-| Construct a bounding box from separate X and Y [intervals](https://package.elm-lang.org/packages/ianmackenzie/elm-units-interval/latest/).
-}
xyz : Interval Float units -> Interval Float units -> Interval Float units -> VectorBoundingBox3d units coordinates
xyz givenXInterval givenYInterval givenZInterval =
    let
        ( Quantity minX, Quantity maxX ) =
            Interval.endpoints givenXInterval

        ( Quantity minY, Quantity maxY ) =
            Interval.endpoints givenYInterval

        ( Quantity minZ, Quantity maxZ ) =
            Interval.endpoints givenZInterval
    in
    Types.VectorBoundingBox3d
        { minX = minX
        , maxX = maxX
        , minY = minY
        , maxY = maxY
        , minZ = minZ
        , maxZ = maxZ
        }


{-| Construct a bounding box from a tuple of X, Y and Z intervals.
-}
fromIntervals :
    ( Interval Float units, Interval Float units, Interval Float units )
    -> VectorBoundingBox3d units coordinates
fromIntervals ( givenXInterval, givenYInterval, givenZInterval ) =
    xyz givenXInterval givenYInterval givenZInterval


{-| Find the bounding box containing one or more input vectors:

    VectorBoundingBox3d.hull v1 [ v2, v3, v4 ]

See also [`hullN`](#hullN).

-}
hull : Vector3d units coordinates -> List (Vector3d units coordinates) -> VectorBoundingBox3d units coordinates
hull first rest =
    let
        (Types.Vector3d { x, y, z }) =
            first
    in
    hullHelp x x y y z z rest


hullHelp : Float -> Float -> Float -> Float -> Float -> Float -> List (Vector3d units coordinates) -> VectorBoundingBox3d units coordinates
hullHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ vectors =
    case vectors of
        next :: rest ->
            let
                (Types.Vector3d { x, y, z }) =
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
            Types.VectorBoundingBox3d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                }


{-| Like [`hull`](#hull), but lets you work on any kind of item as long as a
vector can be extracted from it.
-}
hullOf : (a -> Vector3d units coordinates) -> a -> List a -> VectorBoundingBox3d units coordinates
hullOf getVector first rest =
    let
        (Types.Vector3d { x, y, z }) =
            getVector first
    in
    hullOfHelp x x y y z z getVector rest


hullOfHelp : Float -> Float -> Float -> Float -> Float -> Float -> (a -> Vector3d units coordinates) -> List a -> VectorBoundingBox3d units coordinates
hullOfHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ getVector list =
    case list of
        next :: rest ->
            let
                (Types.Vector3d { x, y, z }) =
                    getVector next
            in
            hullOfHelp
                (min x currentMinX)
                (max x currentMaxX)
                (min y currentMinY)
                (max y currentMaxY)
                (min z currentMinZ)
                (max z currentMaxZ)
                getVector
                rest

        [] ->
            Types.VectorBoundingBox3d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                }


{-| Build a bounding box that contains all three of the given vectors;

    VectorBoundingBox3d.hull3 v1 v2 v3

is equivalent to

    VectorBoundingBox3d.hull v1 [ v2, v3 ]

but is more efficient.

-}
hull3 : Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> VectorBoundingBox3d units coordinates
hull3 firstVector secondVector thirdVector =
    let
        (Types.Vector3d v1) =
            firstVector

        (Types.Vector3d v2) =
            secondVector

        (Types.Vector3d v3) =
            thirdVector

        x1 =
            v1.x

        y1 =
            v1.y

        z1 =
            v1.z

        x2 =
            v2.x

        y2 =
            v2.y

        z2 =
            v2.z

        x3 =
            v3.x

        y3 =
            v3.y

        z3 =
            v3.z
    in
    Types.VectorBoundingBox3d
        { minX = min (min x1 x2) x3
        , maxX = max (max x1 x2) x3
        , minY = min (min y1 y2) y3
        , maxY = max (max y1 y2) y3
        , minZ = min (min z1 z2) z3
        , maxZ = max (max z1 z2) z3
        }


{-| Construct a bounding box containing all _N_ vectors in the given list. If the
list is empty, returns `Nothing`. If you know you have at least one point, you
can use [`hull`](#hull) instead.
-}
hullN : List (Vector3d units coordinates) -> Maybe (VectorBoundingBox3d units coordinates)
hullN vectors =
    case vectors of
        first :: rest ->
            Just (hull first rest)

        [] ->
            Nothing


{-| Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
-}
hullOfN : (a -> Vector3d units coordinates) -> List a -> Maybe (VectorBoundingBox3d units coordinates)
hullOfN getVector items =
    case items of
        first :: rest ->
            Just (hullOf getVector first rest)

        [] ->
            Nothing


{-| Find the bounding box containing one or more input boxes; works much like
[`hull`](#hull). See also [`aggregateN`](#aggregateN).
-}
aggregate : VectorBoundingBox3d units coordinates -> List (VectorBoundingBox3d units coordinates) -> VectorBoundingBox3d units coordinates
aggregate first rest =
    let
        (Types.VectorBoundingBox3d b1) =
            first
    in
    aggregateHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ rest


aggregateHelp : Float -> Float -> Float -> Float -> Float -> Float -> List (VectorBoundingBox3d units coordinates) -> VectorBoundingBox3d units coordinates
aggregateHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ boxes =
    case boxes of
        next :: rest ->
            let
                (Types.VectorBoundingBox3d b) =
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
            Types.VectorBoundingBox3d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                }


{-| Like [`aggregate`](#aggregate), but lets you work on any kind of item as
long as a bounding box can be extracted from it.
-}
aggregateOf : (a -> VectorBoundingBox3d units coordinates) -> a -> List a -> VectorBoundingBox3d units coordinates
aggregateOf getBoundingBox first rest =
    let
        (Types.VectorBoundingBox3d b1) =
            getBoundingBox first
    in
    aggregateOfHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ getBoundingBox rest


aggregateOfHelp : Float -> Float -> Float -> Float -> Float -> Float -> (a -> VectorBoundingBox3d units coordiantes) -> List a -> VectorBoundingBox3d units coordinates
aggregateOfHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ getBoundingBox items =
    case items of
        next :: rest ->
            let
                (Types.VectorBoundingBox3d b) =
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
            Types.VectorBoundingBox3d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                }


{-| Build a bounding box that contains both given bounding boxes. (Note that
this is not strictly speaking a 'union' in the precise mathematical sense.)
-}
union : VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates
union firstBox secondBox =
    let
        (Types.VectorBoundingBox3d b1) =
            firstBox

        (Types.VectorBoundingBox3d b2) =
            secondBox
    in
    Types.VectorBoundingBox3d
        { minX = min b1.minX b2.minX
        , maxX = max b1.maxX b2.maxX
        , minY = min b1.minY b2.minY
        , maxY = max b1.maxY b2.maxY
        , minZ = min b1.minZ b2.minZ
        , maxZ = max b1.maxZ b2.maxZ
        }


{-| Build a bounding box that contains all three of the given bounding boxes;

    VectorBoundingBox3d.aggregate3 b1 b2 b3

is equivalent to

    VectorBoundingBox3d.aggregate b1 [ b2, b3 ]

but is more efficient.

-}
aggregate3 : VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates
aggregate3 firstBox secondBox thirdBox =
    let
        (Types.VectorBoundingBox3d b1) =
            firstBox

        (Types.VectorBoundingBox3d b2) =
            secondBox

        (Types.VectorBoundingBox3d b3) =
            thirdBox
    in
    Types.VectorBoundingBox3d
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
aggregateN : List (VectorBoundingBox3d units coordinates) -> Maybe (VectorBoundingBox3d units coordinates)
aggregateN boxes =
    case boxes of
        first :: rest ->
            Just (aggregate first rest)

        [] ->
            Nothing


{-| Combination of [`aggregateOf`](#aggregateOf) and [`aggregateN`](#aggregateN).
-}
aggregateOfN : (a -> VectorBoundingBox3d units coordinates) -> List a -> Maybe (VectorBoundingBox3d units coordinates)
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
at : Quantity Float (Rate units2 units1) -> VectorBoundingBox3d units1 coordinates -> VectorBoundingBox3d units2 coordinates
at rate boundingBox =
    let
        (Quantity r) =
            rate

        (Types.VectorBoundingBox3d b) =
            boundingBox
    in
    if r >= 0 then
        Types.VectorBoundingBox3d
            { minX = r * b.minX
            , maxX = r * b.maxX
            , minY = r * b.minY
            , maxY = r * b.maxY
            , minZ = r * b.minZ
            , maxZ = r * b.maxZ
            }

    else
        Types.VectorBoundingBox3d
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
at_ : Quantity Float (Rate units1 units2) -> VectorBoundingBox3d units1 coordinates -> VectorBoundingBox3d units2 coordinates
at_ rate boundingBox =
    at (Quantity.inverse rate) boundingBox


{-| Get the range of X values contained by a bounding box.
-}
xInterval : VectorBoundingBox3d units coordinates -> Interval Float units
xInterval boundingBox =
    let
        (Types.VectorBoundingBox3d b) =
            boundingBox
    in
    Interval.from (Quantity b.minX) (Quantity b.maxX)


{-| Get the range of Y values contained by a bounding box.
-}
yInterval : VectorBoundingBox3d units coordinates -> Interval Float units
yInterval boundingBox =
    let
        (Types.VectorBoundingBox3d b) =
            boundingBox
    in
    Interval.from (Quantity b.minY) (Quantity b.maxY)


{-| Get the range of Z values contained by a bounding box.
-}
zInterval : VectorBoundingBox3d units coordinates -> Interval Float units
zInterval boundingBox =
    let
        (Types.VectorBoundingBox3d b) =
            boundingBox
    in
    Interval.from (Quantity b.minZ) (Quantity b.maxZ)


{-| Convert a bounding box to a pair of X and Y intervals.
-}
intervals : VectorBoundingBox3d units coordinates -> ( Interval Float units, Interval Float units, Interval Float units )
intervals boundingBox =
    ( xInterval boundingBox, yInterval boundingBox, zInterval boundingBox )


{-| Get the range of lengths of vectors contained in a given bounding box.
-}
length : VectorBoundingBox3d units coordinates -> Interval Float units
length boundingBox =
    let
        ( Quantity xMin, Quantity xMax ) =
            Interval.endpoints (Interval.abs (xInterval boundingBox))

        ( Quantity yMin, Quantity yMax ) =
            Interval.endpoints (Interval.abs (yInterval boundingBox))

        ( Quantity zMin, Quantity zMax ) =
            Interval.endpoints (Interval.abs (zInterval boundingBox))

        minLength =
            Quantity (sqrt (xMin * xMin + yMin * yMin + zMin * zMin))

        maxLength =
            Quantity (sqrt (xMax * xMax + yMax * yMax + zMax * zMax))
    in
    Interval.from minLength maxLength


{-| Check if a bounding box contains a particular point.
-}
contains : Vector3d units coordinates -> VectorBoundingBox3d units coordinates -> Bool
contains vector boundingBox =
    let
        (Types.Vector3d { x, y, z }) =
            vector

        (Types.VectorBoundingBox3d b) =
            boundingBox
    in
    ((x >= b.minX) && (x <= b.maxX))
        && ((y >= b.minY) && (y <= b.maxY))
        && ((z >= b.minZ) && (z <= b.maxZ))


{-| Test if two boxes touch or overlap at all (have any points in common);

    VectorBoundingBox3d.intersects firstBox secondBox

is equivalent to

    VectorBoundingBox3d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

-}
intersects : VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates -> Bool
intersects other boundingBox =
    let
        (Types.VectorBoundingBox3d b2) =
            other

        (Types.VectorBoundingBox3d b1) =
            boundingBox
    in
    ((b1.minX <= b2.maxX) && (b1.maxX >= b2.minX))
        && ((b1.minY <= b2.maxY) && (b1.maxY >= b2.minY))
        && ((b1.minZ <= b2.maxZ) && (b1.maxZ >= b2.minZ))


overlappingByAtLeast : Quantity Float units -> VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates -> Bool
overlappingByAtLeast tolerance firstBox secondBox =
    let
        (Types.VectorBoundingBox3d b1) =
            firstBox

        (Types.VectorBoundingBox3d b2) =
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


separatedByAtLeast : Quantity Float units -> VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates -> Bool
separatedByAtLeast tolerance firstBox secondBox =
    let
        (Types.VectorBoundingBox3d b1) =
            firstBox

        (Types.VectorBoundingBox3d b2) =
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
-}
isContainedIn :
    VectorBoundingBox3d units coordinates
    -> VectorBoundingBox3d units coordinates
    -> Bool
isContainedIn other boundingBox =
    let
        (Types.VectorBoundingBox3d b2) =
            other

        (Types.VectorBoundingBox3d b1) =
            boundingBox
    in
    ((b2.minX <= b1.minX) && (b1.maxX <= b2.maxX))
        && ((b2.minY <= b1.minY) && (b1.maxY <= b2.maxY))
        && ((b2.minZ <= b1.minZ) && (b1.maxZ <= b2.maxZ))


{-| Attempt to build a bounding box that contains all vectors common to both
given bounding boxes. If the given boxes do not intersect, returns `Nothing`.

If two boxes just touch along an edge or at a corner, they are still considered
to have an intersection, even though that intersection will have zero area (at
least one of its dimensions will be zero).

-}
intersection : VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates -> Maybe (VectorBoundingBox3d units coordinates)
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        let
            (Types.VectorBoundingBox3d b1) =
                firstBox

            (Types.VectorBoundingBox3d b2) =
                secondBox
        in
        Just
            (Types.VectorBoundingBox3d
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


{-| Expand the given bounding box in all directions by the given offset.
Negative offsets will be treated as positive (the absolute value will be used),
so the resulting box will always be at least as large as the original.
-}
expandBy : Quantity Float units -> VectorBoundingBox3d units coordinates -> VectorBoundingBox3d units coordinates
expandBy amount boundingBox =
    let
        (Quantity dGiven) =
            amount

        d =
            abs dGiven

        (Types.VectorBoundingBox3d b) =
            boundingBox
    in
    Types.VectorBoundingBox3d
        { minX = b.minX - d
        , minY = b.minY - d
        , minZ = b.minZ - d
        , maxX = b.maxX + d
        , maxY = b.maxY + d
        , maxZ = b.maxZ + d
        }


{-| Interpolate within a bounding box based on parameter values which range from
0 to 1.
-}
interpolate : VectorBoundingBox3d units coordinates -> Float -> Float -> Float -> Vector3d units coordinates
interpolate boundingBox u v w =
    let
        (Types.VectorBoundingBox3d b) =
            boundingBox
    in
    Types.Vector3d
        { x = Float.interpolateFrom b.minX b.maxX u
        , y = Float.interpolateFrom b.minY b.maxY v
        , z = Float.interpolateFrom b.minZ b.maxZ w
        }


{-| Create a [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for vectors within a given bounding box.
-}
randomVector : VectorBoundingBox3d units coordinates -> Generator (Vector3d units coordinates)
randomVector boundingBox =
    let
        parameterValue =
            Random.float 0 1
    in
    Random.map3 (interpolate boundingBox) parameterValue parameterValue parameterValue
