--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module VectorBoundingBox4d exposing
    ( VectorBoundingBox4d
    , from, singleton, xyzw
    , union, intersection
    , hull, hull3, hullN, hullOf, hullOfN
    , aggregate, aggregate3, aggregateN, aggregateOf, aggregateOfN
    , xInterval, yInterval, zInterval, wInterval
    , contains, isContainedIn, intersects
    , interpolate
    , at, at_
    )

{-| A `VectorBoundingBox4d` is a version of a `BoundingBox4d` that contains
vectors instead of points. All functions behave like their `BoundingBox4d`
counterparts; see the [`BoundingBox4d` docs and examples](BoundingBox4d) for
details.

@docs VectorBoundingBox4d


# Constructors

@docs from, singleton, xyzw


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

@docs xInterval, yInterval, zInterval, wInterval


# Queries

@docs contains, isContainedIn, intersects


# Interpolation

@docs interpolate


# Unit conversions

@docs at, at_

-}

import Float.Extra as Float
import Geometry.Types as Types
import Quantity exposing (Quantity(..), Rate)
import Quantity.Extra as Quantity
import Quantity.Interval as Interval exposing (Interval)
import Vector4d exposing (Vector4d)


{-| -}
type alias VectorBoundingBox4d units coordinates =
    Types.VectorBoundingBox4d units coordinates


{-| Construct a bounding box containing the two vectors. The vectors can be
given in any order and don't have to represent the 'primary' diagonal of the
bounding box.
-}
from : Vector4d units coordinates -> Vector4d units coordinates -> VectorBoundingBox4d units coordinates
from firstVector secondVector =
    let
        (Types.Vector4d v1) =
            firstVector

        (Types.Vector4d v2) =
            secondVector

        x1 =
            v1.x

        y1 =
            v1.y

        z1 =
            v1.z

        w1 =
            v1.w

        x2 =
            v2.x

        y2 =
            v2.y

        z2 =
            v2.z

        w2 =
            v2.w
    in
    Types.VectorBoundingBox4d
        { minX = min x1 x2
        , maxX = max x1 x2
        , minY = min y1 y2
        , maxY = max y1 y2
        , minZ = min z1 z2
        , maxZ = max z1 z2
        , minW = min w1 w2
        , maxW = max w1 w2
        }


{-| Construct a zero-width bounding box containing a single vector.
-}
singleton : Vector4d units coordinates -> VectorBoundingBox4d units coordinates
singleton vector =
    let
        (Types.Vector4d { x, y, z, w }) =
            vector
    in
    Types.VectorBoundingBox4d
        { minX = x
        , maxX = x
        , minY = y
        , maxY = y
        , minZ = z
        , maxZ = z
        , minW = w
        , maxW = w
        }


{-| Construct a bounding box from separate X and Y [intervals](https://package.elm-lang.org/packages/ianmackenzie/elm-units-interval/latest/).
-}
xyzw : Interval Float units -> Interval Float units -> Interval Float units -> Interval Float units -> VectorBoundingBox4d units coordinates
xyzw givenXInterval givenYInterval givenZInterval givenWInterval =
    let
        ( Quantity minX, Quantity maxX ) =
            Interval.endpoints givenXInterval

        ( Quantity minY, Quantity maxY ) =
            Interval.endpoints givenYInterval

        ( Quantity minZ, Quantity maxZ ) =
            Interval.endpoints givenZInterval

        ( Quantity minW, Quantity maxW ) =
            Interval.endpoints givenWInterval
    in
    Types.VectorBoundingBox4d
        { minX = minX
        , maxX = maxX
        , minY = minY
        , maxY = maxY
        , minZ = minZ
        , maxZ = maxZ
        , minW = minW
        , maxW = maxW
        }


{-| Find the bounding box containing one or more input vectors:

    VectorBoundingBox4d.hull v1 [ v2, v3, v4 ]

See also [`hullN`](#hullN).

-}
hull : Vector4d units coordinates -> List (Vector4d units coordinates) -> VectorBoundingBox4d units coordinates
hull first rest =
    let
        (Types.Vector4d { x, y, z, w }) =
            first
    in
    hullHelp x x y y z z w w rest


hullHelp : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> List (Vector4d units coordinates) -> VectorBoundingBox4d units coordinates
hullHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ currentMinW currentMaxW vectors =
    case vectors of
        next :: rest ->
            let
                (Types.Vector4d { x, y, z, w }) =
                    next
            in
            hullHelp
                (min x currentMinX)
                (max x currentMaxX)
                (min y currentMinY)
                (max y currentMaxY)
                (min z currentMinZ)
                (max z currentMaxZ)
                (min w currentMinW)
                (max w currentMaxW)
                rest

        [] ->
            Types.VectorBoundingBox4d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                , minW = currentMinW
                , maxW = currentMaxW
                }


{-| Like [`hull`](#hull), but lets you work on any kind of item as long as a
vector can be extracted from it.
-}
hullOf : (a -> Vector4d units coordinates) -> a -> List a -> VectorBoundingBox4d units coordinates
hullOf getVector first rest =
    let
        (Types.Vector4d { x, y, z, w }) =
            getVector first
    in
    hullOfHelp x x y y z z w w getVector rest


hullOfHelp : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> (a -> Vector4d units coordinates) -> List a -> VectorBoundingBox4d units coordinates
hullOfHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ currentMinW currentMaxW getVector list =
    case list of
        next :: rest ->
            let
                (Types.Vector4d { x, y, z, w }) =
                    getVector next
            in
            hullOfHelp
                (min x currentMinX)
                (max x currentMaxX)
                (min y currentMinY)
                (max y currentMaxY)
                (min z currentMinZ)
                (max z currentMaxZ)
                (min w currentMinW)
                (max w currentMaxW)
                getVector
                rest

        [] ->
            Types.VectorBoundingBox4d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                , minW = currentMinW
                , maxW = currentMaxW
                }


{-| Build a bounding box that contains all three of the given vectors;

    VectorBoundingBox4d.hull3 v1 v2 v3

is equivalent to

    VectorBoundingBox4d.hull v1 [ v2, v3 ]

but is more efficient.

-}
hull3 : Vector4d units coordinates -> Vector4d units coordinates -> Vector4d units coordinates -> VectorBoundingBox4d units coordinates
hull3 firstVector secondVector thirdVector =
    let
        (Types.Vector4d v1) =
            firstVector

        (Types.Vector4d v2) =
            secondVector

        (Types.Vector4d v3) =
            thirdVector

        x1 =
            v1.x

        y1 =
            v1.y

        z1 =
            v1.z

        w1 =
            v1.w

        x2 =
            v2.x

        y2 =
            v2.y

        z2 =
            v2.z

        w2 =
            v2.w

        x3 =
            v3.x

        y3 =
            v3.y

        z3 =
            v3.z

        w3 =
            v3.w
    in
    Types.VectorBoundingBox4d
        { minX = min (min x1 x2) x3
        , maxX = max (max x1 x2) x3
        , minY = min (min y1 y2) y3
        , maxY = max (max y1 y2) y3
        , minZ = min (min z1 z2) z3
        , maxZ = max (max z1 z2) z3
        , minW = min (min w1 w2) w3
        , maxW = max (max w1 w2) w3
        }


{-| Construct a bounding box containing all _N_ vectors in the given list. If the
list is empty, returns `Nothing`. If you know you have at least one point, you
can use [`hull`](#hull) instead.
-}
hullN : List (Vector4d units coordinates) -> Maybe (VectorBoundingBox4d units coordinates)
hullN vectors =
    case vectors of
        first :: rest ->
            Just (hull first rest)

        [] ->
            Nothing


{-| Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
-}
hullOfN : (a -> Vector4d units coordinates) -> List a -> Maybe (VectorBoundingBox4d units coordinates)
hullOfN getVector items =
    case items of
        first :: rest ->
            Just (hullOf getVector first rest)

        [] ->
            Nothing


{-| Find the bounding box containing one or more input boxes; works much like
[`hull`](#hull). See also [`aggregateN`](#aggregateN).
-}
aggregate : VectorBoundingBox4d units coordinates -> List (VectorBoundingBox4d units coordinates) -> VectorBoundingBox4d units coordinates
aggregate first rest =
    let
        (Types.VectorBoundingBox4d b1) =
            first
    in
    aggregateHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ b1.minW b1.maxW rest


aggregateHelp : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> List (VectorBoundingBox4d units coordinates) -> VectorBoundingBox4d units coordinates
aggregateHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ currentMinW currentMaxW boxes =
    case boxes of
        next :: rest ->
            let
                (Types.VectorBoundingBox4d b) =
                    next
            in
            aggregateHelp
                (min b.minX currentMinX)
                (max b.maxX currentMaxX)
                (min b.minY currentMinY)
                (max b.maxY currentMaxY)
                (min b.minZ currentMinZ)
                (max b.maxZ currentMaxZ)
                (min b.minW currentMinW)
                (max b.maxW currentMaxW)
                rest

        [] ->
            Types.VectorBoundingBox4d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                , minW = currentMinW
                , maxW = currentMaxW
                }


{-| Like [`aggregate`](#aggregate), but lets you work on any kind of item as
long as a bounding box can be extracted from it.
-}
aggregateOf : (a -> VectorBoundingBox4d units coordinates) -> a -> List a -> VectorBoundingBox4d units coordinates
aggregateOf getBoundingBox first rest =
    let
        (Types.VectorBoundingBox4d b1) =
            getBoundingBox first
    in
    aggregateOfHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ b1.minW b1.maxW getBoundingBox rest


aggregateOfHelp : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> (a -> VectorBoundingBox4d units coordiantes) -> List a -> VectorBoundingBox4d units coordinates
aggregateOfHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ currentMinW currentMaxW getBoundingBox items =
    case items of
        next :: rest ->
            let
                (Types.VectorBoundingBox4d b) =
                    getBoundingBox next
            in
            aggregateOfHelp
                (min b.minX currentMinX)
                (max b.maxX currentMaxX)
                (min b.minY currentMinY)
                (max b.maxY currentMaxY)
                (min b.minZ currentMinZ)
                (max b.maxZ currentMaxZ)
                (min b.minZ currentMinW)
                (max b.maxZ currentMaxW)
                getBoundingBox
                rest

        [] ->
            Types.VectorBoundingBox4d
                { minX = currentMinX
                , maxX = currentMaxX
                , minY = currentMinY
                , maxY = currentMaxY
                , minZ = currentMinZ
                , maxZ = currentMaxZ
                , minW = currentMinW
                , maxW = currentMaxW
                }


{-| Build a bounding box that contains both given bounding boxes. (Note that
this is not strictly speaking a 'union' in the precise mathematical sense.)
-}
union : VectorBoundingBox4d units coordinates -> VectorBoundingBox4d units coordinates -> VectorBoundingBox4d units coordinates
union firstBox secondBox =
    let
        (Types.VectorBoundingBox4d b1) =
            firstBox

        (Types.VectorBoundingBox4d b2) =
            secondBox
    in
    Types.VectorBoundingBox4d
        { minX = min b1.minX b2.minX
        , maxX = max b1.maxX b2.maxX
        , minY = min b1.minY b2.minY
        , maxY = max b1.maxY b2.maxY
        , minZ = min b1.minZ b2.minZ
        , maxZ = max b1.maxZ b2.maxZ
        , minW = min b1.minW b2.minW
        , maxW = max b1.maxW b2.maxW
        }


{-| Build a bounding box that contains all three of the given bounding boxes;

    VectorBoundingBox4d.aggregate3 b1 b2 b3

is equivalent to

    VectorBoundingBox4d.aggregate b1 [ b2, b3 ]

but is more efficient.

-}
aggregate3 : VectorBoundingBox4d units coordinates -> VectorBoundingBox4d units coordinates -> VectorBoundingBox4d units coordinates -> VectorBoundingBox4d units coordinates
aggregate3 firstBox secondBox thirdBox =
    let
        (Types.VectorBoundingBox4d b1) =
            firstBox

        (Types.VectorBoundingBox4d b2) =
            secondBox

        (Types.VectorBoundingBox4d b3) =
            thirdBox
    in
    Types.VectorBoundingBox4d
        { minX = min b1.minX (min b2.minX b3.minX)
        , maxX = max b1.maxX (max b2.maxX b3.maxX)
        , minY = min b1.minY (min b2.minY b3.minY)
        , maxY = max b1.maxY (max b2.maxY b3.maxY)
        , minZ = min b1.minZ (min b2.minZ b3.minZ)
        , maxZ = max b1.maxZ (max b2.maxZ b3.maxZ)
        , minW = min b1.minW (min b2.minW b3.minW)
        , maxW = max b1.maxW (max b2.maxW b3.maxW)
        }


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`. If you know you have at least one bounding
box, you can use [`aggregate`](#aggregate) instead.
-}
aggregateN : List (VectorBoundingBox4d units coordinates) -> Maybe (VectorBoundingBox4d units coordinates)
aggregateN boxes =
    case boxes of
        first :: rest ->
            Just (aggregate first rest)

        [] ->
            Nothing


{-| Combination of [`aggregateOf`](#aggregateOf) and [`aggregateN`](#aggregateN).
-}
aggregateOfN : (a -> VectorBoundingBox4d units coordinates) -> List a -> Maybe (VectorBoundingBox4d units coordinates)
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
at : Quantity Float (Rate units2 units1) -> VectorBoundingBox4d units1 coordinates -> VectorBoundingBox4d units2 coordinates
at rate boundingBox =
    let
        (Quantity r) =
            rate

        (Types.VectorBoundingBox4d b) =
            boundingBox
    in
    if r >= 0 then
        Types.VectorBoundingBox4d
            { minX = r * b.minX
            , maxX = r * b.maxX
            , minY = r * b.minY
            , maxY = r * b.maxY
            , minZ = r * b.minZ
            , maxZ = r * b.maxZ
            , minW = r * b.minW
            , maxW = r * b.maxW
            }

    else
        Types.VectorBoundingBox4d
            { minX = r * b.maxX
            , maxX = r * b.minX
            , minY = r * b.maxY
            , maxY = r * b.minY
            , minZ = r * b.maxZ
            , maxZ = r * b.minZ
            , minW = r * b.maxW
            , maxW = r * b.minW
            }


{-| Convert a bounding box from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> VectorBoundingBox4d units1 coordinates -> VectorBoundingBox4d units2 coordinates
at_ rate boundingBox =
    at (Quantity.inverse rate) boundingBox


{-| Get the range of X values contained by a bounding box.
-}
xInterval : VectorBoundingBox4d units coordinates -> Interval Float units
xInterval boundingBox =
    let
        (Types.VectorBoundingBox4d b) =
            boundingBox
    in
    Interval.from (Quantity b.minX) (Quantity b.maxX)


{-| Get the range of Y values contained by a bounding box.
-}
yInterval : VectorBoundingBox4d units coordinates -> Interval Float units
yInterval boundingBox =
    let
        (Types.VectorBoundingBox4d b) =
            boundingBox
    in
    Interval.from (Quantity b.minY) (Quantity b.maxY)


{-| Get the range of Z values contained by a bounding box.
-}
zInterval : VectorBoundingBox4d units coordinates -> Interval Float units
zInterval boundingBox =
    let
        (Types.VectorBoundingBox4d b) =
            boundingBox
    in
    Interval.from (Quantity b.minZ) (Quantity b.maxZ)


{-| Get the range of W values contained by a bounding box.
-}
wInterval : VectorBoundingBox4d units coordinates -> Interval Float units
wInterval boundingBox =
    let
        (Types.VectorBoundingBox4d b) =
            boundingBox
    in
    Interval.from (Quantity b.minW) (Quantity b.maxW)


{-| Check if a bounding box contains a particular point.
-}
contains : Vector4d units coordinates -> VectorBoundingBox4d units coordinates -> Bool
contains vector boundingBox =
    let
        (Types.Vector4d { x, y, z, w }) =
            vector

        (Types.VectorBoundingBox4d b) =
            boundingBox
    in
    ((x >= b.minX) && (x <= b.maxX))
        && ((y >= b.minY) && (y <= b.maxY))
        && ((z >= b.minZ) && (z <= b.maxZ))
        && ((w >= b.minW) && (w <= b.maxW))


{-| Test if two boxes touch or overlap at all (have any points in common);

    VectorBoundingBox4d.intersects firstBox secondBox

is equivalent to

    VectorBoundingBox4d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

-}
intersects : VectorBoundingBox4d units coordinates -> VectorBoundingBox4d units coordinates -> Bool
intersects other boundingBox =
    let
        (Types.VectorBoundingBox4d b2) =
            other

        (Types.VectorBoundingBox4d b1) =
            boundingBox
    in
    ((b1.minX <= b2.maxX) && (b1.maxX >= b2.minX))
        && ((b1.minY <= b2.maxY) && (b1.maxY >= b2.minY))
        && ((b1.minZ <= b2.maxZ) && (b1.maxZ >= b2.minZ))
        && ((b1.minW <= b2.maxW) && (b1.maxW >= b2.minW))


overlappingByAtLeast : Quantity Float units -> VectorBoundingBox4d units coordinates -> VectorBoundingBox4d units coordinates -> Bool
overlappingByAtLeast tolerance firstBox secondBox =
    let
        (Types.VectorBoundingBox4d b1) =
            firstBox

        (Types.VectorBoundingBox4d b2) =
            secondBox

        xOverlap =
            min b1.maxX b2.maxX - max b1.minX b2.minX

        yOverlap =
            min b1.maxY b2.maxY - max b1.minY b2.minY

        zOverlap =
            min b1.maxZ b2.maxZ - max b1.minZ b2.minZ

        wOverlap =
            min b1.maxW b2.maxW - max b1.minW b2.minW

        (Quantity dGiven) =
            tolerance

        d =
            max dGiven 0
    in
    (xOverlap >= d) && (yOverlap >= d) && (zOverlap >= d) && (wOverlap >= d)


separatedByAtLeast : Quantity Float units -> VectorBoundingBox4d units coordinates -> VectorBoundingBox4d units coordinates -> Bool
separatedByAtLeast tolerance firstBox secondBox =
    let
        (Types.VectorBoundingBox4d b1) =
            firstBox

        (Types.VectorBoundingBox4d b2) =
            secondBox

        xSeparation =
            max b1.minX b2.minX - min b1.maxX b2.maxX

        ySeparation =
            max b1.minY b2.minY - min b1.maxY b2.maxY

        zSeparation =
            max b1.minZ b2.minZ - min b1.maxZ b2.maxZ

        wSeparation =
            max b1.minW b2.minW - min b1.maxW b2.maxW

        (Quantity dGiven) =
            tolerance

        d =
            max dGiven 0
    in
    if (xSeparation >= 0) || (ySeparation >= 0) || (zSeparation >= 0) && (wSeparation >= 0) then
        let
            dX =
                max xSeparation 0

            dY =
                max ySeparation 0

            dZ =
                max zSeparation 0

            dW =
                max wSeparation 0
        in
        dX * dX + dY * dY + dZ * dZ + dW * dW >= d * d

    else
        False


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).
-}
isContainedIn :
    VectorBoundingBox4d units coordinates
    -> VectorBoundingBox4d units coordinates
    -> Bool
isContainedIn other boundingBox =
    let
        (Types.VectorBoundingBox4d b2) =
            other

        (Types.VectorBoundingBox4d b1) =
            boundingBox
    in
    ((b2.minX <= b1.minX) && (b1.maxX <= b2.maxX))
        && ((b2.minY <= b1.minY) && (b1.maxY <= b2.maxY))
        && ((b2.minZ <= b1.minZ) && (b1.maxZ <= b2.maxZ))
        && ((b2.minW <= b1.minW) && (b1.maxW <= b2.maxW))


{-| Attempt to build a bounding box that contains all vectors common to both
given bounding boxes. If the given boxes do not intersect, returns `Nothing`.

If two boxes just touch along an edge or at a corner, they are still considered
to have an intersection, even though that intersection will have zero area (at
least one of its dimensions will be zero).

-}
intersection : VectorBoundingBox4d units coordinates -> VectorBoundingBox4d units coordinates -> Maybe (VectorBoundingBox4d units coordinates)
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        let
            (Types.VectorBoundingBox4d b1) =
                firstBox

            (Types.VectorBoundingBox4d b2) =
                secondBox
        in
        Just
            (Types.VectorBoundingBox4d
                { minX = max b1.minX b2.minX
                , maxX = min b1.maxX b2.maxX
                , minY = max b1.minY b2.minY
                , maxY = min b1.maxY b2.maxY
                , minZ = max b1.minZ b2.minZ
                , maxZ = min b1.maxZ b2.maxZ
                , minW = max b1.minW b2.minW
                , maxW = min b1.maxW b2.maxW
                }
            )

    else
        Nothing


{-| Interpolate within a bounding box based on parameter values which range from
0 to 1.
-}
interpolate : VectorBoundingBox4d units coordinates -> Float -> Float -> Float -> Float -> Vector4d units coordinates
interpolate boundingBox t u v w =
    let
        (Types.VectorBoundingBox4d b) =
            boundingBox
    in
    Types.Vector4d
        { x = Float.interpolateFrom b.minX b.maxX t
        , y = Float.interpolateFrom b.minY b.maxY u
        , z = Float.interpolateFrom b.minZ b.maxZ v
        , w = Float.interpolateFrom b.minW b.maxW w
        }
