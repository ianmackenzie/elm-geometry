--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module OpenSolid.BoundingBox3d
    exposing
        ( BoundingBox3d
        , centroid
        , contains
        , dimensions
        , extrema
        , hull
        , hullOf
        , intersection
        , intersects
        , isContainedIn
        , maxX
        , maxY
        , maxZ
        , midX
        , midY
        , midZ
        , minX
        , minY
        , minZ
        , overlappingBy
        , overlaps
        , scaleAbout
        , separatedBy
        , singleton
        , translateBy
        , with
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/boundingBox3d.svg" alt="BoundingBox3d" width="160">

A `BoundingBox3d` is a rectangular box in 3D defined by its minimum and maximum
X, Y and Z values. It is possible to generate bounding boxes for most geometric
objects; for example, [`Triangle3d.boundingBox`](OpenSolid-Triangle3d#boundingBox)
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

@docs with, singleton, hull, intersection, hullOf


# Properties

@docs extrema, minX, maxX, minY, maxY, minZ, maxZ, dimensions, midX, midY, midZ, centroid


# Queries

@docs contains, isContainedIn, intersects, overlaps, overlappingBy, separatedBy


# Transformations

@docs scaleAbout, translateBy

-}

import OpenSolid.Bootstrap.BoundingBox3d as Bootstrap
import OpenSolid.Bootstrap.Point3d as Point3d
import OpenSolid.Geometry.Internal as Internal exposing (Point3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{-| -}
type alias BoundingBox3d =
    Internal.BoundingBox3d


{-| Construct a bounding box from its minimum and maximum X, Y and Z values:

    exampleBox =
        BoundingBox3d.with
            { minX = -2
            , maxX = 2
            , minY = 2
            , maxY = 5
            , minZ = 3
            , maxZ = 4
            }

If the minimum and maximum values are provided in the wrong order (for example
if <code>minX&nbsp;>&nbsp;maxX</code>), then they will be swapped so that the
resulting bounding box is valid.

-}
with : { minX : Float, maxX : Float, minY : Float, maxY : Float, minZ : Float, maxZ : Float } -> BoundingBox3d
with =
    Bootstrap.with


{-| Construct a zero-width bounding box containing a single point.

    point =
        Point3d.fromCoordinates ( 2, 1, 3 )

    BoundingBox3d.singleton point
    --> BoundingBox3d.with
    -->     { minX = 2
    -->     , maxX = 2
    -->     , minY = 1
    -->     , maxY = 1
    -->     , minZ = 3
    -->     , maxZ = 3
    -->     }

-}
singleton : Point3d -> BoundingBox3d
singleton point =
    let
        ( x, y, z ) =
            Point3d.coordinates point
    in
    with
        { minX = x
        , maxX = x
        , minY = y
        , maxY = y
        , minZ = z
        , maxZ = z
        }


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`.

    singletonBox =
        BoundingBox3d.singleton
            (Point3d.fromCoordinates ( 2, 1, 0 ))

    BoundingBox3d.hullOf [ exampleBox, singletonBox ]
    --> Just
    -->     (BoundingBox3d.with
    -->         { minX = -2,
    -->         , maxX = 2
    -->         , minY = 1
    -->         , maxY = 5
    -->         , minZ = 0
    -->         , maxZ = 4
    -->         }
    -->     )

    BoundingBox3d.hullOf [ exampleBox ]
    --> Just exampleBox

    BoundingBox3d.hullOf []
    --> Nothing

If you have exactly two bounding boxes, you can use [`BoundingBox3d.hull`](#hull)
instead (which returns a `BoundingBox3d` instead of a `Maybe BoundingBox3d`).

-}
hullOf : List BoundingBox3d -> Maybe BoundingBox3d
hullOf boundingBoxes =
    case boundingBoxes of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Get the minimum and maximum X, Y and Z values of a bounding box in a single
record.

    BoundingBox3d.extrema exampleBox
    --> { minX = -2
    --> , maxX = 2
    --> , minY = 2
    --> , maxY = 5
    --> , minZ = 3
    --> , maxZ = 4
    --> }

Can be useful when combined with record destructuring, for example

    { minX, maxX, minY, maxY, minZ, maxZ } =
        BoundingBox3d.extrema exampleBox


    --> minX = -2
    --> maxX = 2
    --> minY = 2
    --> maxY = 5
    --> minZ = 3
    --> maxZ = 4

-}
extrema : BoundingBox3d -> { minX : Float, maxX : Float, minY : Float, maxY : Float, minZ : Float, maxZ : Float }
extrema (Internal.BoundingBox3d properties) =
    properties


{-| Get the minimum X value of a bounding box.

    BoundingBox3d.minX exampleBox
    --> -2

-}
minX : BoundingBox3d -> Float
minX boundingBox =
    (extrema boundingBox).minX


{-| Get the maximum X value of a bounding box.

    BoundingBox3d.maxX exampleBox
    --> 2

-}
maxX : BoundingBox3d -> Float
maxX boundingBox =
    (extrema boundingBox).maxX


{-| Get the minimum Y value of a bounding box.

    BoundingBox3d.minY exampleBox
    --> 2

-}
minY : BoundingBox3d -> Float
minY boundingBox =
    (extrema boundingBox).minY


{-| Get the maximum Y value of a bounding box.

    BoundingBox3d.maxY exampleBox
    --> 5

-}
maxY : BoundingBox3d -> Float
maxY boundingBox =
    (extrema boundingBox).maxY


{-| Get the minimum Z value of a bounding box.

    BoundingBox3d.minZ exampleBox
    --> 3

-}
minZ : BoundingBox3d -> Float
minZ boundingBox =
    (extrema boundingBox).minZ


{-| Get the maximum Z value of a bounding box.

    BoundingBox3d.maxZ exampleBox
    --> 4

-}
maxZ : BoundingBox3d -> Float
maxZ boundingBox =
    (extrema boundingBox).maxZ


{-| Get the X, Y and Z dimensions (widths) of a bounding box.

    BoundingBox3d.dimensions exampleBox
    --> ( 4, 3, 1 )

-}
dimensions : BoundingBox3d -> ( Float, Float, Float )
dimensions boundingBox =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            extrema boundingBox
    in
    ( maxX - minX, maxY - minY, maxZ - minZ )


{-| Get the median X value of a bounding box.

    BoundingBox3d.midX exampleBox
    --> 0

-}
midX : BoundingBox3d -> Float
midX boundingBox =
    let
        { minX, maxX } =
            extrema boundingBox
    in
    minX + 0.5 * (maxX - minX)


{-| Get the median Y value of a bounding box.

    BoundingBox3d.midY exampleBox
    --> 3.5

-}
midY : BoundingBox3d -> Float
midY boundingBox =
    let
        { minY, maxY } =
            extrema boundingBox
    in
    minY + 0.5 * (maxY - minY)


{-| Get the median Z value of a bounding box.

    BoundingBox3d.midZ exampleBox
    --> 3.5

-}
midZ : BoundingBox3d -> Float
midZ boundingBox =
    let
        { minZ, maxZ } =
            extrema boundingBox
    in
    minZ + 0.5 * (maxZ - minZ)


{-| Get the point at the center of a bounding box.

    BoundingBox3d.centroid exampleBox
    --> Point3d.fromCoordinates ( 0, 3.5, 3.5 )

-}
centroid : BoundingBox3d -> Point3d
centroid boundingBox =
    Point3d.fromCoordinates
        ( midX boundingBox
        , midY boundingBox
        , midZ boundingBox
        )


{-| Check if a bounding box contains a particular point.

    firstPoint =
        Point3d.fromCoordinates ( 1, 4, 3 )

    secondPoint =
        Point3d.fromCoordinates ( 3, 4, 5 )

    BoundingBox3d.contains firstPoint exampleBox
    --> True

    BoundingBox3d.contains secondPoint exampleBox
    --> False

-}
contains : Point3d -> BoundingBox3d -> Bool
contains point boundingBox =
    let
        ( x, y, z ) =
            Point3d.coordinates point

        { minX, maxX, minY, maxY, minZ, maxZ } =
            extrema boundingBox
    in
    (minX <= x && x <= maxX)
        && (minY <= y && y <= maxY)
        && (minZ <= z && z <= maxZ)


{-| Test if two boxes touch or overlap at all (have any points in common);

    BoundingBox3d.intersects firstBox secondBox

is equivalent to

    BoundingBox3d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

    firstBox =
        BoundingBox3d.with
            { minX = 0
            , maxX = 3
            , minY = 0
            , maxY = 2
            , minZ = 0
            , maxZ = 1
            }

    secondBox =
        BoundingBox3d.with
            { minX = 0
            , maxX = 3
            , minY = 1
            , maxY = 4
            , minZ = -1
            , maxZ = 2
            }

    thirdBox =
        BoundingBox3d.with
            { minX = 0
            , maxX = 3
            , minY = 4
            , maxY = 5
            , minZ = -1
            , maxZ = 2
            }

    BoundingBox3d.intersects firstBox secondBox
    --> True

    BoundingBox3d.intersects firstBox thirdBox
    --> False

-}
intersects : BoundingBox3d -> BoundingBox3d -> Bool
intersects other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)
        && (minZ boundingBox <= maxZ other)
        && (maxZ boundingBox >= minZ other)


{-| DEPRECATED: Alias for `intersects`, kept for compatibility. Use `intersects`
instead.
-}
overlaps : BoundingBox3d -> BoundingBox3d -> Bool
overlaps =
    intersects


overlapAmount : BoundingBox3d -> BoundingBox3d -> Maybe Float
overlapAmount firstBox secondBox =
    let
        xOverlap =
            min (maxX firstBox) (maxX secondBox)
                - max (minX firstBox) (minX secondBox)

        yOverlap =
            min (maxY firstBox) (maxY secondBox)
                - max (minY firstBox) (minY secondBox)

        zOverlap =
            min (maxZ firstBox) (maxZ secondBox)
                - max (minZ firstBox) (minZ secondBox)
    in
    if xOverlap >= 0 && yOverlap >= 0 && zOverlap >= 0 then
        Just (min xOverlap (min yOverlap zOverlap))
    else
        Nothing


squaredSeparationAmount : BoundingBox3d -> BoundingBox3d -> Maybe Float
squaredSeparationAmount firstBox secondBox =
    let
        xSeparation =
            max (minX firstBox) (minX secondBox)
                - min (maxX firstBox) (maxX secondBox)

        ySeparation =
            max (minY firstBox) (minY secondBox)
                - min (maxY firstBox) (maxY secondBox)

        zSeparation =
            max (minZ firstBox) (minZ secondBox)
                - min (maxZ firstBox) (maxZ secondBox)
    in
    if xSeparation >= 0 || ySeparation >= 0 || zSeparation >= 0 then
        let
            dX =
                max xSeparation 0

            dY =
                max ySeparation 0

            dZ =
                max zSeparation 0
        in
        Just (dX * dX + dY * dY + dZ * dZ)
    else
        Nothing


alwaysFalse : BoundingBox3d -> BoundingBox3d -> Bool
alwaysFalse firstBox secondBox =
    False


{-| Check if one box overlaps another by less than, greater than or equal to a
given amount. For example, you could perform a tolerant collision check (one
that only returns true if the boxes overlap by at least some small finite
amount, and ignores boxes that just barely touch each other) as

    boxesCollide =
        BoundingBox3d.overlappingBy GT 0.001 box1 box2

This can be read as "`box1` and `box2` are overlapping by greater than 0.001
units". (The [`Order`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#Order)
type and its three values `LT`, `GT` and `EQ` are defined in Elm's `Basics`
module so are available by default in any Elm program.)

Overlap is defined as the _minimum_ distance one box would have to move so that
it did not touch the other, and is always positive for any two overlapping
boxes.

Boxes that just touch are considered to have an overlap of zero, which is
distinct from 'no overlap'. Boxes that do not touch or overlap at all are
considered to have an overlap which is less than zero but not comparable to any
negative number.


### Less than

  - `overlappingBy LT 1e-3` will return true if the two boxes overlap by less
    than 0.001 units or if they do not overlap at all (false if they overlap by
    more than 0.001 units).
  - `overlappingBy LT 0` will return true only if the two boxes don't touch or
    overlap at all.
  - `overlappingBy LT -1e-3` will always return false! If you care about _how
    much_ two boxes are separated by, use `separatedBy` instead.


### Greater than

  - `overlappingBy GT 1e-3` will return true if the two boxes overlap by at
    least 0.001 units (false if they overlap by less than that or do not overlap
    at all).
  - `overlappingBy GT 0` will return true if the two boxes overlap by any
    non-zero amount (false if they just touch or do not overlap at all).
  - `overlappingBy GT -1e-3` doesn't make a lot of sense but will return true if
    the boxes touch or overlap at all (false if they don't overlap, regardless
    of how close they are to overlapping). In this case, though, it would make
    more sense to just user `intersects` instead.


### Equal to

Checking whether two boxes overlap by exactly a given amount is pretty weird and
vulnerable to floating-point roundoff, but is defined as follows:

  - `overlappingBy EQ 1e-3` will return true if the two boxes overlap by exactly
    0.001 units.
  - `overlappingBy EQ 0` will return true if and only if the boxes just touch
    each other.
  - `overlappingBy EQ -1e-3` will always return false.

-}
overlappingBy : Order -> Float -> BoundingBox3d -> BoundingBox3d -> Bool
overlappingBy order tolerance =
    case order of
        LT ->
            if tolerance > 0 then
                \firstBox secondBox ->
                    case overlapAmount firstBox secondBox of
                        Just distance ->
                            distance < tolerance

                        Nothing ->
                            True
            else if tolerance == 0 then
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox == Nothing
            else
                alwaysFalse

        GT ->
            if tolerance >= 0 then
                \firstBox secondBox ->
                    case overlapAmount firstBox secondBox of
                        Just distance ->
                            distance > tolerance

                        Nothing ->
                            False
            else
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox /= Nothing

        EQ ->
            if tolerance >= 0 then
                let
                    expected =
                        Just tolerance
                in
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox == expected
            else
                alwaysFalse


{-| Check if one box is separated from another by less than, greater than or
equal to a given amount. For example, to perform clash detection between some
objects, you could use `separatedBy` on those objects' bounding boxes as a quick
check to see if the objects had a gap of at least 1 cm between them:

    safelySeparated =
        BoundingBox3d.separatedBy GT 0.01 box1 box2

This can be read as "`box1` and `box2` are separated by greater than 0.01
units". (The [`Order`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#Order)
type and its three values `LT`, `GT` and `EQ` are defined in Elm's `Basics`
module so are available by default in any Elm program.)

Separation is defined as the _minimum_ distance one box would have to move
so that it touched the other, and is always positive for any two boxes that do
not touch.

Boxes that just touch are considered to have a separation of zero, which is
distinct from 'no separation'. 'No separation' (overlap) is considered to be
less than zero but not comparable to any negative number.


### Less than

  - `separatedBy LT 1e-3` will return true if the two boxes are separated by
    less than 0.001 units or if they touch or overlap (false if they are
    separated by at least 0.001 units).
  - `separatedBy LT 0` will return true only if the boxes overlap by some
    non-zero amount.
  - `separatedBy LT -1e-3` will always return false! If you care about _how
    much_ two boxes overlap by, use `overlappingBy` instead.


### Greater than

  - `separatedBy GT 1e-3` will return true if the two boxes are separated by at
    least 0.001 units (false if they are separated by less than that or if they
    touch or overlap).
  - `separatedBy GT 0` will return true if the two boxes are separated by any
    non-zero amount (false if they touch or overlap).
  - `separatedBy GT -1e-3` doesn't make a lot of sense but will return true if
    the boxes just touch or are separated by any amount (false if they overlap
    by any non-zero amount).


### Equal to

Checking whether two boxes are separated by exactly a given amount is pretty
weird and vulnerable to floating-point roundoff, but is defined as follows:

  - `separatedBy EQ 1e-3` will return true if the two boxes are separated by
    exactly 0.001 units.
  - `separatedBy EQ 0` will return true if and only if the boxes just touch each
    other.
  - `separatedBy EQ -3` will always return false.

-}
separatedBy : Order -> Float -> BoundingBox3d -> BoundingBox3d -> Bool
separatedBy order tolerance =
    case order of
        LT ->
            if tolerance > 0 then
                \firstBox secondBox ->
                    case squaredSeparationAmount firstBox secondBox of
                        Just squaredDistance ->
                            squaredDistance < tolerance * tolerance

                        Nothing ->
                            True
            else if tolerance == 0 then
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox == Nothing
            else
                alwaysFalse

        GT ->
            if tolerance >= 0 then
                \firstBox secondBox ->
                    case squaredSeparationAmount firstBox secondBox of
                        Just squaredDistance ->
                            squaredDistance > tolerance * tolerance

                        Nothing ->
                            False
            else
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox /= Nothing

        EQ ->
            if tolerance >= 0 then
                let
                    expected =
                        Just (tolerance * tolerance)
                in
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox == expected
            else
                alwaysFalse


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).

    outerBox =
        BoundingBox3d.with
            { minX = 0
            , maxX = 10
            , minY = 0
            , maxY = 10
            , minZ = 0
            , maxZ = 10
            }

    innerBox =
        BoundingBox3d.with
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 9
            , minZ = 7
            , maxZ = 8
            }

    overlappingBox =
        BoundingBox3d.with
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 12
            , minZ = 7
            , maxZ = 8
            }

    BoundingBox3d.isContainedIn outerBox innerBox
    --> True

    BoundingBox3d.isContainedIn outerBox overlappingBox
    --> False

-}
isContainedIn : BoundingBox3d -> BoundingBox3d -> Bool
isContainedIn other boundingBox =
    (minX other <= minX boundingBox && maxX boundingBox <= maxX other)
        && (minY other <= minY boundingBox && maxY boundingBox <= maxY other)
        && (minZ other <= minZ boundingBox && maxZ boundingBox <= maxZ other)


{-| Build a bounding box that contains both given bounding boxes.

    firstBox =
        BoundingBox3d.with
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            , minZ = 0
            , maxZ = 5
            }

    secondBox =
        BoundingBox3d.with
            { minX = -2
            , maxX = 2
            , minY = 4
            , maxY = 5
            , minZ = -1
            , maxZ = 0
            }

    BoundingBox3d.hull firstBox secondBox
    --> BoundingBox3d.with
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = 2
    -->     , maxY = 5
    -->     , minZ = -1
    -->     , maxZ = 5
    -->     }

-}
hull : BoundingBox3d -> BoundingBox3d -> BoundingBox3d
hull firstBox secondBox =
    with
        { minX = min (minX firstBox) (minX secondBox)
        , maxX = max (maxX firstBox) (maxX secondBox)
        , minY = min (minY firstBox) (minY secondBox)
        , maxY = max (maxY firstBox) (maxY secondBox)
        , minZ = min (minZ firstBox) (minZ secondBox)
        , maxZ = max (maxZ firstBox) (maxZ secondBox)
        }


{-| Attempt to build a bounding box that contains all points common to both
given bounding boxes. If the given boxes do not overlap, returns `Nothing`.

    firstBox =
        BoundingBox3d.with
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            , minZ = 5
            , maxZ = 8
            }

    secondBox =
        BoundingBox3d.with
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 4
            , minZ = 6
            , maxZ = 7
            }

    thirdBox =
        BoundingBox3d.with
            { minX = 1
            , maxX = 4
            , minY = 4
            , maxY = 5
            , minZ = 5
            , maxZ = 8
            }

    BoundingBox3d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox3d.with
    -->         { minX = 2
    -->         , maxX = 4
    -->         , minY = 2
    -->         , maxY = 3
    -->         , minZ = 6
    -->         , maxZ = 7
    -->         }
    -->     )

    BoundingBox3d.intersection firstBox thirdBox
    --> Nothing

-}
intersection : BoundingBox3d -> BoundingBox3d -> Maybe BoundingBox3d
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        Just
            (with
                { minX = max (minX firstBox) (minX secondBox)
                , maxX = min (maxX firstBox) (maxX secondBox)
                , minY = max (minY firstBox) (minY secondBox)
                , maxY = min (maxY firstBox) (maxY secondBox)
                , minZ = max (minZ firstBox) (minZ secondBox)
                , maxZ = min (maxZ firstBox) (maxZ secondBox)
                }
            )
    else
        Nothing


{-| Scale a bounding box about a given point by a given scale.

    point =
        Point3d.fromCoordinates ( 2, 2, 2 )

    BoundingBox3d.scaleAbout point 2 exampleBox
    --> BoundingBox3d.with
    -->     { minX = -6
    -->     , maxX = 2
    -->     , minY = 2
    -->     , maxY = 8
    -->     , minZ = 4
    -->     , maxZ = 6
    -->     }

-}
scaleAbout : Point3d -> Float -> BoundingBox3d -> BoundingBox3d
scaleAbout point scale boundingBox =
    let
        { minX, minY, minZ, maxX, maxY, maxZ } =
            extrema boundingBox

        ( x0, y0, z0 ) =
            Point3d.coordinates point
    in
    if scale >= 0 then
        with
            { minX = x0 + scale * (minX - x0)
            , maxX = x0 + scale * (maxX - x0)
            , minY = y0 + scale * (minY - y0)
            , maxY = y0 + scale * (maxY - y0)
            , minZ = z0 + scale * (minZ - z0)
            , maxZ = z0 + scale * (maxZ - z0)
            }
    else
        with
            { minX = x0 + scale * (maxX - x0)
            , maxX = x0 + scale * (minX - x0)
            , minY = y0 + scale * (maxY - y0)
            , maxY = y0 + scale * (minY - y0)
            , minZ = z0 + scale * (maxZ - z0)
            , maxZ = z0 + scale * (minZ - z0)
            }


{-| Translate a bounding box by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, -3, 1 )

    BoundingBox3d.translateBy displacement exampleBox
    --> BoundingBox3d.with
    -->     { minX = 0
    -->     , maxX = 4
    -->     , minY = -1
    -->     , maxY = 2
    -->     , minZ = 4
    -->     , maxZ = 5
    -->     }

-}
translateBy : Vector3d -> BoundingBox3d -> BoundingBox3d
translateBy displacement boundingBox =
    let
        { minX, minY, minZ, maxX, maxY, maxZ } =
            extrema boundingBox

        ( dx, dy, dz ) =
            Vector3d.components displacement
    in
    with
        { minX = minX + dx
        , maxX = maxX + dx
        , minY = minY + dy
        , maxY = maxY + dy
        , minZ = minZ + dz
        , maxZ = maxZ + dz
        }
