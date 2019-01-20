--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module BoundingBox3d exposing
    ( BoundingBox3d
    , fromExtrema, singleton, from, hull, intersection, aggregate, containingPoints
    , extrema, minX, maxX, minY, maxY, minZ, maxZ, dimensions, midX, midY, midZ, centerPoint
    , contains, isContainedIn, intersects, overlappingBy, separatedBy
    , scaleAbout, translateBy, translateIn, offsetBy, expandBy
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

@docs fromExtrema, singleton, from, hull, intersection, aggregate, containingPoints


# Properties

@docs extrema, minX, maxX, minY, maxY, minZ, maxZ, dimensions, midX, midY, midZ, centerPoint, centroid


# Queries

@docs contains, isContainedIn, intersects, overlappingBy, separatedBy


# Transformations

@docs scaleAbout, translateBy, translateIn, offsetBy, expandBy

-}

import Direction3d exposing (Direction3d)
import Geometry.Types as Types
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Squared)
import Quantity.Extra as Quantity
import Vector3d exposing (Vector3d)


{-| -}
type alias BoundingBox3d units coordinates =
    Types.BoundingBox3d units coordinates


{-| Construct a bounding box from its minimum and maximum X, Y and Z values:

    exampleBox =
        BoundingBox3d.fromExtrema
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
fromExtrema :
    { minX : Quantity Float units
    , maxX : Quantity Float units
    , minY : Quantity Float units
    , maxY : Quantity Float units
    , minZ : Quantity Float units
    , maxZ : Quantity Float units
    }
    -> BoundingBox3d units coordinates
fromExtrema givenExtrema =
    if
        (givenExtrema.minX |> Quantity.lessThanOrEqualTo givenExtrema.maxX)
            && (givenExtrema.minY |> Quantity.lessThanOrEqualTo givenExtrema.maxY)
            && (givenExtrema.minZ |> Quantity.lessThanOrEqualTo givenExtrema.maxZ)
    then
        Types.BoundingBox3d givenExtrema

    else
        Types.BoundingBox3d
            { minX = Quantity.min givenExtrema.minX givenExtrema.maxX
            , maxX = Quantity.max givenExtrema.minX givenExtrema.maxX
            , minY = Quantity.min givenExtrema.minY givenExtrema.maxY
            , maxY = Quantity.max givenExtrema.minY givenExtrema.maxY
            , minZ = Quantity.min givenExtrema.minZ givenExtrema.maxZ
            , maxZ = Quantity.max givenExtrema.minZ givenExtrema.maxZ
            }


{-| Construct a zero-width bounding box containing a single point.

    point =
        Point3d.fromCoordinates ( 2, 1, 3 )

    BoundingBox3d.singleton point
    --> BoundingBox3d.fromExtrema
    -->     { minX = 2
    -->     , maxX = 2
    -->     , minY = 1
    -->     , maxY = 1
    -->     , minZ = 3
    -->     , maxZ = 3
    -->     }

-}
singleton : Point3d units coordinates -> BoundingBox3d units coordinates
singleton point =
    let
        ( x, y, z ) =
            Point3d.coordinates point
    in
    fromExtrema
        { minX = x
        , maxX = x
        , minY = y
        , maxY = y
        , minZ = z
        , maxZ = z
        }


{-| Construct a bounding box with the two given points as two of its corners.
The points can be given in any order and don't have to represent the 'primary'
diagonal of the bounding box.

    firstPoint =
        Point3d.fromCoordinates ( 2, 1, 3 )

    secondPoint =
        Point3d.fromCoordinates ( -1, 5, -2 )

    BoundingBox3d.from firstPoint secondPoint
    --> BoundingBox3d.fromExtrema
    -->     { minX = -1
    -->     , maxX = 2
    -->     , minY = 1
    -->     , maxY = 5
    -->     , minZ = -2
    -->     , maxZ = 3
    -->     }

-}
from : Point3d units coordinates -> Point3d units coordinates -> BoundingBox3d units coordinates
from firstPoint secondPoint =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates firstPoint

        ( x2, y2, z2 ) =
            Point3d.coordinates secondPoint
    in
    fromExtrema
        { minX = Quantity.min x1 x2
        , maxX = Quantity.max x1 x2
        , minY = Quantity.min y1 y2
        , maxY = Quantity.max y1 y2
        , minZ = Quantity.min z1 z2
        , maxZ = Quantity.max z1 z2
        }


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`.

    singletonBox =
        BoundingBox3d.singleton
            (Point3d.fromCoordinates ( 2, 1, 0 ))

    BoundingBox3d.aggregate [ exampleBox, singletonBox ]
    --> Just
    -->     (BoundingBox3d.fromExtrema
    -->         { minX = -2,
    -->         , maxX = 2
    -->         , minY = 1
    -->         , maxY = 5
    -->         , minZ = 0
    -->         , maxZ = 4
    -->         }
    -->     )

    BoundingBox3d.aggregate [ exampleBox ]
    --> Just exampleBox

    BoundingBox3d.aggregate []
    --> Nothing

If you have exactly two bounding boxes, you can use [`BoundingBox3d.hull`](#hull)
instead (which returns a `BoundingBox3d` instead of a `Maybe BoundingBox3d`).

-}
aggregate : List (BoundingBox3d units coordinates) -> Maybe (BoundingBox3d units coordinates)
aggregate boundingBoxes =
    case boundingBoxes of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Construct a bounding box containing all points in the given list. If the
list is empty, returns `Nothing`.

    BoundingBox3d.containingPoints
        [ Point3d.fromCoordinates ( 2, 1, 3 )
        , Point3d.fromCoordinates ( -1, 5, -2 )
        , Point3d.fromCoordinates ( 6, 4, 2 )
        ]
    --> Just <|
    -->     BoundingBox3d.fromExtrema
    -->         { minX = -1
    -->         , maxX = 6
    -->         , minY = 1
    -->         , maxY = 5
    -->         , minZ = -2
    -->         , maxZ = 3
    -->         }

    BoundingBox3d.containingPoints []
    --> Nothing

-}
containingPoints : List (Point3d units coordinates) -> Maybe (BoundingBox3d units coordinates)
containingPoints points =
    aggregate (List.map singleton points)


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


{-| Get the minimum X value of a bounding box.

    BoundingBox3d.minX exampleBox
    --> -2

-}
minX : BoundingBox3d units coordinates -> Quantity Float units
minX (Types.BoundingBox3d boundingBox) =
    boundingBox.minX


{-| Get the maximum X value of a bounding box.

    BoundingBox3d.maxX exampleBox
    --> 2

-}
maxX : BoundingBox3d units coordinates -> Quantity Float units
maxX (Types.BoundingBox3d boundingBox) =
    boundingBox.maxX


{-| Get the minimum Y value of a bounding box.

    BoundingBox3d.minY exampleBox
    --> 2

-}
minY : BoundingBox3d units coordinates -> Quantity Float units
minY (Types.BoundingBox3d boundingBox) =
    boundingBox.minY


{-| Get the maximum Y value of a bounding box.

    BoundingBox3d.maxY exampleBox
    --> 5

-}
maxY : BoundingBox3d units coordinates -> Quantity Float units
maxY (Types.BoundingBox3d boundingBox) =
    boundingBox.maxY


{-| Get the minimum Z value of a bounding box.

    BoundingBox3d.minZ exampleBox
    --> 3

-}
minZ : BoundingBox3d units coordinates -> Quantity Float units
minZ (Types.BoundingBox3d boundingBox) =
    boundingBox.minZ


{-| Get the maximum Z value of a bounding box.

    BoundingBox3d.maxZ exampleBox
    --> 4

-}
maxZ : BoundingBox3d units coordinates -> Quantity Float units
maxZ (Types.BoundingBox3d boundingBox) =
    boundingBox.maxZ


{-| Get the X, Y and Z dimensions (widths) of a bounding box.

    BoundingBox3d.dimensions exampleBox
    --> ( 4, 3, 1 )

-}
dimensions : BoundingBox3d units coordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units )
dimensions boundingBox =
    ( maxX boundingBox |> Quantity.minus (minX boundingBox)
    , maxY boundingBox |> Quantity.minus (minY boundingBox)
    , maxZ boundingBox |> Quantity.minus (minZ boundingBox)
    )


{-| Get the median X value of a bounding box.

    BoundingBox3d.midX exampleBox
    --> 0

-}
midX : BoundingBox3d units coordinates -> Quantity Float units
midX (Types.BoundingBox3d boundingBox) =
    Quantity.interpolateFrom boundingBox.minX boundingBox.maxX 0.5


{-| Get the median Y value of a bounding box.

    BoundingBox3d.midY exampleBox
    --> 3.5

-}
midY : BoundingBox3d units coordinates -> Quantity Float units
midY (Types.BoundingBox3d boundingBox) =
    Quantity.interpolateFrom boundingBox.minY boundingBox.maxY 0.5


{-| Get the median Z value of a bounding box.

    BoundingBox3d.midZ exampleBox
    --> 3.5

-}
midZ : BoundingBox3d units coordinates -> Quantity Float units
midZ (Types.BoundingBox3d boundingBox) =
    Quantity.interpolateFrom boundingBox.minZ boundingBox.maxZ 0.5


{-| Get the point at the center of a bounding box.

    BoundingBox3d.centerPoint exampleBox
    --> Point3d.fromCoordinates ( 0, 3.5, 3.5 )

-}
centerPoint : BoundingBox3d units coordinates -> Point3d units coordinates
centerPoint boundingBox =
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
contains : Point3d units coordinates -> BoundingBox3d units coordinates -> Bool
contains point boundingBox =
    let
        ( x, y, z ) =
            Point3d.coordinates point
    in
    (x |> Quantity.greaterThanOrEqualTo (minX boundingBox))
        && (x |> Quantity.lessThanOrEqualTo (maxX boundingBox))
        && (y |> Quantity.greaterThanOrEqualTo (minY boundingBox))
        && (y |> Quantity.lessThanOrEqualTo (maxY boundingBox))
        && (z |> Quantity.greaterThanOrEqualTo (minZ boundingBox))
        && (z |> Quantity.lessThanOrEqualTo (maxZ boundingBox))


{-| Test if two boxes touch or overlap at all (have any points in common);

    BoundingBox3d.intersects firstBox secondBox

is equivalent to

    BoundingBox3d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

    firstBox =
        BoundingBox3d.fromExtrema
            { minX = 0
            , maxX = 3
            , minY = 0
            , maxY = 2
            , minZ = 0
            , maxZ = 1
            }

    secondBox =
        BoundingBox3d.fromExtrema
            { minX = 0
            , maxX = 3
            , minY = 1
            , maxY = 4
            , minZ = -1
            , maxZ = 2
            }

    thirdBox =
        BoundingBox3d.fromExtrema
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
intersects : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
intersects other boundingBox =
    (minX boundingBox |> Quantity.lessThanOrEqualTo (maxX other))
        && (maxX boundingBox |> Quantity.greaterThanOrEqualTo (minX other))
        && (minY boundingBox |> Quantity.lessThanOrEqualTo (maxY other))
        && (maxY boundingBox |> Quantity.greaterThanOrEqualTo (minY other))
        && (minZ boundingBox |> Quantity.lessThanOrEqualTo (maxZ other))
        && (maxZ boundingBox |> Quantity.greaterThanOrEqualTo (minZ other))


overlapAmount : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Maybe (Quantity Float units)
overlapAmount firstBox secondBox =
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
    in
    if
        (xOverlap |> Quantity.greaterThanOrEqualTo Quantity.zero)
            && (yOverlap |> Quantity.greaterThanOrEqualTo Quantity.zero)
            && (zOverlap |> Quantity.greaterThanOrEqualTo Quantity.zero)
    then
        Just (Quantity.min xOverlap (Quantity.min yOverlap zOverlap))

    else
        Nothing


squaredSeparationAmount : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Maybe (Quantity Float (Squared units))
squaredSeparationAmount firstBox secondBox =
    let
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
        Just
            (Quantity.squared dX
                |> Quantity.plus (Quantity.squared dY)
                |> Quantity.plus (Quantity.squared dZ)
            )

    else
        Nothing


alwaysFalse : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
alwaysFalse firstBox secondBox =
    False


{-| Check if one box overlaps another by less than, greater than or equal to a
given amount. For example, you could implement a tolerant collision check (one
that only returns true if the boxes overlap by at least some small finite
amount, and ignores boxes that just barely touch each other) as

    boxesCollide box1 box2 =
        BoundingBox3d.overlappingBy GT 0.001 box1 box2

This can be read as "`box1` and `box2` are overlapping by greater than 0.001
units". (The [`Order`](https://package.elm-lang.org/packages/elm/core/latest/Basics#Order)
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
overlappingBy : Order -> Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
overlappingBy order tolerance =
    case order of
        LT ->
            if tolerance |> Quantity.greaterThan Quantity.zero then
                \firstBox secondBox ->
                    case overlapAmount firstBox secondBox of
                        Just distance ->
                            distance |> Quantity.lessThan tolerance

                        Nothing ->
                            True

            else if tolerance == Quantity.zero then
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox == Nothing

            else
                alwaysFalse

        GT ->
            if tolerance |> Quantity.greaterThanOrEqualTo Quantity.zero then
                \firstBox secondBox ->
                    case overlapAmount firstBox secondBox of
                        Just distance ->
                            distance |> Quantity.greaterThan tolerance

                        Nothing ->
                            False

            else
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox /= Nothing

        EQ ->
            if tolerance |> Quantity.greaterThanOrEqualTo Quantity.zero then
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

    safelySeparated box1 box2 =
        BoundingBox3d.separatedBy GT 0.01 box1 box2

This can be read as "`box1` and `box2` are separated by greater than 0.01
units". (The [`Order`](https://package.elm-lang.org/packages/elm/core/latest/Basics#Order)
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
separatedBy : Order -> Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
separatedBy order tolerance =
    case order of
        LT ->
            if tolerance |> Quantity.greaterThan Quantity.zero then
                \firstBox secondBox ->
                    case squaredSeparationAmount firstBox secondBox of
                        Just squaredDistance ->
                            squaredDistance
                                |> Quantity.lessThan
                                    (Quantity.squared tolerance)

                        Nothing ->
                            True

            else if tolerance == Quantity.zero then
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox == Nothing

            else
                alwaysFalse

        GT ->
            if tolerance |> Quantity.greaterThanOrEqualTo Quantity.zero then
                \firstBox secondBox ->
                    case squaredSeparationAmount firstBox secondBox of
                        Just squaredDistance ->
                            squaredDistance
                                |> Quantity.greaterThan
                                    (Quantity.squared tolerance)

                        Nothing ->
                            False

            else
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox /= Nothing

        EQ ->
            if tolerance |> Quantity.greaterThanOrEqualTo Quantity.zero then
                let
                    expected =
                        Just (Quantity.squared tolerance)
                in
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox == expected

            else
                alwaysFalse


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).

    outerBox =
        BoundingBox3d.fromExtrema
            { minX = 0
            , maxX = 10
            , minY = 0
            , maxY = 10
            , minZ = 0
            , maxZ = 10
            }

    innerBox =
        BoundingBox3d.fromExtrema
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 9
            , minZ = 7
            , maxZ = 8
            }

    overlappingBox =
        BoundingBox3d.fromExtrema
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
isContainedIn : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Bool
isContainedIn other boundingBox =
    (minX other |> Quantity.lessThanOrEqualTo (minX boundingBox))
        && (maxX boundingBox |> Quantity.lessThanOrEqualTo (maxX other))
        && (minY other |> Quantity.lessThanOrEqualTo (minY boundingBox))
        && (maxY boundingBox |> Quantity.lessThanOrEqualTo (maxY other))
        && (minZ other |> Quantity.lessThanOrEqualTo (minZ boundingBox))
        && (maxZ boundingBox |> Quantity.lessThanOrEqualTo (maxZ other))


{-| Build a bounding box that contains both given bounding boxes.

    firstBox =
        BoundingBox3d.fromExtrema
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            , minZ = 0
            , maxZ = 5
            }

    secondBox =
        BoundingBox3d.fromExtrema
            { minX = -2
            , maxX = 2
            , minY = 4
            , maxY = 5
            , minZ = -1
            , maxZ = 0
            }

    BoundingBox3d.hull firstBox secondBox
    --> BoundingBox3d.fromExtrema
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = 2
    -->     , maxY = 5
    -->     , minZ = -1
    -->     , maxZ = 5
    -->     }

-}
hull : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
hull firstBox secondBox =
    fromExtrema
        { minX = Quantity.min (minX firstBox) (minX secondBox)
        , maxX = Quantity.max (maxX firstBox) (maxX secondBox)
        , minY = Quantity.min (minY firstBox) (minY secondBox)
        , maxY = Quantity.max (maxY firstBox) (maxY secondBox)
        , minZ = Quantity.min (minZ firstBox) (minZ secondBox)
        , maxZ = Quantity.max (maxZ firstBox) (maxZ secondBox)
        }


{-| Attempt to build a bounding box that contains all points common to both
given bounding boxes. If the given boxes do not overlap, returns `Nothing`.

    firstBox =
        BoundingBox3d.fromExtrema
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            , minZ = 5
            , maxZ = 8
            }

    secondBox =
        BoundingBox3d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 4
            , minZ = 6
            , maxZ = 7
            }

    thirdBox =
        BoundingBox3d.fromExtrema
            { minX = 1
            , maxX = 4
            , minY = 4
            , maxY = 5
            , minZ = 5
            , maxZ = 8
            }

    BoundingBox3d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox3d.fromExtrema
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

If two boxes just touch along an edge or at a corner, they are still considered
to have an intersection, even though that intersection will have zero area (at
least one of its dimensions will be zero):

    firstBox =
        BoundingBox3d.fromExtrema
            { minX = 0
            , maxX = 1
            , minY = 0
            , maxY = 2
            , minZ = 0
            , maxZ = 3
            }

    secondBox =
        BoundingBox3d.fromExtrema
            { minX = 1
            , maxX = 2
            , minY = 1
            , maxY = 3
            , minZ = 1
            , maxZ = 4
            }

    BoundingBox3d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox3d.fromExtrema
    -->         { minX = 1
    -->         , maxX = 1
    -->         , minY = 1
    -->         , maxY = 2
    -->         , minZ = 1
    -->         , maxZ = 3
    -->         }
    -->     )

-}
intersection : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Maybe (BoundingBox3d units coordinates)
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        Just
            (fromExtrema
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

    point =
        Point3d.fromCoordinates ( 2, 2, 2 )

    BoundingBox3d.scaleAbout point 2 exampleBox
    --> BoundingBox3d.fromExtrema
    -->     { minX = -6
    -->     , maxX = 2
    -->     , minY = 2
    -->     , maxY = 8
    -->     , minZ = 4
    -->     , maxZ = 6
    -->     }

-}
scaleAbout : Point3d units coordinates -> Float -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
scaleAbout point scale boundingBox =
    let
        ( x0, y0, z0 ) =
            Point3d.coordinates point

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
        fromExtrema
            { minX = scaledMinX
            , maxX = scaledMaxX
            , minY = scaledMinY
            , maxY = scaledMaxY
            , minZ = scaledMinZ
            , maxZ = scaledMaxZ
            }

    else
        fromExtrema
            { minX = scaledMaxX
            , maxX = scaledMinX
            , minY = scaledMaxY
            , maxY = scaledMinY
            , minZ = scaledMaxZ
            , maxZ = scaledMinZ
            }


{-| Translate a bounding box by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, -3, 1 )

    BoundingBox3d.translateBy displacement exampleBox
    --> BoundingBox3d.fromExtrema
    -->     { minX = 0
    -->     , maxX = 4
    -->     , minY = -1
    -->     , maxY = 2
    -->     , minZ = 4
    -->     , maxZ = 5
    -->     }

-}
translateBy : Vector3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
translateBy displacement boundingBox =
    let
        ( dx, dy, dz ) =
            Vector3d.components displacement
    in
    fromExtrema
        { minX = minX boundingBox |> Quantity.plus dx
        , maxX = maxX boundingBox |> Quantity.plus dx
        , minY = minY boundingBox |> Quantity.plus dy
        , maxY = maxY boundingBox |> Quantity.plus dy
        , minZ = minZ boundingBox |> Quantity.plus dz
        , maxZ = maxZ boundingBox |> Quantity.plus dz
        }


{-| Translate a bounding box in a given direction by a given distance;

    BoundingBox3d.translateIn direction distance

is equivalent to

    BoundingBox3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d coordinates -> Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
translateIn direction distance boundingBox =
    translateBy (Vector3d.withLength distance direction) boundingBox


{-| Offsets boundingBox irrespective of the resulting bounding box is valid or not.
-}
unsafeOffsetBy : Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
unsafeOffsetBy amount boundingBox =
    fromExtrema
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

    BoundingBox3d.offsetBy 2 exampleBox
    --> Just <|
    -->     BoundingBox3d.fromExtrema
    -->         { minX = -4
    -->         , maxX = 4
    -->         , minY = 0
    -->         , maxY = 7
    -->         , minZ = 1
    -->         , maxZ = 6
    -->         }

    BoundingBox3d.offsetBy -0.5 exampleBox
    --> Just <|
    -->     BoundingBox3d.fromExtrema
    -->         { minX = -1.5
    -->         , maxX = 1.5
    -->         , minY = 2.5
    -->         , maxY = 4.5
    -->         , minZ = 3.5
    -->         , maxZ = 3.5
    -->         }

Returns `Nothing` if the offset is negative and large enough to cause the
bounding box to vanish (that is, if the offset is larger than half the height or
half the width of the bounding box, whichever is less):

    BoundingBox3d.offsetBy -1 exampleBox
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

    BoundingBox3d.expandBy 3 exampleBox
    --> BoundingBox3d.fromExtrema
    -->     { minX = -5
    -->     , maxX = 5
    -->     , minY = -1
    -->     , maxY = 8
    -->     , minZ = 0
    -->     , maxZ = 7
    -->     }

Negative offsets will be treated as positive (the absolute value will be used),
so the resulting box will always be at least as large as the original. If you
need to be able to contract a bounding box, use
[`offsetBy`](BoundingBox3d#offsetBy) instead.

-}
expandBy : Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
expandBy amount boundingBox =
    unsafeOffsetBy (Quantity.abs amount) boundingBox
