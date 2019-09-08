--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module BoundingBox3d exposing
    ( BoundingBox3d
    , fromExtrema, singleton, intersection
    , hull, hullOf, hull2, hull3, hullN
    , extrema, minX, maxX, minY, maxY, minZ, maxZ, dimensions, midX, midY, midZ, centerPoint
    , contains, isContainedIn, intersects, overlappingByAtLeast, separatedByAtLeast
    , scaleAbout, translateBy, translateIn, expandBy, offsetBy
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

@docs fromExtrema, singleton, intersection


## Hull

@docs hull, hullOf, hull2, hull3, hullN


# Properties

@docs extrema, minX, maxX, minY, maxY, minZ, maxZ, dimensions, midX, midY, midZ, centerPoint


# Queries

@docs contains, isContainedIn, intersects, overlappingByAtLeast, separatedByAtLeast


# Transformations

@docs scaleAbout, translateBy, translateIn, expandBy, offsetBy

-}

import Direction3d exposing (Direction3d)
import Geometry.Types as Types
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), Squared)
import Quantity.Extra as Quantity
import Vector3d exposing (Vector3d)


{-| -}
type alias BoundingBox3d units coordinates =
    Types.BoundingBox3d units coordinates


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


{-| Construct a zero-width bounding box containing a single point.

    point =
        Point3d.meters 2 1 3

    BoundingBox3d.singleton point
    --> BoundingBox3d.fromExtrema
    -->     { minX = Length.meters 2
    -->     , maxX = Length.meters 2
    -->     , minY = Length.meters 1
    -->     , maxY = Length.meters 1
    -->     , minZ = Length.meters 3
    -->     , maxZ = Length.meters 3
    -->     }

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


{-| Find the bounding box containing one or more input boxes. If you need to
handle the case of zero input boxes, see `hullN`.
-}
hull : BoundingBox3d units coordinates -> List (BoundingBox3d units coordinates) -> BoundingBox3d units coordinates
hull first rest =
    let
        b1 =
            extrema first
    in
    hullHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ rest


hullHelp : Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> List (BoundingBox3d units coordinates) -> BoundingBox3d units coordinates
hullHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ boxes =
    case boxes of
        next :: rest ->
            let
                b =
                    extrema next
            in
            hullHelp
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


{-| Like `hull`, but lets you work on any kind of item as long as a bounding
box can be extracted from it. For example, to get the bounding box around four
triangles:

    BoundingBox3d.hullOf Triangle3d.boundingBox
        firstTriangle
        [ secondTriangle
        , thirdTriangle
        , fourthTriangle
        ]

-}
hullOf : (a -> BoundingBox3d units coordinates) -> a -> List a -> BoundingBox3d units coordinates
hullOf getBoundingBox first rest =
    let
        b1 =
            extrema (getBoundingBox first)
    in
    hullOfHelp b1.minX b1.maxX b1.minY b1.maxY b1.minZ b1.maxZ getBoundingBox rest


hullOfHelp : Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> (a -> BoundingBox3d units coordiantes) -> List a -> BoundingBox3d units coordinates
hullOfHelp currentMinX currentMaxX currentMinY currentMaxY currentMinZ currentMaxZ getBoundingBox items =
    case items of
        next :: rest ->
            let
                b =
                    extrema (getBoundingBox next)
            in
            hullOfHelp
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
        BoundingBox3d.fromExtrema
            { minX = Length.meters 1
            , maxX = Length.meters 4
            , minY = Length.meters 2
            , maxY = Length.meters 3
            , minZ = Length.meters 0
            , maxZ = Length.meters 5
            }

    secondBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters -2
            , maxX = Length.meters 2
            , minY = Length.meters 4
            , maxY = Length.meters 5
            , minZ = Length.meters -1
            , maxZ = Length.meters 0
            }

    BoundingBox3d.hull2 firstBox secondBox
    --> BoundingBox3d.fromExtrema
    -->     { minX = Length.meters -2
    -->     , maxX = Length.meters 4
    -->     , minY = Length.meters 2
    -->     , maxY = Length.meters 5
    -->     , minZ = Length.meters -1
    -->     , maxZ = Length.meters 5
    -->     }

-}
hull2 : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
hull2 firstBox secondBox =
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


{-| Build a bounding box that contains all three of the given bounding boxes.
-}
hull3 : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates
hull3 firstBox secondBox thirdBox =
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
the list is empty, returns `Nothing`.

    singletonBox =
        BoundingBox3d.singleton (Point3d.meters 2 1 0)

    BoundingBox3d.hullN [ exampleBox, singletonBox ]
    --> Just
    -->     (BoundingBox3d.fromExtrema
    -->         { minX = Length.meters -2
    -->         , maxX = Length.meters 2
    -->         , minY = Length.meters 1
    -->         , maxY = Length.meters 5
    -->         , minZ = Length.meters 0
    -->         , maxZ = Length.meters 4
    -->         }
    -->     )

    BoundingBox3d.hullN [ exampleBox ]
    --> Just exampleBox

    BoundingBox3d.hullN []
    --> Nothing

If you know you have at least one bounding box, you can use `hull` instead.

-}
hullN : List (BoundingBox3d units coordinates) -> Maybe (BoundingBox3d units coordinates)
hullN boxes =
    case boxes of
        first :: rest ->
            Just (hull first rest)

        [] ->
            Nothing


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

Can be useful when combined with record destructuring, for example


    { minX, maxX, minY, maxY, minZ, maxZ } =
        BoundingBox3d.extrema exampleBox

    --> minX = Length.meters -2
    --> maxX = Length.meters 2
    --> minY = Length.meters 2
    --> maxY = Length.meters 5
    --> minZ = Length.meters 3
    --> maxZ = Length.meters 4

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
    --> Length.meters -2

-}
minX : BoundingBox3d units coordinates -> Quantity Float units
minX (Types.BoundingBox3d boundingBox) =
    boundingBox.minX


{-| Get the maximum X value of a bounding box.

    BoundingBox3d.maxX exampleBox
    --> Length.meters 2

-}
maxX : BoundingBox3d units coordinates -> Quantity Float units
maxX (Types.BoundingBox3d boundingBox) =
    boundingBox.maxX


{-| Get the minimum Y value of a bounding box.

    BoundingBox3d.minY exampleBox
    --> Length.meters 2

-}
minY : BoundingBox3d units coordinates -> Quantity Float units
minY (Types.BoundingBox3d boundingBox) =
    boundingBox.minY


{-| Get the maximum Y value of a bounding box.

    BoundingBox3d.maxY exampleBox
    --> Length.meters 5

-}
maxY : BoundingBox3d units coordinates -> Quantity Float units
maxY (Types.BoundingBox3d boundingBox) =
    boundingBox.maxY


{-| Get the minimum Z value of a bounding box.

    BoundingBox3d.minZ exampleBox
    --> Length.meters 3

-}
minZ : BoundingBox3d units coordinates -> Quantity Float units
minZ (Types.BoundingBox3d boundingBox) =
    boundingBox.minZ


{-| Get the maximum Z value of a bounding box.

    BoundingBox3d.maxZ exampleBox
    --> Length.meters 4

-}
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


{-| Get the median X value of a bounding box.

    BoundingBox3d.midX exampleBox
    --> Length.meters 0

-}
midX : BoundingBox3d units coordinates -> Quantity Float units
midX (Types.BoundingBox3d boundingBox) =
    Quantity.interpolateFrom boundingBox.minX boundingBox.maxX 0.5


{-| Get the median Y value of a bounding box.

    BoundingBox3d.midY exampleBox
    --> Length.meters 3.5

-}
midY : BoundingBox3d units coordinates -> Quantity Float units
midY (Types.BoundingBox3d boundingBox) =
    Quantity.interpolateFrom boundingBox.minY boundingBox.maxY 0.5


{-| Get the median Z value of a bounding box.

    BoundingBox3d.midZ exampleBox
    --> Length.meters 3.5

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


{-| Check if a bounding box contains a particular point.

    firstPoint =
        Point3d.meters 1 4 3

    secondPoint =
        Point3d.meters 3 4 5

    BoundingBox3d.contains firstPoint exampleBox
    --> True

    BoundingBox3d.contains secondPoint exampleBox
    --> False

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

    firstBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 0
            , maxX = Length.meters 3
            , minY = Length.meters 0
            , maxY = Length.meters 2
            , minZ = Length.meters 0
            , maxZ = Length.meters 1
            }

    secondBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 0
            , maxX = Length.meters 3
            , minY = Length.meters 1
            , maxY = Length.meters 4
            , minZ = Length.meters -1
            , maxZ = Length.meters 2
            }

    thirdBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 0
            , maxX = Length.meters 3
            , minY = Length.meters 4
            , maxY = Length.meters 5
            , minZ = Length.meters -1
            , maxZ = Length.meters 2
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
        BoundingBox3d.fromExtrema
            { minX = Length.meters 0
            , maxX = Length.meters 10
            , minY = Length.meters 0
            , maxY = Length.meters 10
            , minZ = Length.meters 0
            , maxZ = Length.meters 10
            }

    innerBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 1
            , maxX = Length.meters 5
            , minY = Length.meters 3
            , maxY = Length.meters 9
            , minZ = Length.meters 7
            , maxZ = Length.meters 8
            }

    overlappingBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 1
            , maxX = Length.meters 5
            , minY = Length.meters 3
            , maxY = Length.meters 12
            , minZ = Length.meters 7
            , maxZ = Length.meters 8
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


{-| Attempt to build a bounding box that contains all points common to both
given bounding boxes. If the given boxes do not overlap, returns `Nothing`.

    firstBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 1
            , maxX = Length.meters 4
            , minY = Length.meters 2
            , maxY = Length.meters 3
            , minZ = Length.meters 5
            , maxZ = Length.meters 8
            }

    secondBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 2
            , maxX = Length.meters 5
            , minY = Length.meters 1
            , maxY = Length.meters 4
            , minZ = Length.meters 6
            , maxZ = Length.meters 7
            }

    thirdBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 1
            , maxX = Length.meters 4
            , minY = Length.meters 4
            , maxY = Length.meters 5
            , minZ = Length.meters 5
            , maxZ = Length.meters 8
            }

    BoundingBox3d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox3d.fromExtrema
    -->         { minX = Length.meters 2
    -->         , maxX = Length.meters 4
    -->         , minY = Length.meters 2
    -->         , maxY = Length.meters 3
    -->         , minZ = Length.meters 6
    -->         , maxZ = Length.meters 7
    -->         }
    -->     )

    BoundingBox3d.intersection firstBox thirdBox
    --> Nothing

If two boxes just touch along an edge or at a corner, they are still considered
to have an intersection, even though that intersection will have zero area (at
least one of its dimensions will be zero):

    firstBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 0
            , maxX = Length.meters 1
            , minY = Length.meters 0
            , maxY = Length.meters 2
            , minZ = Length.meters 0
            , maxZ = Length.meters 3
            }

    secondBox =
        BoundingBox3d.fromExtrema
            { minX = Length.meters 1
            , maxX = Length.meters 2
            , minY = Length.meters 1
            , maxY = Length.meters 3
            , minZ = Length.meters 1
            , maxZ = Length.meters 4
            }

    BoundingBox3d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox3d.fromExtrema
    -->         { minX = Length.meters 1
    -->         , maxX = Length.meters 1
    -->         , minY = Length.meters 1
    -->         , maxY = Length.meters 2
    -->         , minZ = Length.meters 1
    -->         , maxZ = Length.meters 3
    -->         }
    -->     )

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

    point =
        Point3d.meters 2 2 2

    BoundingBox3d.scaleAbout point 2 exampleBox
    --> BoundingBox3d.fromExtrema
    -->     { minX = Length.meters -6
    -->     , maxX = Length.meters 2
    -->     , minY = Length.meters 2
    -->     , maxY = Length.meters 8
    -->     , minZ = Length.meters 4
    -->     , maxZ = Length.meters 6
    -->     }

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

    displacement =
        Vector3d.meters 2 -3 1

    BoundingBox3d.translateBy displacement exampleBox
    --> BoundingBox3d.fromExtrema
    -->     { minX = Length.meters 0
    -->     , maxX = Length.meters 4
    -->     , minY = Length.meters -1
    -->     , maxY = Length.meters 2
    -->     , minZ = Length.meters 4
    -->     , maxZ = Length.meters 5
    -->     }

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


{-| Translate a bounding box in a given direction by a given distance;

    BoundingBox3d.translateIn direction distance

is equivalent to

    BoundingBox3d.translateBy
        (Vector3d.withLength distance direction)

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

    BoundingBox3d.offsetBy (Length.meters 2) exampleBox
    --> Just <|
    -->     BoundingBox3d.fromExtrema
    -->         { minX = Length.meters -4
    -->         , maxX = Length.meters 4
    -->         , minY = Length.meters 0
    -->         , maxY = Length.meters 7
    -->         , minZ = Length.meters 1
    -->         , maxZ = Length.meters 6
    -->         }

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
