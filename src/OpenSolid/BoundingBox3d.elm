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
        ( singleton
        , containing
        , hullOf
        , extrema
        , minX
        , maxX
        , minY
        , maxY
        , minZ
        , maxZ
        , dimensions
        , midX
        , midY
        , midZ
        , centroid
        , contains
        , overlaps
        , isContainedIn
        , hull
        , intersection
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
    boxes *do* overlap.
  - 3D rendering: When rendering a 3D scene, any object whose bounding box is
    not visible must itself be not visible, and therefore does not have to be
    drawn. This provides a simple form of culling.

Bounding boxes can be constructed by passing a record with `minX`, `maxX`,
`minY`, `maxY`, `minZ` and `maxZ` fields to the `BoundingBox3d` constructor, for
example

    exampleBox =
        BoundingBox3d
            { minX = -2
            , maxX = 2
            , minY = 2
            , maxY = 5
            , minZ = 3
            , maxZ = 4
            }

If you construct a `BoundingBox3d` this way, **you must ensure that the given
values are properly ordered**: <code>minX&nbsp;<=&nbsp;maxX</code>,
<code>minY&nbsp;<=&nbsp;maxY</code>, <code>minZ&nbsp;<=&nbsp;maxZ</code>.
Alternately, you can construct bounding boxes using functions such as
[`Point3d.hull`](OpenSolid-Point3d#hull) where the input order does not matter.


# Constructors

@docs singleton, containing, hullOf


# Accessors

@docs extrema, minX, maxX, minY, maxY, minZ, maxZ
@docs dimensions, midX, midY, midZ, centroid


# Checks

@docs contains, overlaps, isContainedIn


# Boolean operations

@docs hull, intersection

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d


{-| Construct a zero-width bounding box containing a single point.

    point =
        Point3d ( 2, 1, 3 )

    BoundingBox3d.singleton point
    --> BoundingBox3d
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
        BoundingBox3d
            { minX = x
            , maxX = x
            , minY = y
            , maxY = y
            , minZ = z
            , maxZ = z
            }


{-| Construct a bounding box containing all points in the given list. If the
list is empty, returns `Nothing`.

    points =
        [ Point3d ( 2, 1, 3 )
        , Point3d ( -1, 5, -2 )
        , Point3d ( 6, 4, 2 )
        ]

    BoundingBox3d.containing points
    --> Just
    -->     (BoundingBox3d
    -->         { minX = -1
    -->         , maxX = 6
    -->         , minY = 1
    -->         , maxY = 5
    -->         , minZ = -2
    -->         , maxZ = 3
    -->         }
    -->     )

    BoundingBox3d.containing []
    --> Nothing

If you have exactly two points, you can use [`Point3d.hull`](OpenSolid-Point3d#hull)
instead (which returns a `BoundingBox3d` instead of a `Maybe BoundingBox3d`).

-}
containing : List Point3d -> Maybe BoundingBox3d
containing points =
    hullOf (List.map singleton points)


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`.

    singletonBox =
        BoundingBox3d.singleton (Point3d ( 2, 1, 0 ))

    BoundingBox3d.hullOf [ exampleBox, singletonBox ]
    --> Just
    -->     (BoundingBox3d
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
extrema (BoundingBox3d properties) =
    properties


{-| Get the minimum X value of a bounding box.

    BoundingBox3d.minX exampleBox
    --> -2

-}
minX : BoundingBox3d -> Float
minX (BoundingBox3d properties) =
    properties.minX


{-| Get the maximum X value of a bounding box.

    BoundingBox3d.maxX exampleBox
    --> 2

-}
maxX : BoundingBox3d -> Float
maxX (BoundingBox3d properties) =
    properties.maxX


{-| Get the minimum Y value of a bounding box.

    BoundingBox3d.minY exampleBox
    --> 2

-}
minY : BoundingBox3d -> Float
minY (BoundingBox3d properties) =
    properties.minY


{-| Get the maximum Y value of a bounding box.

    BoundingBox3d.maxY exampleBox
    --> 5

-}
maxY : BoundingBox3d -> Float
maxY (BoundingBox3d properties) =
    properties.maxY


{-| Get the minimum Z value of a bounding box.

    BoundingBox3d.minZ exampleBox
    --> 3

-}
minZ : BoundingBox3d -> Float
minZ (BoundingBox3d properties) =
    properties.minZ


{-| Get the maximum Z value of a bounding box.

    BoundingBox3d.maxZ exampleBox
    --> 4

-}
maxZ : BoundingBox3d -> Float
maxZ (BoundingBox3d properties) =
    properties.maxZ


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
    --> Point3d ( 0, 3.5, 3.5 )

-}
centroid : BoundingBox3d -> Point3d
centroid boundingBox =
    Point3d ( midX boundingBox, midY boundingBox, midZ boundingBox )


{-| Check if a bounding box contains a particular point.

    firstPoint =
        Point3d ( 1, 4, 3 )

    secondPoint =
        Point3d ( 3, 4, 5 )

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
    in
        (minX boundingBox <= x && x <= maxX boundingBox)
            && (minY boundingBox <= y && y <= maxY boundingBox)
            && (minZ boundingBox <= z && z <= maxZ boundingBox)


{-| Test if one bounding box overlaps (touches) another.

    firstBox =
        BoundingBox3d
            { minX = 0
            , maxX = 3
            , minY = 0
            , maxY = 2
            , minZ = 0
            , maxZ = 1
            }

    secondBox =
        BoundingBox3d
            { minX = 0
            , maxX = 3
            , minY = 1
            , maxY = 4
            , minZ = -1
            , maxZ = 2
            }

    thirdBox =
        BoundingBox3d
            { minX = 0
            , maxX = 3
            , minY = 4
            , maxY = 5
            , minZ = -1
            , maxZ = 2
            }

    BoundingBox3d.overlaps firstBox secondBox
    --> True

    BoundingBox3d.overlaps firstBox thirdBox
    --> False

-}
overlaps : BoundingBox3d -> BoundingBox3d -> Bool
overlaps other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)
        && (minZ boundingBox <= maxZ other)
        && (maxZ boundingBox >= minZ other)


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).

    outerBox =
        BoundingBox3d
            { minX = 0
            , maxX = 10
            , minY = 0
            , maxY = 10
            , minZ = 0
            , maxZ = 10
            }

    innerBox =
        BoundingBox3d
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 9
            , minZ = 7
            , maxZ = 8
            }

    overlappingBox =
        BoundingBox3d
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
        BoundingBox3d
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            , minZ = 0
            , maxZ = 5
            }

    secondBox =
        BoundingBox3d
            { minX = -2
            , maxX = 2
            , minY = 4
            , maxY = 5
            , minZ = -1
            , maxZ = 0
            }

    BoundingBox3d.hull firstBox secondBox
    --> BoundingBox3d
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
    BoundingBox3d
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
        BoundingBox3d
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            , minZ = 5
            , maxZ = 8
            }

    secondBox =
        BoundingBox3d
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 4
            , minZ = 6
            , maxZ = 7
            }

    thirdBox =
        BoundingBox3d
            { minX = 1
            , maxX = 4
            , minY = 4
            , maxY = 5
            , minZ = 5
            , maxZ = 8
            }

    BoundingBox3d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox3d
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
    if overlaps firstBox secondBox then
        Just
            (BoundingBox3d
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
