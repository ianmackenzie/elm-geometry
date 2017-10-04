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


module OpenSolid.BoundingBox2d
    exposing
        ( BoundingBox2d
        , centroid
        , contains
        , dimensions
        , extrema
        , hull
        , hullOf
        , intersection
        , isContainedIn
        , maxX
        , maxY
        , midX
        , midY
        , minX
        , minY
        , overlaps
        , singleton
        , strictlyOverlaps
        , with
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/boundingBox2d.svg" alt="BoundingBox2d" width="160">

A `BoundingBox2d` is a rectangular box in 2D defined by its minimum and maximum
X and Y values. It is possible to generate bounding boxes for most geometric
objects; for example, [`Triangle2d.boundingBox`](OpenSolid-Triangle2d#boundingBox)
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

@docs with, singleton, hull, intersection, hullOf


# Properties

@docs extrema, minX, maxX, minY, maxY, dimensions, midX, midY, centroid


# Queries

@docs contains, overlaps, isContainedIn

-}

import OpenSolid.Bootstrap.BoundingBox2d as Bootstrap
import OpenSolid.Bootstrap.Point2d as Point2d
import OpenSolid.Geometry.Internal as Internal exposing (Point2d)


{-| -}
type alias BoundingBox2d =
    Internal.BoundingBox2d


{-| Construct a bounding box from its minimum and maximum X and Y values:

    exampleBox =
        BoundingBox2d.with
            { minX = 3
            , maxX = 8
            , minY = 2
            , maxY = 6
            }

If the minimum and maximum values are provided in the wrong order (for example
if <code>minX&nbsp;>&nbsp;maxX</code>), then they will be swapped so that the
resulting bounding box is valid.

-}
with : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> BoundingBox2d
with =
    Bootstrap.with


{-| Construct a zero-width bounding box containing a single point.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    BoundingBox2d.singleton point
    --> BoundingBox2d.with
    -->     { minX = 2
    -->     , maxX = 2
    -->     , minY = 3
    -->     , maxY = 3
    -->     }

-}
singleton : Point2d -> BoundingBox2d
singleton point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    with { minX = x, maxX = x, minY = y, maxY = y }


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`.

    singletonBox =
        BoundingBox2d.singleton
            (Point2d.fromCoordinates ( 1, 3 ))

    BoundingBox2d.hullOf [ exampleBox, singletonBox ]
    --> Just
    -->     (BoundingBox2d.with
    -->         { minX = 1,
    -->         , maxX = 8
    -->         , minY = 2
    -->         , maxY = 6
    -->         }
    -->     )

    BoundingBox2d.hullOf [ exampleBox ]
    --> Just exampleBox

    BoundingBox2d.hullOf []
    --> Nothing

If you have exactly two bounding boxes, you can use [`BoundingBox2d.hull`](#hull)
instead (which returns a `BoundingBox2d` instead of a `Maybe BoundingBox2d`).

-}
hullOf : List BoundingBox2d -> Maybe BoundingBox2d
hullOf boundingBoxes =
    case boundingBoxes of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Get the minimum and maximum X and Y values of a bounding box in a single
record.

    BoundingBox2d.extrema exampleBox
    --> { minX = 3
    --> , maxX = 8
    --> , minY = 2
    --> , maxY = 6
    --> }

Can be useful when combined with record destructuring, for example

    { minX, maxX, minY, maxY } =
        BoundingBox2d.extrema exampleBox


    --> minX = 3
    --> maxX = 8
    --> minY = 2
    --> maxY = 6

-}
extrema : BoundingBox2d -> { minX : Float, maxX : Float, minY : Float, maxY : Float }
extrema (Internal.BoundingBox2d properties) =
    properties


{-| Get the minimum X value of a bounding box.

    BoundingBox2d.minX exampleBox
    --> 3

-}
minX : BoundingBox2d -> Float
minX boundingBox =
    (extrema boundingBox).minX


{-| Get the maximum X value of a bounding box.

    BoundingBox2d.maxX exampleBox
    --> 8

-}
maxX : BoundingBox2d -> Float
maxX boundingBox =
    (extrema boundingBox).maxX


{-| Get the minimum Y value of a bounding box.

    BoundingBox2d.minY exampleBox
    --> 2

-}
minY : BoundingBox2d -> Float
minY boundingBox =
    (extrema boundingBox).minY


{-| Get the maximum Y value of a bounding box.

    BoundingBox2d.maxY exampleBox
    --> 6

-}
maxY : BoundingBox2d -> Float
maxY boundingBox =
    (extrema boundingBox).maxY


{-| Get the X and Y dimensions (width and height) of a bounding box.

    ( width, height ) =
        BoundingBox2d.dimensions exampleBox


    --> width = 5
    --> height = 4

-}
dimensions : BoundingBox2d -> ( Float, Float )
dimensions boundingBox =
    let
        { minX, maxX, minY, maxY } =
            extrema boundingBox
    in
    ( maxX - minX, maxY - minY )


{-| Get the median X value of a bounding box.

    BoundingBox2d.midX exampleBox
    --> 5.5

-}
midX : BoundingBox2d -> Float
midX boundingBox =
    let
        { minX, maxX } =
            extrema boundingBox
    in
    minX + 0.5 * (maxX - minX)


{-| Get the median Y value of a bounding box.

    BoundingBox2d.midY exampleBox
    --> 4

-}
midY : BoundingBox2d -> Float
midY boundingBox =
    let
        { minY, maxY } =
            extrema boundingBox
    in
    minY + 0.5 * (maxY - minY)


{-| Get the point at the center of a bounding box.

    BoundingBox2d.centroid exampleBox
    --> Point2d.fromCoordinates ( 5.5, 4 )

-}
centroid : BoundingBox2d -> Point2d
centroid boundingBox =
    Point2d.fromCoordinates ( midX boundingBox, midY boundingBox )


{-| Check if a bounding box contains a particular point.

    point =
        Point2d.fromCoordinates ( 4, 3 )

    BoundingBox2d.contains point exampleBox
    --> True

    BoundingBox2d.contains Point2d.origin exampleBox
    --> False

-}
contains : Point2d -> BoundingBox2d -> Bool
contains point boundingBox =
    let
        ( x, y ) =
            Point2d.coordinates point

        { minX, maxX, minY, maxY } =
            extrema boundingBox
    in
    (minX <= x && x <= maxX) && (minY <= y && y <= maxY)


{-| Test if one bounding box overlaps (touches) another.

    firstBox =
        BoundingBox2d.with
            { minX = 0
            , maxX = 3
            , minY = 0
            , maxY = 2
            }

    secondBox =
        BoundingBox2d.with
            { minX = 0
            , maxX = 3
            , minY = 1
            , maxY = 4
            }

    thirdBox =
        BoundingBox2d.with
            { minX = 0
            , maxX = 3
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.overlaps firstBox secondBox
    --> True

    BoundingBox2d.overlaps firstBox thirdBox
    --> False

-}
overlaps : BoundingBox2d -> BoundingBox2d -> Bool
overlaps other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)


strictlyOverlaps : BoundingBox2d -> BoundingBox2d -> Bool
strictlyOverlaps other boundingBox =
    (minX boundingBox < maxX other)
        && (maxX boundingBox > minX other)
        && (minY boundingBox < maxY other)
        && (maxY boundingBox > minY other)


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).

    outerBox =
        BoundingBox2d.with
            { minX = 0
            , maxX = 10
            , minY = 0
            , maxY = 10
            }

    innerBox =
        BoundingBox2d.with
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 9
            }

    overlappingBox =
        BoundingBox2d.with
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 12
            }

    BoundingBox2d.isContainedIn outerBox innerBox
    --> True

    BoundingBox2d.isContainedIn outerBox overlappingBox
    --> False

-}
isContainedIn : BoundingBox2d -> BoundingBox2d -> Bool
isContainedIn other boundingBox =
    (minX other <= minX boundingBox && maxX boundingBox <= maxX other)
        && (minY other <= minY boundingBox && maxY boundingBox <= maxY other)


{-| Build a bounding box that contains both given bounding boxes.

    firstBox =
        BoundingBox2d.with
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            }

    secondBox =
        BoundingBox2d.with
            { minX = -2
            , maxX = 2
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.hull firstBox secondBox
    --> BoundingBox2d.with
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = 2
    -->     , maxY = 5
    -->     }

-}
hull : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
hull firstBox secondBox =
    with
        { minX = min (minX firstBox) (minX secondBox)
        , maxX = max (maxX firstBox) (maxX secondBox)
        , minY = min (minY firstBox) (minY secondBox)
        , maxY = max (maxY firstBox) (maxY secondBox)
        }


{-| Attempt to build a bounding box that contains all points common to both
given bounding boxes. If the given boxes do not overlap, returns `Nothing`.

    firstBox =
        BoundingBox2d.with
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            }

    secondBox =
        BoundingBox2d.with
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 4
            }

    thirdBox =
        BoundingBox2d.with
            { minX = 1
            , maxX = 4
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox2d.with
    -->         { minX = 2
    -->         , maxX = 4
    -->         , minY = 2
    -->         , maxY = 3
    -->         }
    -->     )

    BoundingBox2d.intersection firstBox thirdBox
    --> Nothing

-}
intersection : BoundingBox2d -> BoundingBox2d -> Maybe BoundingBox2d
intersection firstBox secondBox =
    if overlaps firstBox secondBox then
        Just
            (with
                { minX = max (minX firstBox) (minX secondBox)
                , maxX = min (maxX firstBox) (maxX secondBox)
                , minY = max (minY firstBox) (minY secondBox)
                , maxY = min (maxY firstBox) (maxY secondBox)
                }
            )
    else
        Nothing
