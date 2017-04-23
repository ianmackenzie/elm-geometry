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
        ( singleton
        , containing
        , hullOf
        , extrema
        , minX
        , maxX
        , minY
        , maxY
        , dimensions
        , midX
        , midY
        , centroid
        , contains
        , overlaps
        , isContainedIn
        , hull
        , intersection
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
    boxes *do* overlap.
  - 2D rendering: When rendering a 2D scene, any object whose bounding box does
    not overlap the viewing area must itself be completely outside the viewing
    area, and therefore does not have to be drawn. This provides a simple form
    of culling.

Bounding boxes can be constructed by passing a record with `minX`, `maxX`,
`minY` and `maxY` fields to the `BoundingBox2d` constructor, for example

    exampleBox =
        BoundingBox2d
            { minX = 3
            , maxX = 8
            , minY = 2
            , maxY = 6
            }

If you construct a `BoundingBox2d` this way, **you must ensure that the given
values are properly ordered**: <code>minX&nbsp;<=&nbsp;maxX</code>,
<code>minY&nbsp;<=&nbsp;maxY</code>. Alternately, you can construct bounding
boxes using functions such as [`Point2d.hull`](OpenSolid-Point2d#hull) where the
input order does not matter.


# Constructors

@docs singleton, containing, hullOf


# Accessors

@docs extrema, minX, maxX, minY, maxY
@docs dimensions, midX, midY, centroid


# Checks

@docs contains, overlaps, isContainedIn


# Boolean operations

@docs hull, intersection

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d


{-| Construct a zero-width bounding box containing a single point.

    point =
        Point2d ( 2, 3 )

    BoundingBox2d.singleton point
    --> BoundingBox2d
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
        BoundingBox2d { minX = x, maxX = x, minY = y, maxY = y }


{-| Construct a bounding box containing all points in the given list. If the
list is empty, returns `Nothing`.

    points =
        [ Point2d ( 2, 3 )
        , Point2d ( -1, 5 )
        , Point2d ( 6, 4 )
        ]

    BoundingBox2d.containing points
    --> Just
    -->     (BoundingBox2d
    -->         { minX = -1
    -->         , maxX = 6
    -->         , minY = 3
    -->         , maxY = 5
    -->         }
    -->     )

    BoundingBox2d.containing []
    --> Nothing

If you have exactly two points, you can use [`Point2d.hull`](OpenSolid-Point2d#hull)
instead (which returns a `BoundingBox2d` instead of a `Maybe BoundingBox2d`).

-}
containing : List Point2d -> Maybe BoundingBox2d
containing points =
    hullOf (List.map singleton points)


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`.

    singletonBox =
        BoundingBox2d.singleton (Point2d ( 1, 3 ))

    BoundingBox2d.hullOf [ exampleBox, singletonBox ]
    --> Just
    -->     (BoundingBox2d
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
extrema (BoundingBox2d properties) =
    properties


{-| Get the minimum X value of a bounding box.

    BoundingBox2d.minX exampleBox
    --> 3

-}
minX : BoundingBox2d -> Float
minX (BoundingBox2d properties) =
    properties.minX


{-| Get the maximum X value of a bounding box.

    BoundingBox2d.maxX exampleBox
    --> 8

-}
maxX : BoundingBox2d -> Float
maxX (BoundingBox2d properties) =
    properties.maxX


{-| Get the minimum Y value of a bounding box.

    BoundingBox2d.minY exampleBox
    --> 2

-}
minY : BoundingBox2d -> Float
minY (BoundingBox2d properties) =
    properties.minY


{-| Get the maximum Y value of a bounding box.

    BoundingBox2d.maxY exampleBox
    --> 6

-}
maxY : BoundingBox2d -> Float
maxY (BoundingBox2d properties) =
    properties.maxY


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
    --> Point2d ( 5.5, 4 )

-}
centroid : BoundingBox2d -> Point2d
centroid boundingBox =
    Point2d ( midX boundingBox, midY boundingBox )


{-| Check if a bounding box contains a particular point.

    BoundingBox2d.contains (Point2d ( 4, 3 )) exampleBox
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
        BoundingBox2d
            { minX = 0
            , maxX = 3
            , minY = 0
            , maxY = 2
            }

    secondBox =
        BoundingBox2d
            { minX = 0
            , maxX = 3
            , minY = 1
            , maxY = 4
            }

    thirdBox =
        BoundingBox2d
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


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).

    outerBox =
        BoundingBox2d
            { minX = 0
            , maxX = 10
            , minY = 0
            , maxY = 10
            }

    innerBox =
        BoundingBox2d
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 9
            }

    overlappingBox =
        BoundingBox2d
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
        BoundingBox2d
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            }

    secondBox =
        BoundingBox2d
            { minX = -2
            , maxX = 2
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.hull firstBox secondBox
    --> BoundingBox2d
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = 2
    -->     , maxY = 5
    -->     }

-}
hull : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
hull firstBox secondBox =
    BoundingBox2d
        { minX = min (minX firstBox) (minX secondBox)
        , maxX = max (maxX firstBox) (maxX secondBox)
        , minY = min (minY firstBox) (minY secondBox)
        , maxY = max (maxY firstBox) (maxY secondBox)
        }


{-| Attempt to build a bounding box that contains all points common to both
given bounding boxes. If the given boxes do not overlap, returns `Nothing`.

    firstBox =
        BoundingBox2d
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            }

    secondBox =
        BoundingBox2d
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 4
            }

    thirdBox =
        BoundingBox2d
            { minX = 1
            , maxX = 4
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox2d
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
            (BoundingBox2d
                { minX = max (minX firstBox) (minX secondBox)
                , maxX = min (maxX firstBox) (maxX secondBox)
                , minY = max (minY firstBox) (minY secondBox)
                , maxY = min (maxY firstBox) (maxY secondBox)
                }
            )
    else
        Nothing
