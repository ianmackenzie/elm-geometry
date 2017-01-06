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

Various functions for creating and working with `BoundingBox2d` values. A
`BoundingBox2d` is defined by its minimum and maximum X and Y values.

Bounding boxes can be constructed by passing a record with `minX`, `maxX`,
`minY` and `maxY` fields to the `BoundingBox2d` constructor, for example

    exampleBox =
        BoundingBox2d
            { minX = 1
            , maxX = 3
            , minY = -2
            , maxY = 4
            }

If you construct a `BoundingBox2d` this way, you must ensure that the given
values are properly ordered: `minX <= maxX`, `minY <= maxY`. Alternately, you
can construct bounding boxes using functions such as `Point2d.hull` where the
input order does not matter.

## Reading this documentation

For the examples below, assume that `exampleBox` has been defined as above, all
OpenSolid core types have been imported using

    import OpenSolid.Geometry.Types exposing (..)

and all other necessary modules have been imported using the following pattern:

    import OpenSolid.BoundingBox2d as BoundingBox2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Constructors

@docs singleton, containing

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

    BoundingBox2d.singleton point ==
        BoundingBox2d
            { minX = 2
            , maxX = 2
            , minY = 3
            , maxY = 3
            }
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

    BoundingBox2d.containing points ==
        Just
            (BoundingBox2d
                { minX = -1
                , maxX = 6
                , minY = 3
                , maxY = 5
                }
            )

    BoundingBox2d.containing [] ==
        Nothing

If you have exactly two points, you can use `Point2d.hull` instead (which
returns a `BoundingBox2d` instead of a `Maybe BoundingBox2d`).
-}
containing : List Point2d -> Maybe BoundingBox2d
containing points =
    case points of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl hull (singleton first) (List.map singleton rest))


{-| Get the minimum and maximum X and Y values of a bounding box in a single
record.

    BoundingBox2d.extrema exampleBox ==
        { minX = 1
        , maxX = 3
        , minY = -2
        , maxY = 4
        }

Can be useful when combined with record destructuring, for example

    { minX, maxX, minY, maxY } =
        BoundingBox2d.extrema exampleBox
-}
extrema :
    BoundingBox2d
    -> { minX : Float
       , maxX : Float
       , minY : Float
       , maxY : Float
       }
extrema (BoundingBox2d extrema_) =
    extrema_


{-| Get the minimum X value of a bounding box.

    BoundingBox2d.minX exampleBox ==
        1
-}
minX : BoundingBox2d -> Float
minX =
    extrema >> .minX


{-| Get the maximum X value of a bounding box.

    BoundingBox2d.maxX exampleBox ==
        3
-}
maxX : BoundingBox2d -> Float
maxX =
    extrema >> .maxX


{-| Get the minimum Y value of a bounding box.

    BoundingBox2d.minY exampleBox ==
        -2
-}
minY : BoundingBox2d -> Float
minY =
    extrema >> .minY


{-| Get the maximum Y value of a bounding box.

    BoundingBox2d.maxY exampleBox ==
        4
-}
maxY : BoundingBox2d -> Float
maxY =
    extrema >> .maxY


{-| Get the X and Y dimensions (width and height) of a bounding box.

    ( width, height ) =
        BoundingBox2d.dimensions exampleBox

    width ==
        2

    height ==
        6
-}
dimensions : BoundingBox2d -> ( Float, Float )
dimensions boundingBox =
    let
        { minX, maxX, minY, maxY } =
            extrema boundingBox
    in
        ( maxX - minX, maxY - minY )


{-| Get the median X value of a bounding box.

    BoundingBox2d.midX exampleBox ==
        2
-}
midX : BoundingBox2d -> Float
midX boundingBox =
    let
        { minX, maxX } =
            extrema boundingBox
    in
        minX + 0.5 * (maxX - minX)


{-| Get the median Y value of a bounding box.

    BoundingBox2d.midY exampleBox ==
        1
-}
midY : BoundingBox2d -> Float
midY boundingBox =
    let
        { minY, maxY } =
            extrema boundingBox
    in
        minY + 0.5 * (maxY - minY)


{-| Get the point at the center of a bounding box.

    BoundingBox2d.centroid exampleBox ==
        Point2d ( 2, 1 )
-}
centroid : BoundingBox2d -> Point2d
centroid boundingBox =
    Point2d ( midX boundingBox, midY boundingBox )


{-| Check if a bounding box contains a particular point.

    boundingBox =
        BoundingBox2d
            { minX = 0
            , maxX = 2
            , minY = 0
            , maxY = 3
            }

    firstPoint =
        Point2d ( 1, 2 )

    secondPoint =
        Point2d ( 3, 1 )

    BoundingBox2d.contains firstPoint boundingBox ==
        True

    BoundingBox2d.contains secondPoint boundingBox ==
        False
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

    BoundingBox2d.overlaps firstBox secondBox ==
        True

    BoundingBox2d.overlaps firstBox thirdBox ==
        False
-}
overlaps : BoundingBox2d -> BoundingBox2d -> Bool
overlaps other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)


{-| Test if the second given bounding box is fully contained within the first
(is a strict subset of it).

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

    BoundingBox2d.isContainedIn outerBox innerBox ==
        True

    BoundingBox2d.isContainedIn outerBox overlappingBox ==
        False
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

    BoundingBox2d.hull firstBox secondBox ==
        BoundingBox2d
            { minX = -2
            , maxX = 4
            , minY = 2
            , maxY = 5
            }
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

    BoundingBox2d.intersection firstBox secondBox ==
        Just
            (BoundingBox2d
                { minX = 2
                , maxX = 4
                , minY = 2
                , maxY = 3
                }
            )

    BoundingBox2d.intersection firstBox thirdBox ==
        Nothing
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
