{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.BoundingBox2d
    exposing
        ( singleton
        , containing2
        , containing3
        , containing
        , extrema
        , minX
        , maxX
        , minY
        , maxY
        , midX
        , midY
        , centroid
        , contains
        , overlaps
        , isContainedIn
        , hull
        , intersection
        )

{-| Various functions for creating and working with `BoundingBox2d` values. For
the examples below, assume that all OpenSolid core types have been imported
using

    import OpenSolid.Core.Types exposing (..)

and all necessary modules have been imported using the following pattern:

    import OpenSolid.BoundingBox2d as BoundingBox2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Constructors

Bounding boxes can be constructed by passing a record with `minX`, `maxX`,
`minY` and `maxY` fields to the `BoundingBox2d` constructor, for example

    boundingBox =
        BoundingBox2d
            { minX = 1
            , maxX = 3
            , minY = -2
            , maxY = 4
            }

@docs singleton, containing2, containing3, containing

# Accessors

For all examples in this section, assume the following example bounding box:

    exampleBox =
        BoundingBox2d
            { minX = 1
            , maxX = 3
            , minY = -2
            , maxY = 4
            }

@docs extrema, minX, maxX, minY, maxY, midX, midY, centroid

# Checks

@docs contains, overlaps, isContainedIn

# Boolean operations

@docs hull, intersection
-}

import OpenSolid.Core.Types exposing (..)
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


{-| Construct a bounding box containing two arbitrary points.

    point1 =
        Point2d ( 2, 3 )

    point2 =
        Point2d ( -1, 5 )

    BoundingBox2d.containing2 ( point1, point2 ) ==
        BoundingBox2d
            { minX = -1
            , maxX = 2
            , minY = 3
            , maxY = 5
            }
-}
containing2 : ( Point2d, Point2d ) -> BoundingBox2d
containing2 points =
    let
        ( firstPoint, secondPoint ) =
            points

        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
        BoundingBox2d
            { minX = min x1 x2
            , maxX = max x1 x2
            , minY = min y1 y2
            , maxY = max y1 y2
            }


{-| Construct a bounding box containing three arbitrary points.

    point1 =
        Point2d ( 2, 3 )

    point2 =
        Point2d ( -1, 5 )

    point3 =
        Point2d ( 6, 4 )

    BoundingBox2d.containing2 ( point1, point2 ) ==
        BoundingBox2d
            { minX = -1
            , maxX = 6
            , minY = 3
            , maxY = 5
            }
-}
containing3 : ( Point2d, Point2d, Point2d ) -> BoundingBox2d
containing3 points =
    let
        ( firstPoint, secondPoint, thirdPoint ) =
            points

        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint

        ( x3, y3 ) =
            Point2d.coordinates thirdPoint
    in
        BoundingBox2d
            { minX = min x1 (min x2 x3)
            , maxX = max x1 (max x2 x3)
            , minY = min y1 (min y2 y3)
            , maxY = max y1 (max y2 y3)
            }


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
extrema (BoundingBox2d extrema') =
    extrema'


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


contains : Point2d -> BoundingBox2d -> Bool
contains point boundingBox =
    let
        ( x, y ) =
            Point2d.coordinates point

        { minX, maxX, minY, maxY } =
            extrema boundingBox
    in
        (minX <= x && x <= maxX) && (minY <= y && y <= maxY)


overlaps : BoundingBox2d -> BoundingBox2d -> Bool
overlaps other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)


isContainedIn : BoundingBox2d -> BoundingBox2d -> Bool
isContainedIn other boundingBox =
    (minX other <= minX boundingBox && maxX boundingBox <= maxX other)
        && (minY other <= minY boundingBox && maxY boundingBox <= maxY other)


hull : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
hull other boundingBox =
    BoundingBox2d
        { minX = min (minX boundingBox) (minX other)
        , maxX = max (maxX boundingBox) (maxX other)
        , minY = min (minY boundingBox) (minY other)
        , maxY = max (maxY boundingBox) (maxY other)
        }


intersection : BoundingBox2d -> BoundingBox2d -> Maybe BoundingBox2d
intersection other boundingBox =
    if overlaps other boundingBox then
        Just
            (BoundingBox2d
                { minX = max (minX boundingBox) (minX other)
                , maxX = min (maxX boundingBox) (maxX other)
                , minY = max (minY boundingBox) (minY other)
                , maxY = min (maxY boundingBox) (maxY other)
                }
            )
    else
        Nothing
