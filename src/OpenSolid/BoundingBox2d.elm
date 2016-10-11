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

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point2d as Point2d


singleton : Point2d -> BoundingBox2d
singleton point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        BoundingBox2d { minX = x, maxX = x, minY = y, maxY = y }


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


containing : List Point2d -> Maybe BoundingBox2d
containing points =
    case points of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl hull (singleton first) (List.map singleton rest))


extrema :
    BoundingBox2d
    -> { minX : Float
       , maxX : Float
       , minY : Float
       , maxY : Float
       }
extrema (BoundingBox2d extrema') =
    extrema'


minX : BoundingBox2d -> Float
minX =
    extrema >> .minX


maxX : BoundingBox2d -> Float
maxX =
    extrema >> .maxX


minY : BoundingBox2d -> Float
minY =
    extrema >> .minY


maxY : BoundingBox2d -> Float
maxY =
    extrema >> .maxY


midX : BoundingBox2d -> Float
midX boundingBox =
    let
        { minX, maxX } =
            extrema boundingBox
    in
        minX + 0.5 * (maxX - minX)


midY : BoundingBox2d -> Float
midY boundingBox =
    let
        { minY, maxY } =
            extrema boundingBox
    in
        minY + 0.5 * (maxY - minY)


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
