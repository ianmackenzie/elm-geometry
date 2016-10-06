{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.BoundingBox3d
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
        , minZ
        , maxZ
        , midX
        , midY
        , midZ
        , point
        , midpoint
        , contains
        , overlaps
        , isContainedIn
        , hull
        , intersection
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point3d as Point3d


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


containing2 : ( Point3d, Point3d ) -> BoundingBox3d
containing2 points =
    let
        ( firstPoint, secondPoint ) =
            points

        ( x1, y1, z1 ) =
            Point3d.coordinates firstPoint

        ( x2, y2, z2 ) =
            Point3d.coordinates secondPoint
    in
        BoundingBox3d
            { minX = min x1 x2
            , maxX = max x1 x2
            , minY = min y1 y2
            , maxY = max y1 y2
            , minZ = min z1 z2
            , maxZ = max z1 z2
            }


containing3 : ( Point3d, Point3d, Point3d ) -> BoundingBox3d
containing3 points =
    let
        ( firstPoint, secondPoint, thirdPoint ) =
            points

        ( x1, y1, z1 ) =
            Point3d.coordinates firstPoint

        ( x2, y2, z2 ) =
            Point3d.coordinates secondPoint

        ( x3, y3, z3 ) =
            Point3d.coordinates thirdPoint
    in
        BoundingBox3d
            { minX = min x1 (min x2 x3)
            , maxX = max x1 (max x2 x3)
            , minY = min y1 (min y2 y3)
            , maxY = max y1 (max y2 y3)
            , minZ = min z1 (min z2 z3)
            , maxZ = max z1 (max z2 z3)
            }


containing : List Point3d -> Maybe BoundingBox3d
containing points =
    case points of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl hull (singleton first) (List.map singleton rest))


extrema :
    BoundingBox3d
    -> { minX : Float
       , maxX : Float
       , minY : Float
       , maxY : Float
       , minZ : Float
       , maxZ : Float
       }
extrema (BoundingBox3d extrema') =
    extrema'


minX : BoundingBox3d -> Float
minX =
    extrema >> .minX


maxX : BoundingBox3d -> Float
maxX =
    extrema >> .maxX


minY : BoundingBox3d -> Float
minY =
    extrema >> .minY


maxY : BoundingBox3d -> Float
maxY =
    extrema >> .maxY


minZ : BoundingBox3d -> Float
minZ =
    extrema >> .minZ


maxZ : BoundingBox3d -> Float
maxZ =
    extrema >> .maxZ


midX : BoundingBox3d -> Float
midX boundingBox =
    let
        { minX, maxX } =
            extrema boundingBox
    in
        minX + 0.5 * (maxX - minX)


midY : BoundingBox3d -> Float
midY boundingBox =
    let
        { minY, maxY } =
            extrema boundingBox
    in
        minY + 0.5 * (maxY - minY)


midZ : BoundingBox3d -> Float
midZ boundingBox =
    let
        { minZ, maxZ } =
            extrema boundingBox
    in
        minZ + 0.5 * (maxZ - minZ)


point :
    ( BoundingBox3d -> Float, BoundingBox3d -> Float, BoundingBox3d -> Float )
    -> BoundingBox3d
    -> Point3d
point ( xFunction, yFunction, zFunction ) boundingBox =
    Point3d
        ( xFunction boundingBox
        , yFunction boundingBox
        , zFunction boundingBox
        )


midpoint : BoundingBox3d -> Point3d
midpoint =
    point ( midX, midY, midZ )


contains : Point3d -> BoundingBox3d -> Bool
contains point boundingBox =
    let
        ( x, y, z ) =
            Point3d.coordinates point
    in
        (minX boundingBox <= x && x <= maxX boundingBox)
            && (minY boundingBox <= y && y <= maxY boundingBox)
            && (minZ boundingBox <= z && z <= maxZ boundingBox)


overlaps : BoundingBox3d -> BoundingBox3d -> Bool
overlaps other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)
        && (minZ boundingBox <= maxZ other)
        && (maxZ boundingBox >= minZ other)


isContainedIn : BoundingBox3d -> BoundingBox3d -> Bool
isContainedIn other boundingBox =
    (minX other <= minX boundingBox && maxX boundingBox <= maxX other)
        && (minY other <= minY boundingBox && maxY boundingBox <= maxY other)
        && (minZ other <= minZ boundingBox && maxZ boundingBox <= maxZ other)


hull : BoundingBox3d -> BoundingBox3d -> BoundingBox3d
hull other boundingBox =
    BoundingBox3d
        { minX = min (minX boundingBox) (minX other)
        , maxX = max (maxX boundingBox) (maxX other)
        , minY = min (minY boundingBox) (minY other)
        , maxY = max (maxY boundingBox) (maxY other)
        , minZ = min (minZ boundingBox) (minZ other)
        , maxZ = max (maxZ boundingBox) (maxZ other)
        }


intersection : BoundingBox3d -> BoundingBox3d -> Maybe BoundingBox3d
intersection other boundingBox =
    if overlaps other boundingBox then
        Just
            (BoundingBox3d
                { minX = max (minX boundingBox) (minX other)
                , maxX = min (maxX boundingBox) (maxX other)
                , minY = max (minY boundingBox) (minY other)
                , maxY = min (maxY boundingBox) (maxY other)
                , minZ = max (minZ boundingBox) (minZ other)
                , maxZ = min (maxZ boundingBox) (maxZ other)
                }
            )
    else
        Nothing
