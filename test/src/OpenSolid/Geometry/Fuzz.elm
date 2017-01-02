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


module OpenSolid.Geometry.Fuzz
    exposing
        ( scalar
        , vector2d
        , vector3d
        , direction2d
        , direction3d
        , point2d
        , point3d
        , axis2d
        , axis3d
        , plane3d
        , frame2d
        , frame3d
        , sketchPlane3d
        , lineSegment2d
        , lineSegment3d
        , triangle2d
        , triangle3d
        , boundingBox2d
        , boundingBox3d
        , polyline2d
        , polyline3d
        , polygon2d
        , circle2d
        )

import Fuzz exposing (Fuzzer)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.BoundingBox3d as BoundingBox3d


scalar : Fuzzer Float
scalar =
    Fuzz.floatRange -10 10


vector2d : Fuzzer Vector2d
vector2d =
    Fuzz.map Vector2d (Fuzz.tuple ( scalar, scalar ))


vector3d : Fuzzer Vector3d
vector3d =
    Fuzz.map Vector3d (Fuzz.tuple3 ( scalar, scalar, scalar ))


direction2d : Fuzzer Direction2d
direction2d =
    Fuzz.map Direction2d.fromAngle (Fuzz.floatRange -pi pi)


direction3d : Fuzzer Direction3d
direction3d =
    let
        phi =
            Fuzz.map acos (Fuzz.floatRange -1 1)

        theta =
            Fuzz.floatRange -pi pi

        direction phi theta =
            let
                r =
                    sin phi

                x =
                    r * cos theta

                y =
                    r * sin theta

                z =
                    cos phi
            in
                Direction3d ( x, y, z )
    in
        Fuzz.map2 direction phi theta


point2d : Fuzzer Point2d
point2d =
    Fuzz.map Point2d (Fuzz.tuple ( scalar, scalar ))


point3d : Fuzzer Point3d
point3d =
    Fuzz.map Point3d (Fuzz.tuple3 ( scalar, scalar, scalar ))


axis2d : Fuzzer Axis2d
axis2d =
    let
        axis originPoint direction =
            Axis2d { originPoint = originPoint, direction = direction }
    in
        Fuzz.map2 axis point2d direction2d


axis3d : Fuzzer Axis3d
axis3d =
    let
        axis originPoint direction =
            Axis3d { originPoint = originPoint, direction = direction }
    in
        Fuzz.map2 axis point3d direction3d


plane3d : Fuzzer Plane3d
plane3d =
    let
        plane originPoint normalDirection =
            Plane3d
                { originPoint = originPoint
                , normalDirection = normalDirection
                }
    in
        Fuzz.map2 plane point3d direction3d


frame2d : Fuzzer Frame2d
frame2d =
    let
        frame originPoint xDirection =
            Frame2d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = Direction2d.perpendicularTo xDirection
                }
    in
        Fuzz.map2 frame point2d direction2d


frame3d : Fuzzer Frame3d
frame3d =
    let
        frame originPoint xDirection =
            let
                yDirection =
                    Direction3d.perpendicularTo xDirection

                zDirectionVector =
                    Direction3d.crossProduct xDirection yDirection

                zDirection =
                    Direction3d (Vector3d.components zDirectionVector)
            in
                Frame3d
                    { originPoint = originPoint
                    , xDirection = xDirection
                    , yDirection = yDirection
                    , zDirection = zDirection
                    }
    in
        Fuzz.map2 frame point3d direction3d


sketchPlane3d : Fuzzer SketchPlane3d
sketchPlane3d =
    let
        sketchPlane originPoint xDirection =
            SketchPlane3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = Direction3d.perpendicularTo xDirection
                }
    in
        Fuzz.map2 sketchPlane point3d direction3d


lineSegment2d : Fuzzer LineSegment2d
lineSegment2d =
    Fuzz.map LineSegment2d (Fuzz.tuple ( point2d, point2d ))


lineSegment3d : Fuzzer LineSegment3d
lineSegment3d =
    Fuzz.map LineSegment3d (Fuzz.tuple ( point3d, point3d ))


triangle2d : Fuzzer Triangle2d
triangle2d =
    Fuzz.map Triangle2d (Fuzz.tuple3 ( point2d, point2d, point2d ))


triangle3d : Fuzzer Triangle3d
triangle3d =
    Fuzz.map Triangle3d (Fuzz.tuple3 ( point3d, point3d, point3d ))


interval : Fuzzer ( Float, Float )
interval =
    let
        ordered firstValue secondValue =
            if firstValue <= secondValue then
                ( firstValue, secondValue )
            else
                ( secondValue, firstValue )
    in
        Fuzz.map2 ordered scalar scalar


boundingBox2d : Fuzzer BoundingBox2d
boundingBox2d =
    let
        boundingBox ( minX, maxX ) ( minY, maxY ) =
            BoundingBox2d
                { minX = minX
                , maxX = maxX
                , minY = minY
                , maxY = maxY
                }
    in
        Fuzz.map2 boundingBox interval interval


boundingBox3d : Fuzzer BoundingBox3d
boundingBox3d =
    let
        boundingBox ( minX, maxX ) ( minY, maxY ) ( minZ, maxZ ) =
            BoundingBox3d
                { minX = minX
                , maxX = maxX
                , minY = minY
                , maxY = maxY
                , minZ = minZ
                , maxZ = maxZ
                }
    in
        Fuzz.map3 boundingBox interval interval interval


polyline2d : Fuzzer Polyline2d
polyline2d =
    Fuzz.map Polyline2d (Fuzz.list point2d)


polyline3d : Fuzzer Polyline3d
polyline3d =
    Fuzz.map Polyline3d (Fuzz.list point3d)


polygon2d : Fuzzer Polygon2d
polygon2d =
    Fuzz.map Polygon2d (Fuzz.list point2d)


circle2d : Fuzzer Circle2d
circle2d =
    let
        circle centerPoint radius =
            Circle2d { centerPoint = centerPoint, radius = radius }
    in
        Fuzz.map2 circle point2d scalar
