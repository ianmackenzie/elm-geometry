{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Fuzz
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
        , boundingBox2d
        , nonEmptyBoundingBox2d
        , boundingBox3d
        , nonEmptyBoundingBox3d
        )

import Fuzz exposing (Fuzzer)
import OpenSolid.Core.Types exposing (..)
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
        theta =
            Fuzz.floatRange -pi pi

        t =
            Fuzz.floatRange -1 1

        phi =
            Fuzz.map acos t

        direction ( theta, phi ) =
            Direction3d
                ( sin phi * cos theta
                , sin phi * sin theta
                , cos phi
                )
    in
        Fuzz.map direction (Fuzz.tuple ( theta, phi ))


point2d : Fuzzer Point2d
point2d =
    Fuzz.map Point2d (Fuzz.tuple ( scalar, scalar ))


point3d : Fuzzer Point3d
point3d =
    Fuzz.map Point3d (Fuzz.tuple3 ( scalar, scalar, scalar ))


axis2d : Fuzzer Axis2d
axis2d =
    let
        tuple =
            Fuzz.tuple ( point2d, direction2d )

        tupleToAxis ( originPoint, direction ) =
            Axis2d { originPoint = originPoint, direction = direction }
    in
        Fuzz.map tupleToAxis tuple


axis3d : Fuzzer Axis3d
axis3d =
    let
        tuple =
            Fuzz.tuple ( point3d, direction3d )

        tupleToAxis ( originPoint, direction ) =
            Axis3d { originPoint = originPoint, direction = direction }
    in
        Fuzz.map tupleToAxis tuple


plane3d : Fuzzer Plane3d
plane3d =
    let
        tuple =
            Fuzz.tuple ( point3d, direction3d )

        tupleToPlane ( originPoint, normalDirection ) =
            Plane3d
                { originPoint = originPoint
                , normalDirection = normalDirection
                }
    in
        Fuzz.map tupleToPlane tuple


frame2d : Fuzzer Frame2d
frame2d =
    let
        tuple =
            Fuzz.tuple ( point2d, direction2d )

        tupleToFrame ( originPoint, xDirection ) =
            Frame2d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = Direction2d.perpendicularTo xDirection
                }
    in
        Fuzz.map tupleToFrame tuple


frame3d : Fuzzer Frame3d
frame3d =
    let
        tuple =
            Fuzz.tuple ( point3d, direction3d )

        tupleToFrame ( originPoint, xDirection ) =
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
        Fuzz.map tupleToFrame tuple


sketchPlane3d : Fuzzer SketchPlane3d
sketchPlane3d =
    let
        tuple =
            Fuzz.tuple ( point3d, direction3d )

        tupleToFrame ( originPoint, xDirection ) =
            SketchPlane3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = Direction3d.perpendicularTo xDirection
                }
    in
        Fuzz.map tupleToFrame tuple


interval : Fuzzer ( Float, Float )
interval =
    let
        tuple =
            Fuzz.tuple ( scalar, scalar )

        ordered ( firstValue, secondValue ) =
            if firstValue <= secondValue then
                ( firstValue, secondValue )
            else
                ( secondValue, firstValue )
    in
        Fuzz.map ordered tuple


boundingBox2d : Fuzzer BoundingBox2d
boundingBox2d =
    let
        emptyBoundingBox2d =
            Fuzz.map (always BoundingBox2d.empty) Fuzz.unit
    in
        Fuzz.frequencyOrCrash
            [ ( 1, nonEmptyBoundingBox2d )
            , ( 1, emptyBoundingBox2d )
            ]


nonEmptyBoundingBox2d =
    let
        intervals =
            Fuzz.tuple ( interval, interval )

        intervalsToBoundingBox ( ( minX, maxX ), ( minY, maxY ) ) =
            BoundingBox2d
                { minX = minX
                , maxX = maxX
                , minY = minY
                , maxY = maxY
                }
    in
        Fuzz.map intervalsToBoundingBox intervals


boundingBox3d : Fuzzer BoundingBox3d
boundingBox3d =
    let
        emptyBoundingBox3d =
            Fuzz.map (always BoundingBox3d.empty) Fuzz.unit
    in
        Fuzz.frequencyOrCrash
            [ ( 1, nonEmptyBoundingBox3d )
            , ( 1, emptyBoundingBox3d )
            ]


nonEmptyBoundingBox3d : Fuzzer BoundingBox3d
nonEmptyBoundingBox3d =
    let
        intervals =
            Fuzz.tuple3 ( interval, interval, interval )

        intervalsToBoundingBox ( xInterval, yInterval, zInterval ) =
            let
                ( minX, maxX ) =
                    xInterval

                ( minY, maxY ) =
                    yInterval

                ( minZ, maxZ ) =
                    zInterval
            in
                BoundingBox3d
                    { minX = minX
                    , maxX = maxX
                    , minY = minY
                    , maxY = maxY
                    , minZ = minZ
                    , maxZ = maxZ
                    }
    in
        Fuzz.map intervalsToBoundingBox intervals
