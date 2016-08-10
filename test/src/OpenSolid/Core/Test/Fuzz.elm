{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Test.Fuzz
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
        , planarFrame3d
        )

{-| This module contains `Fuzzer` implementations for the core OpenSolid types.
-}

import Fuzz exposing (Fuzzer)
import Random.Pcg as Random
import Shrink
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Axis2d as Axis2d
import OpenSolid.Core.Axis3d as Axis3d
import OpenSolid.Core.Plane3d as Plane3d


scalar : Fuzzer Float
scalar =
    Fuzz.floatRange -10 10


tuple2d : Fuzzer ( Float, Float )
tuple2d =
    Fuzz.tuple ( Fuzz.float, Fuzz.float )


tuple3d : Fuzzer ( Float, Float, Float )
tuple3d =
    Fuzz.tuple3 ( Fuzz.float, Fuzz.float, Fuzz.float )


vector2d : Fuzzer Vector2d
vector2d =
    Fuzz.map Vector2d tuple2d


vector3d : Fuzzer Vector3d
vector3d =
    Fuzz.map Vector3d tuple3d


direction2d : Fuzzer Direction2d
direction2d =
    let
        componentGenerator =
            Random.float -2 2

        vectorGenerator =
            Random.map2 (\x y -> Vector2d ( x, y ))
                componentGenerator
                componentGenerator

        isValid vector =
            let
                length =
                    Vector2d.length vector
            in
                length >= 0.5 && length <= 2

        validVectorGenerator =
            Random.filter isValid vectorGenerator

        directionOf validVector =
            let
                normalizedVector =
                    Vector2d.times (1 / Vector2d.length validVector) validVector
            in
                Direction2d (Vector2d.components normalizedVector)

        directionGenerator =
            Random.map directionOf validVectorGenerator
    in
        Fuzz.custom directionGenerator Shrink.noShrink


direction3d : Fuzzer Direction3d
direction3d =
    let
        componentGenerator =
            Random.float -2 2

        vectorGenerator =
            Random.map3 (\x y z -> Vector3d ( x, y, z ))
                componentGenerator
                componentGenerator
                componentGenerator

        isValid vector =
            let
                length =
                    Vector3d.length vector
            in
                length >= 0.5 && length <= 2

        validVectorGenerator =
            Random.filter isValid vectorGenerator

        directionOf validVector =
            let
                normalizedVector =
                    Vector3d.times (1 / Vector3d.length validVector) validVector
            in
                Direction3d (Vector3d.components normalizedVector)

        directionGenerator =
            Random.map directionOf validVectorGenerator
    in
        Fuzz.custom directionGenerator Shrink.noShrink


point2d : Fuzzer Point2d
point2d =
    Fuzz.map Point2d tuple2d


point3d : Fuzzer Point3d
point3d =
    Fuzz.map Point3d tuple3d


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


planarFrame3d : Fuzzer PlanarFrame3d
planarFrame3d =
    let
        tuple =
            Fuzz.tuple ( point3d, direction3d )

        tupleToFrame ( originPoint, xDirection ) =
            PlanarFrame3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = Direction3d.perpendicularTo xDirection
                }
    in
        Fuzz.map tupleToFrame tuple
