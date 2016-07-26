{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Test.Producers
    exposing
        ( angle
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
        )

{-| This module contains `Producer` implementations for the core OpenSolid types
(used for constructing claim-based tests).
-}

import Check.Producer as Producer exposing (Producer)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Axis2d as Axis2d
import OpenSolid.Core.Axis3d as Axis3d
import OpenSolid.Core.Plane3d as Plane3d


angle : Producer Float
angle =
    Producer.rangeFloat (-2 * pi) (2 * pi)


scalar : Producer Float
scalar =
    Producer.rangeFloat -10 10


tuple2d : Producer ( Float, Float )
tuple2d =
    Producer.tuple ( scalar, scalar )


tuple3d : Producer ( Float, Float, Float )
tuple3d =
    Producer.tuple3 ( scalar, scalar, scalar )


vector2d : Producer Vector2d
vector2d =
    Producer.convert Vector2d Vector2d.components tuple2d


vector3d : Producer Vector3d
vector3d =
    Producer.convert Vector3d Vector3d.components tuple3d


direction2d : Producer Direction2d
direction2d =
    let
        isValid =
            Vector2d.length >> (\length -> length >= 2 && length <= 5)

        validVector =
            Producer.filter isValid vector2d

        direction vector =
            let
                normalizedVector =
                    Vector2d.times (1 / Vector2d.length vector) vector
            in
                Direction2d (Vector2d.components normalizedVector)
    in
        Producer.map direction validVector


direction3d : Producer Direction3d
direction3d =
    let
        isValid =
            Vector3d.length >> (\length -> length >= 2 && length <= 5)

        validVector =
            Producer.filter isValid vector3d

        direction vector =
            let
                normalizedVector =
                    Vector3d.times (1 / Vector3d.length vector) vector
            in
                Direction3d (Vector3d.components normalizedVector)
    in
        Producer.map direction validVector


point2d : Producer Point2d
point2d =
    Producer.convert Point2d Point2d.coordinates tuple2d


point3d : Producer Point3d
point3d =
    Producer.convert Point3d Point3d.coordinates tuple3d


axis2d : Producer Axis2d
axis2d =
    let
        tupleProducer =
            Producer.tuple ( point2d, direction2d )

        tupleToAxis ( point, direction ) =
            Axis2d { originPoint = point, direction = direction }

        axisToTuple axis =
            ( Axis2d.originPoint axis, Axis2d.direction axis )
    in
        Producer.convert tupleToAxis axisToTuple tupleProducer


axis3d : Producer Axis3d
axis3d =
    let
        tupleProducer =
            Producer.tuple ( point3d, direction3d )

        tupleToAxis ( point, direction ) =
            Axis3d { originPoint = point, direction = direction }

        axisToTuple axis =
            ( Axis3d.originPoint axis, Axis3d.direction axis )
    in
        Producer.convert tupleToAxis axisToTuple tupleProducer


plane3d : Producer Plane3d
plane3d =
    let
        tupleProducer =
            Producer.tuple ( point3d, direction3d )

        tupleToPlane ( point, direction ) =
            Plane3d { originPoint = point, normalDirection = direction }
    in
        Producer.map tupleToPlane tupleProducer


frame2d : Producer Frame2d
frame2d =
    let
        tupleProducer =
            Producer.tuple ( point2d, direction2d )

        tupleToFrame ( point, direction ) =
            Frame2d
                { originPoint = point
                , xDirection = direction
                , yDirection = Direction2d.perpendicularTo direction
                }
    in
        Producer.map tupleToFrame tupleProducer


frame3d : Producer Frame3d
frame3d =
    let
        linearlyIndependentVectors ( _, v1, v2 ) =
            Vector3d.squaredLength (Vector3d.crossProduct v1 v2) > 1

        tupleProducer =
            Producer.filter linearlyIndependentVectors
                (Producer.tuple3 ( point3d, vector3d, vector3d ))

        toDirection vector =
            let
                normalizedVector =
                    Vector3d.times (1 / Vector3d.length vector) vector
            in
                Direction3d (Vector3d.components normalizedVector)

        tupleToFrame ( point, v1, v2 ) =
            let
                xVector =
                    v1

                zVector =
                    Vector3d.crossProduct xVector v2

                yVector =
                    Vector3d.crossProduct zVector xVector
            in
                Frame3d
                    { originPoint = point
                    , xDirection = toDirection xVector
                    , yDirection = toDirection yVector
                    , zDirection = toDirection zVector
                    }
    in
        Producer.map tupleToFrame tupleProducer
