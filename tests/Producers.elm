{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Producers
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
        )

{-| This module contains `Producer` implementations for the core OpenSolid types
(used for constructing claim-based tests).
-}

import Check.Producer as Producer exposing (Producer)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d


angle : Producer Float
angle =
    Producer.rangeFloat (-2 * pi) (2 * pi)


component : Producer Float
component =
    Producer.rangeFloat -10 10


components2d : Producer ( Float, Float )
components2d =
    Producer.tuple ( component, component )


components3d : Producer ( Float, Float, Float )
components3d =
    Producer.tuple3 ( component, component, component )


vector2d : Producer Vector2d
vector2d =
    Producer.convert Vector2d.fromComponents Vector2d.components components2d


vector3d : Producer Vector3d
vector3d =
    Producer.convert Vector3d.fromComponents Vector3d.components components3d


direction2d : Producer Direction2d
direction2d =
    let
        isValid =
            Vector2d.length >> (\length -> length >= 2 && length <= 5)

        validVector =
            Producer.filter isValid vector2d

        direction vector =
            Direction2d (Vector2d.times (1 / Vector2d.length vector) vector)
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
            Direction3d (Vector3d.times (1 / Vector3d.length vector) vector)
    in
        Producer.map direction validVector


point2d : Producer Point2d
point2d =
    Producer.convert Point2d.fromComponents Point2d.components components2d


point3d : Producer Point3d
point3d =
    Producer.convert Point3d.fromComponents Point3d.components components3d


axis2d : Producer Axis2d
axis2d =
    let
        tupleProducer =
            Producer.tuple ( point2d, direction2d )

        tupleToAxis ( point, direction ) =
            Axis2d point direction

        axisToTuple axis =
            ( axis.originPoint, axis.direction )
    in
        Producer.convert tupleToAxis axisToTuple tupleProducer


axis3d : Producer Axis3d
axis3d =
    let
        tupleProducer =
            Producer.tuple ( point3d, direction3d )

        tupleToAxis ( point, direction ) =
            Axis3d point direction

        axisToTuple axis =
            ( axis.originPoint, axis.direction )
    in
        Producer.convert tupleToAxis axisToTuple tupleProducer
