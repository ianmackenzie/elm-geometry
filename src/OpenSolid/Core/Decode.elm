{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Decode
    exposing
        ( vector2d
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

{-| JSON decoders for the core OpenSolid types.
-}

import Json.Decode exposing (..)
import OpenSolid.Core.Types exposing (..)


vector2d : Decoder Vector2d
vector2d =
    tuple2 Vector2d float float


vector3d : Decoder Vector3d
vector3d =
    tuple3 Vector3d float float float


direction2d : Decoder Direction2d
direction2d =
    tuple2 (Vector2d >> Direction2d) float float


direction3d : Decoder Direction3d
direction3d =
    tuple3 (Vector3d >> Direction3d) float float float


point2d : Decoder Point2d
point2d =
    tuple2 Point2d float float


point3d : Decoder Point3d
point3d =
    tuple3 Point3d float float float


axis2d : Decoder Axis2d
axis2d =
    object2 Axis2d ("originPoint" := point2d) ("direction" := direction2d)


axis3d : Decoder Axis3d
axis3d =
    object2 Axis3d ("originPoint" := point3d) ("direction" := direction3d)


plane3d : Decoder Plane3d
plane3d =
    object4 Plane3d
        ("originPoint" := point3d)
        ("xDirection" := direction3d)
        ("yDirection" := direction3d)
        ("normalDirection" := direction3d)


frame2d : Decoder Frame2d
frame2d =
    object3 Frame2d
        ("originPoint" := point2d)
        ("xDirection" := direction2d)
        ("yDirection" := direction2d)


frame3d : Decoder Frame3d
frame3d =
    object3 Frame3d
        ("originPoint" := point3d)
        ("xDirection" := direction3d)
        ("yDirection" := direction3d)
        ("zDirection" := direction3d)
