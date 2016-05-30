{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Encode
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

{-| JSON encoders for the core OpenSolid types.
-}

import Json.Encode exposing (..)
import OpenSolid.Core.Types exposing (..)


vector2d : Vector2d -> Value
vector2d (Vector2d x y) =
    list [ float x, float y ]


vector3d : Vector3d -> Value
vector3d (Vector3d x y z) =
    list [ float x, float y, float z ]


direction2d : Direction2d -> Value
direction2d (Direction2d vector) =
    vector2d vector


direction3d : Direction3d -> Value
direction3d (Direction3d vector) =
    vector3d vector


point2d : Point2d -> Value
point2d (Point2d x y) =
    list [ float x, float y ]


point3d : Point3d -> Value
point3d (Point3d x y z) =
    list [ float x, float y, float z ]


axis2d : Axis2d -> Value
axis2d axis =
    object
        [ ( "originPoint", point2d axis.originPoint )
        , ( "direction", direction2d axis.direction )
        ]


axis3d : Axis3d -> Value
axis3d axis =
    object
        [ ( "originPoint", point3d axis.originPoint )
        , ( "direction", direction3d axis.direction )
        ]


plane3d : Plane3d -> Value
plane3d plane =
    object
        [ ( "originPoint", point3d plane.originPoint )
        , ( "xDirection", direction3d plane.xDirection )
        , ( "yDirection", direction3d plane.yDirection )
        , ( "normalDirection", direction3d plane.normalDirection )
        ]


frame2d : Frame2d -> Value
frame2d frame =
    object
        [ ( "originPoint", point2d frame.originPoint )
        , ( "xDirection", direction2d frame.xDirection )
        , ( "yDirection", direction2d frame.yDirection )
        ]


frame3d : Frame3d -> Value
frame3d frame =
    object
        [ ( "originPoint", point3d frame.originPoint )
        , ( "xDirection", direction3d frame.xDirection )
        , ( "yDirection", direction3d frame.yDirection )
        , ( "zDirection", direction3d frame.zDirection )
        ]
