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

@docs vector2d, vector3d, direction2d, direction3d, point2d, point3d
@docs axis2d, axis3d, plane3d, frame2d, frame3d
-}

import Json.Encode exposing (..)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Axis2d as Axis2d
import OpenSolid.Core.Axis3d as Axis3d
import OpenSolid.Core.Plane3d as Plane3d
import OpenSolid.Core.Frame2d as Frame2d
import OpenSolid.Core.Frame3d as Frame3d


tuple2 : ( Float, Float ) -> Value
tuple2 ( x, y ) =
    list [ float x, float y ]


tuple3 : ( Float, Float, Float ) -> Value
tuple3 ( x, y, z ) =
    list [ float x, float y, float z ]


{-| Encode a Vector2d as a list of two floats.
-}
vector2d : Vector2d -> Value
vector2d =
    Vector2d.components >> tuple2


{-| Encode a Vector3d as a list of three floats.
-}
vector3d : Vector3d -> Value
vector3d =
    Vector3d.components >> tuple3


{-| Encode a Direction2d as a list of two floats.
-}
direction2d : Direction2d -> Value
direction2d =
    Direction2d.components >> tuple2


{-| Encode a Direction3d as a list of three floats.
-}
direction3d : Direction3d -> Value
direction3d =
    Direction3d.components >> tuple3


{-| Encode a Point2d as a list of two floats.
-}
point2d : Point2d -> Value
point2d =
    Point2d.coordinates >> tuple2


{-| Encode a Point3d as a list of three floats.
-}
point3d : Point3d -> Value
point3d =
    Point3d.coordinates >> tuple3


{-| Encode an Axis2d as an object with 'originPoint' and 'direction' fields.
-}
axis2d : Axis2d -> Value
axis2d axis =
    object
        [ ( "originPoint", point2d (Axis2d.originPoint axis) )
        , ( "direction", direction2d (Axis2d.direction axis) )
        ]


{-| Encode an Axis3d as an object with 'originPoint' and 'direction' fields.
-}
axis3d : Axis3d -> Value
axis3d axis =
    object
        [ ( "originPoint", point3d (Axis3d.originPoint axis) )
        , ( "direction", direction3d (Axis3d.direction axis) )
        ]


{-| Encode a Plane3d as an object with 'originPoint', 'xDirection', 'yDirection'
and 'normalDirection' fields.
-}
plane3d : Plane3d -> Value
plane3d plane =
    object
        [ ( "originPoint", point3d (Plane3d.originPoint plane) )
        , ( "xDirection", direction3d (Plane3d.xDirection plane) )
        , ( "yDirection", direction3d (Plane3d.yDirection plane) )
        , ( "normalDirection", direction3d (Plane3d.normalDirection plane) )
        ]


{-| Encode a Frame2d as an object with 'originPoint', 'xDirection' and
'yDirection' fields.
-}
frame2d : Frame2d -> Value
frame2d frame =
    object
        [ ( "originPoint", point2d (Frame2d.originPoint frame) )
        , ( "xDirection", direction2d (Frame2d.xDirection frame) )
        , ( "yDirection", direction2d (Frame2d.yDirection frame) )
        ]


{-| Encode a Frame3d as an object with 'originPoint', 'xDirection', 'yDirection'
and 'zDirection' fields.
-}
frame3d : Frame3d -> Value
frame3d frame =
    object
        [ ( "originPoint", point3d (Frame3d.originPoint frame) )
        , ( "xDirection", direction3d (Frame3d.xDirection frame) )
        , ( "yDirection", direction3d (Frame3d.yDirection frame) )
        , ( "zDirection", direction3d (Frame3d.zDirection frame) )
        ]
