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

@docs vector2d, vector3d, direction2d, direction3d, point2d, point3d
@docs axis2d, axis3d, plane3d, frame2d, frame3d
-}

import Json.Decode exposing (..)
import OpenSolid.Core.Types exposing (..)


{-| Decode a Vector2d from a list of two floats.
-}
vector2d : Decoder Vector2d
vector2d =
    map Vector2d (tuple2 (,) float float)


{-| Decode a Vector3d from a list of three floats.
-}
vector3d : Decoder Vector3d
vector3d =
    map Vector3d (tuple3 (,,) float float float)


{-| Decode a Direction2d from a list of two floats.
-}
direction2d : Decoder Direction2d
direction2d =
    map Direction2d (tuple2 (,) float float)


{-| Decode a Direction3d from a list of three floats.
-}
direction3d : Decoder Direction3d
direction3d =
    map Direction3d (tuple3 (,,) float float float)


{-| Decode a Point2d from a list of two floats.
-}
point2d : Decoder Point2d
point2d =
    map Point2d (tuple2 (,) float float)


{-| Decode a Point3d from a list of three floats.
-}
point3d : Decoder Point3d
point3d =
    map Point3d (tuple3 (,,) float float float)


{-| Decode an Axis2d from an object with 'originPoint' and 'direction' fields.
-}
axis2d : Decoder Axis2d
axis2d =
    let
        constructor originPoint direction =
            Axis2d { originPoint = originPoint, direction = direction }
    in
        object2 constructor
            ("originPoint" := point2d)
            ("direction" := direction2d)


{-| Decode an Axis3d from an object with 'originPoint' and 'direction' fields.
-}
axis3d : Decoder Axis3d
axis3d =
    let
        constructor originPoint direction =
            Axis3d { originPoint = originPoint, direction = direction }
    in
        object2 constructor
            ("originPoint" := point3d)
            ("direction" := direction3d)


{-| Decode a Plane3d from an object with 'originPoint', 'xDirection',
'yDirection' and 'normalDirection' fields.
-}
plane3d : Decoder Plane3d
plane3d =
    let
        constructor originPoint xDirection yDirection normalDirection =
            Plane3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                , normalDirection = normalDirection
                }
    in
        object4 constructor
            ("originPoint" := point3d)
            ("xDirection" := direction3d)
            ("yDirection" := direction3d)
            ("normalDirection" := direction3d)


{-| Decode a Frame2d from an object with 'originPoint', 'xDirection', and
'yDirection' fields.
-}
frame2d : Decoder Frame2d
frame2d =
    let
        constructor originPoint xDirection yDirection =
            Frame2d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                }
    in
        object3 constructor
            ("originPoint" := point2d)
            ("xDirection" := direction2d)
            ("yDirection" := direction2d)


{-| Decode a Frame3d from an object with 'originPoint', 'xDirection',
'yDirection' and 'zDirection' fields.
-}
frame3d : Decoder Frame3d
frame3d =
    let
        constructor originPoint xDirection yDirection zDirection =
            Frame3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                , zDirection = zDirection
                }
    in
        object4 constructor
            ("originPoint" := point3d)
            ("xDirection" := direction3d)
            ("yDirection" := direction3d)
            ("zDirection" := direction3d)
