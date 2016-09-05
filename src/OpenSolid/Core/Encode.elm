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
        , sketchPlane3d
        , boundingBox2d
        , boundingBox3d
        )

import Json.Encode as Encode exposing (Value)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.SketchPlane3d as SketchPlane3d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.BoundingBox3d as BoundingBox3d


{-| Encode a Vector2d as an object with 'x' and 'y' fields.
-}
vector2d : Vector2d -> Value
vector2d vector =
    let
        ( x, y ) =
            Vector2d.components vector
    in
        Encode.object
            [ ( "x", Encode.float x )
            , ( "y", Encode.float y )
            ]


{-| Encode a Vector3d as an object with 'x', 'y' and 'z' fields.
-}
vector3d : Vector3d -> Value
vector3d vector =
    let
        ( x, y, z ) =
            Vector3d.components vector
    in
        Encode.object
            [ ( "x", Encode.float x )
            , ( "y", Encode.float y )
            , ( "z", Encode.float z )
            ]


{-| Encode a Direction2d as an object with 'x' and 'y' fields.
-}
direction2d : Direction2d -> Value
direction2d direction =
    let
        ( x, y ) =
            Direction2d.components direction
    in
        Encode.object
            [ ( "x", Encode.float x )
            , ( "y", Encode.float y )
            ]


{-| Encode a Direction3d as an object with 'x', 'y' and 'z' fields.
-}
direction3d : Direction3d -> Value
direction3d direction =
    let
        ( x, y, z ) =
            Direction3d.components direction
    in
        Encode.object
            [ ( "x", Encode.float x )
            , ( "y", Encode.float y )
            , ( "z", Encode.float z )
            ]


{-| Encode a Point2d as an object with 'x' and 'y' fields.
-}
point2d : Point2d -> Value
point2d point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        Encode.object
            [ ( "x", Encode.float x )
            , ( "y", Encode.float y )
            ]


{-| Encode a Point3d as an object with 'x', 'y' and 'z' fields.
-}
point3d : Point3d -> Value
point3d point =
    let
        ( x, y, z ) =
            Point3d.coordinates point
    in
        Encode.object
            [ ( "x", Encode.float x )
            , ( "y", Encode.float y )
            , ( "z", Encode.float z )
            ]


{-| Encode an Axis2d as an object with 'originPoint' and 'direction' fields.
-}
axis2d : Axis2d -> Value
axis2d axis =
    Encode.object
        [ ( "originPoint", point2d (Axis2d.originPoint axis) )
        , ( "direction", direction2d (Axis2d.direction axis) )
        ]


{-| Encode an Axis3d as an object with 'originPoint' and 'direction' fields.
-}
axis3d : Axis3d -> Value
axis3d axis =
    Encode.object
        [ ( "originPoint", point3d (Axis3d.originPoint axis) )
        , ( "direction", direction3d (Axis3d.direction axis) )
        ]


{-| Encode a Plane3d as an object with 'originPoint' and 'normalDirection'
fields.
-}
plane3d : Plane3d -> Value
plane3d plane =
    Encode.object
        [ ( "originPoint", point3d (Plane3d.originPoint plane) )
        , ( "normalDirection", direction3d (Plane3d.normalDirection plane) )
        ]


{-| Encode a Frame2d as an object with 'originPoint', 'xDirection' and
'yDirection' fields.
-}
frame2d : Frame2d -> Value
frame2d frame =
    Encode.object
        [ ( "originPoint", point2d (Frame2d.originPoint frame) )
        , ( "xDirection", direction2d (Frame2d.xDirection frame) )
        , ( "yDirection", direction2d (Frame2d.yDirection frame) )
        ]


{-| Encode a Frame3d as an object with 'originPoint', 'xDirection', 'yDirection'
and 'zDirection' fields.
-}
frame3d : Frame3d -> Value
frame3d frame =
    Encode.object
        [ ( "originPoint", point3d (Frame3d.originPoint frame) )
        , ( "xDirection", direction3d (Frame3d.xDirection frame) )
        , ( "yDirection", direction3d (Frame3d.yDirection frame) )
        , ( "zDirection", direction3d (Frame3d.zDirection frame) )
        ]


{-| Encode a SketchPlane3d as an object with 'originPoint', 'xDirection' and
'yDirection' fields.
-}
sketchPlane3d : SketchPlane3d -> Value
sketchPlane3d sketchPlane =
    Encode.object
        [ ( "originPoint", point3d (SketchPlane3d.originPoint sketchPlane) )
        , ( "xDirection", direction3d (SketchPlane3d.xDirection sketchPlane) )
        , ( "yDirection", direction3d (SketchPlane3d.yDirection sketchPlane) )
        ]


{-| Encode a Float in such a way that infinite and NaN values can be preserved,
by falling back to string constants "Infinity", "-Infinity" and "NaN" for
special values.
-}
extremum : Float -> Value
extremum value =
    if isNaN value then
        Encode.string "NaN"
    else if value == 1 / 0 then
        Encode.string "Infinity"
    else if value == -1 / 0 then
        Encode.string "-Infinity"
    else
        Encode.float value


{-| Encode a BoundingBox2d as an object with 'minX', 'maxX', 'minY' and 'maxY'
fields.
-}
boundingBox2d : BoundingBox2d -> Value
boundingBox2d boundingBox =
    Encode.object
        [ ( "minX", extremum (BoundingBox2d.minX boundingBox) )
        , ( "maxX", extremum (BoundingBox2d.maxX boundingBox) )
        , ( "minY", extremum (BoundingBox2d.minY boundingBox) )
        , ( "maxY", extremum (BoundingBox2d.maxY boundingBox) )
        ]


{-| Encode a BoundingBox3d as an object with 'minX', 'maxX', 'minY', 'maxY',
'minZ' and 'maxZ' fields.
-}
boundingBox3d : BoundingBox3d -> Value
boundingBox3d boundingBox =
    Encode.object
        [ ( "minX", extremum (BoundingBox3d.minX boundingBox) )
        , ( "maxX", extremum (BoundingBox3d.maxX boundingBox) )
        , ( "minY", extremum (BoundingBox3d.minY boundingBox) )
        , ( "maxY", extremum (BoundingBox3d.maxY boundingBox) )
        , ( "minZ", extremum (BoundingBox3d.minZ boundingBox) )
        , ( "maxZ", extremum (BoundingBox3d.maxZ boundingBox) )
        ]
