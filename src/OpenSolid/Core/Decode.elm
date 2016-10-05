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
        , sketchPlane3d
        , lineSegment2d
        , lineSegment3d
        , triangle2d
        , triangle3d
        , boundingBox2d
        , boundingBox3d
        )

import Json.Decode as Decode exposing (Decoder, (:=))
import OpenSolid.Core.Types exposing (..)


{-| Decoder for Vector2d values from objects with 'x' and 'y' fields.
-}
vector2d : Decoder Vector2d
vector2d =
    Decode.object2 (\x y -> Vector2d ( x, y ))
        ("x" := Decode.float)
        ("y" := Decode.float)


{-| Decoder for Vector3d values from objects with 'x', 'y' and 'z' fields.
-}
vector3d : Decoder Vector3d
vector3d =
    Decode.object3 (\x y z -> Vector3d ( x, y, z ))
        ("x" := Decode.float)
        ("y" := Decode.float)
        ("z" := Decode.float)


{-| Decoder for Direction2d values from objects with 'x' and 'y' fields.
-}
direction2d : Decoder Direction2d
direction2d =
    Decode.object2 (\x y -> Direction2d ( x, y ))
        ("x" := Decode.float)
        ("y" := Decode.float)


{-| Decoder for Direction3d values from objects with 'x', 'y' and 'z' fields.
-}
direction3d : Decoder Direction3d
direction3d =
    Decode.object3 (\x y z -> Direction3d ( x, y, z ))
        ("x" := Decode.float)
        ("y" := Decode.float)
        ("z" := Decode.float)


{-| Decoder for Point2d values from objects with 'x' and 'y' fields.
-}
point2d : Decoder Point2d
point2d =
    Decode.object2 (\x y -> Point2d ( x, y ))
        ("x" := Decode.float)
        ("y" := Decode.float)


{-| Decoder for Point3d values from objects with 'x', 'y' and 'z' fields.
-}
point3d : Decoder Point3d
point3d =
    Decode.object3 (\x y z -> Point3d ( x, y, z ))
        ("x" := Decode.float)
        ("y" := Decode.float)
        ("z" := Decode.float)


{-| Decoder for Axis2d values from objects with 'originPoint' and 'direction'
fields.
-}
axis2d : Decoder Axis2d
axis2d =
    Decode.object2
        (\originPoint direction ->
            Axis2d { originPoint = originPoint, direction = direction }
        )
        ("originPoint" := point2d)
        ("direction" := direction2d)


{-| Decoder for Axis3d values from objects with 'originPoint' and 'direction'
fields.
-}
axis3d : Decoder Axis3d
axis3d =
    Decode.object2
        (\originPoint direction ->
            Axis3d { originPoint = originPoint, direction = direction }
        )
        ("originPoint" := point3d)
        ("direction" := direction3d)


{-| Decoder for Plane3d values from objects with 'originPoint' and
'normalDirection' fields.
-}
plane3d : Decoder Plane3d
plane3d =
    Decode.object2
        (\originPoint normalDirection ->
            Plane3d
                { originPoint = originPoint
                , normalDirection = normalDirection
                }
        )
        ("originPoint" := point3d)
        ("normalDirection" := direction3d)


{-| Decoder for Frame2d values from objects with 'originPoint', 'xDirection' and
'yDirection' fields.
-}
frame2d : Decoder Frame2d
frame2d =
    Decode.object3
        (\originPoint xDirection yDirection ->
            Frame2d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                }
        )
        ("originPoint" := point2d)
        ("xDirection" := direction2d)
        ("yDirection" := direction2d)


{-| Decoder for Frame3d values from objects with 'originPoint', 'xDirection',
'yDirection' and 'zDirection' fields.
-}
frame3d : Decoder Frame3d
frame3d =
    Decode.object4
        (\originPoint xDirection yDirection zDirection ->
            Frame3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                , zDirection = zDirection
                }
        )
        ("originPoint" := point3d)
        ("xDirection" := direction3d)
        ("yDirection" := direction3d)
        ("zDirection" := direction3d)


{-| Decoder for SketchPlane3d values from objects with 'originPoint',
'xDirection' and 'yDirection' fields.
-}
sketchPlane3d : Decoder SketchPlane3d
sketchPlane3d =
    Decode.object3
        (\originPoint xDirection yDirection ->
            SketchPlane3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                }
        )
        ("originPoint" := point3d)
        ("xDirection" := direction3d)
        ("yDirection" := direction3d)


lineSegment2d : Decoder LineSegment2d
lineSegment2d =
    Decode.object2
        (\firstVertex secondVertex ->
            LineSegment2d ( firstVertex, secondVertex )
        )
        ("startPoint" := point2d)
        ("endPoint" := point2d)


lineSegment3d : Decoder LineSegment3d
lineSegment3d =
    Decode.object2
        (\firstVertex secondVertex ->
            LineSegment3d ( firstVertex, secondVertex )
        )
        ("startPoint" := point3d)
        ("endPoint" := point3d)


triangle2d : Decoder Triangle2d
triangle2d =
    Decode.object1 Triangle2d
        ("vertices" := Decode.tuple3 (,,) point2d point2d point2d)


triangle3d : Decoder Triangle3d
triangle3d =
    Decode.object1 Triangle3d
        ("vertices" := Decode.tuple3 (,,) point3d point3d point3d)


{-| Decoder for BoundingBox2d values from objects with 'minX', 'maxX', 'minY'
and 'maxY' fields.
-}
boundingBox2d : Decoder BoundingBox2d
boundingBox2d =
    Decode.object4
        (\minX maxX minY maxY ->
            BoundingBox2d
                { minX = minX
                , maxX = maxX
                , minY = minY
                , maxY = maxY
                }
        )
        ("minX" := Decode.float)
        ("maxX" := Decode.float)
        ("minY" := Decode.float)
        ("maxY" := Decode.float)


{-| Decoder for BoundingBox3d values from objects with 'minX', 'maxX', 'minY',
'maxY', 'minZ' and 'maxZ' fields.
-}
boundingBox3d : Decoder BoundingBox3d
boundingBox3d =
    Decode.object6
        (\minX maxX minY maxY minZ maxZ ->
            BoundingBox3d
                { minX = minX
                , maxX = maxX
                , minY = minY
                , maxY = maxY
                , minZ = minZ
                , maxZ = maxZ
                }
        )
        ("minX" := Decode.float)
        ("maxX" := Decode.float)
        ("minY" := Decode.float)
        ("maxY" := Decode.float)
        ("minZ" := Decode.float)
        ("maxZ" := Decode.float)
