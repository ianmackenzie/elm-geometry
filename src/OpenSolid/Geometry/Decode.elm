{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Geometry.Decode
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
        , polyline2d
        , polyline3d
        , polygon2d
        )

{-| JSON decoders for the core OpenSolid types.

@docs vector2d, vector3d, direction2d, direction3d, point2d, point3d
@docs axis2d, axis3d, plane3d, frame2d, frame3d, sketchPlane3d
@docs lineSegment2d, lineSegment3d, triangle2d, triangle3d
@docs boundingBox2d, boundingBox3d
@docs polyline2d, polyline3d, polygon2d
-}

import Json.Decode as Decode exposing (Decoder, (:=))
import OpenSolid.Geometry.Types exposing (..)


{-| Decodes a Vector2d from a list of two floating-point components.
-}
vector2d : Decoder Vector2d
vector2d =
    Decode.tuple2 (\x y -> Vector2d ( x, y )) Decode.float Decode.float


{-| Decodes a Vector3d from a list of three floating-point components.
-}
vector3d : Decoder Vector3d
vector3d =
    Decode.tuple3 (\x y z -> Vector3d ( x, y, z ))
        Decode.float
        Decode.float
        Decode.float


{-| Decodes a Direction2d from a list of two floating-point components.
-}
direction2d : Decoder Direction2d
direction2d =
    Decode.tuple2 (\x y -> Direction2d ( x, y )) Decode.float Decode.float


{-| Decodes a Direction3d from a list of three floating-point components.
-}
direction3d : Decoder Direction3d
direction3d =
    Decode.tuple3 (\x y z -> Direction3d ( x, y, z ))
        Decode.float
        Decode.float
        Decode.float


{-| Decodes a Point2d from a list of two floating-point coordinates.
-}
point2d : Decoder Point2d
point2d =
    Decode.tuple2 (\x y -> Point2d ( x, y )) Decode.float Decode.float


{-| Decodes a Point3d from a list of three floating-point coordinates.
-}
point3d : Decoder Point3d
point3d =
    Decode.tuple3 (\x y z -> Point3d ( x, y, z ))
        Decode.float
        Decode.float
        Decode.float


{-| Decodes an Axis2d from an object with 'originPoint' and 'direction' fields.
-}
axis2d : Decoder Axis2d
axis2d =
    Decode.object2
        (\originPoint direction ->
            Axis2d { originPoint = originPoint, direction = direction }
        )
        ("originPoint" := point2d)
        ("direction" := direction2d)


{-| Decodes an Axis3d from an object with 'originPoint' and 'direction' fields.
-}
axis3d : Decoder Axis3d
axis3d =
    Decode.object2
        (\originPoint direction ->
            Axis3d { originPoint = originPoint, direction = direction }
        )
        ("originPoint" := point3d)
        ("direction" := direction3d)


{-| Decodes a Plane3d from an object with 'originPoint' and 'normalDirection'
fields.
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


{-| Decodes a Frame2d from an object with 'originPoint', 'xDirection' and
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


{-| Decodes a Frame3d from an object with 'originPoint', 'xDirection',
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


{-| Decodes a SketchPlane3d from an object with 'originPoint', 'xDirection' and
'yDirection' fields.
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


{-| Decodes a LineSegment2d from a list of two endpoints.
-}
lineSegment2d : Decoder LineSegment2d
lineSegment2d =
    Decode.tuple2 (\v1 v2 -> LineSegment2d ( v1, v2 )) point2d point2d


{-| Decodes a LineSegment3d from a list of two endpoints.
-}
lineSegment3d : Decoder LineSegment3d
lineSegment3d =
    Decode.tuple2 (\v1 v2 -> LineSegment3d ( v1, v2 )) point3d point3d


{-| Decodes a Triangle2d from a list of three vertices.
-}
triangle2d : Decoder Triangle2d
triangle2d =
    Decode.tuple3 (\v1 v2 v3 -> Triangle2d ( v1, v2, v3 ))
        point2d
        point2d
        point2d


{-| Decodes a Triangle3d from a list of three vertices.
-}
triangle3d : Decoder Triangle3d
triangle3d =
    Decode.tuple3 (\v1 v2 v3 -> Triangle3d ( v1, v2, v3 ))
        point3d
        point3d
        point3d


{-| Decodes a BoundingBox2d from an object with 'minX', 'maxX', 'minY' and
'maxY' fields.
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


{-| Decodes a BoundingBox3d from an object with 'minX', 'maxX', 'minY', 'maxY',
'minZ' and 'maxZ' fields.
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


{-| Decodes a Polyline2d from a list of vertices.
-}
polyline2d : Decoder Polyline2d
polyline2d =
    Decode.map Polyline2d (Decode.list point2d)


{-| Decodes a Polyline3d from a list of vertices.
-}
polyline3d : Decoder Polyline3d
polyline3d =
    Decode.map Polyline3d (Decode.list point3d)


{-| Decodes a Polygon2d from a list of vertices.
-}
polygon2d : Decoder Polygon2d
polygon2d =
    Decode.map Polygon2d (Decode.list point2d)
