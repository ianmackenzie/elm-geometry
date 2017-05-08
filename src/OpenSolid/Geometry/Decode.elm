--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


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
        , circle2d
        , circle3d
        , arc2d
        , arc3d
        , quadraticSpline2d
        , quadraticSpline3d
        , cubicSpline2d
        , cubicSpline3d
        )

{-| JSON decoders for the core OpenSolid types.

@docs vector2d, vector3d, direction2d, direction3d, point2d, point3d
@docs axis2d, axis3d, plane3d, frame2d, frame3d, sketchPlane3d
@docs lineSegment2d, lineSegment3d, triangle2d, triangle3d
@docs boundingBox2d, boundingBox3d
@docs polyline2d, polyline3d, polygon2d
@docs circle2d, circle3d, arc2d, arc3d
@docs quadraticSpline2d, quadraticSpline3d, cubicSpline2d, cubicSpline3d

-}

import Json.Decode as Decode exposing (Decoder)
import OpenSolid.Geometry.Types exposing (..)


{-| Decodes a `Vector2d` from a list of two floating-point components.
-}
vector2d : Decoder Vector2d
vector2d =
    Decode.map2 (\x y -> Vector2d ( x, y ))
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


{-| Decodes a `Vector3d` from a list of three floating-point components.
-}
vector3d : Decoder Vector3d
vector3d =
    Decode.map3 (\x y z -> Vector3d ( x, y, z ))
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)


{-| Decodes a `Direction2d` from a list of two floating-point components.
-}
direction2d : Decoder Direction2d
direction2d =
    Decode.map2 (\x y -> Direction2d ( x, y ))
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


{-| Decodes a `Direction3d` from a list of three floating-point components.
-}
direction3d : Decoder Direction3d
direction3d =
    Decode.map3 (\x y z -> Direction3d ( x, y, z ))
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)


{-| Decodes a `Point2d` from a list of two floating-point coordinates.
-}
point2d : Decoder Point2d
point2d =
    Decode.map2 (\x y -> Point2d ( x, y ))
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


{-| Decodes a `Point3d` from a list of three floating-point coordinates.
-}
point3d : Decoder Point3d
point3d =
    Decode.map3 (\x y z -> Point3d ( x, y, z ))
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)


{-| Decodes an `Axis2d` from an object with `originPoint` and `direction`
fields.
-}
axis2d : Decoder Axis2d
axis2d =
    Decode.map2
        (\originPoint direction ->
            Axis2d { originPoint = originPoint, direction = direction }
        )
        (Decode.field "originPoint" point2d)
        (Decode.field "direction" direction2d)


{-| Decodes an `Axis3d` from an object with `originPoint` and `direction`
fields.
-}
axis3d : Decoder Axis3d
axis3d =
    Decode.map2
        (\originPoint direction ->
            Axis3d { originPoint = originPoint, direction = direction }
        )
        (Decode.field "originPoint" point3d)
        (Decode.field "direction" direction3d)


{-| Decodes a `Plane3d` from an object with `originPoint` and `normalDirection`
fields.
-}
plane3d : Decoder Plane3d
plane3d =
    Decode.map2
        (\originPoint normalDirection ->
            Plane3d
                { originPoint = originPoint
                , normalDirection = normalDirection
                }
        )
        (Decode.field "originPoint" point3d)
        (Decode.field "normalDirection" direction3d)


{-| Decodes a `Frame2d` from an object with `originPoint`, `xDirection` and
`yDirection` fields.
-}
frame2d : Decoder Frame2d
frame2d =
    Decode.map3
        (\originPoint xDirection yDirection ->
            Frame2d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                }
        )
        (Decode.field "originPoint" point2d)
        (Decode.field "xDirection" direction2d)
        (Decode.field "yDirection" direction2d)


{-| Decodes a `Frame3d` from an object with `originPoint`, `xDirection`,
`yDirection` and `zDirection` fields.
-}
frame3d : Decoder Frame3d
frame3d =
    Decode.map4
        (\originPoint xDirection yDirection zDirection ->
            Frame3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                , zDirection = zDirection
                }
        )
        (Decode.field "originPoint" point3d)
        (Decode.field "xDirection" direction3d)
        (Decode.field "yDirection" direction3d)
        (Decode.field "zDirection" direction3d)


{-| Decodes a `SketchPlane3d` from an object with `originPoint`, `xDirection`
and `yDirection` fields.
-}
sketchPlane3d : Decoder SketchPlane3d
sketchPlane3d =
    Decode.map3
        (\originPoint xDirection yDirection ->
            SketchPlane3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                }
        )
        (Decode.field "originPoint" point3d)
        (Decode.field "xDirection" direction3d)
        (Decode.field "yDirection" direction3d)


{-| Decodes a `LineSegment2d` from a list of two endpoints.
-}
lineSegment2d : Decoder LineSegment2d
lineSegment2d =
    Decode.map2 (\v1 v2 -> LineSegment2d ( v1, v2 ))
        (Decode.index 0 point2d)
        (Decode.index 1 point2d)


{-| Decodes a `LineSegment3d` from a list of two endpoints.
-}
lineSegment3d : Decoder LineSegment3d
lineSegment3d =
    Decode.map2 (\v1 v2 -> LineSegment3d ( v1, v2 ))
        (Decode.index 0 point3d)
        (Decode.index 1 point3d)


{-| Decodes a `Triangle2d` from a list of three vertices.
-}
triangle2d : Decoder Triangle2d
triangle2d =
    Decode.map3 (\v1 v2 v3 -> Triangle2d ( v1, v2, v3 ))
        (Decode.index 0 point2d)
        (Decode.index 1 point2d)
        (Decode.index 2 point2d)


{-| Decodes a `Triangle3d` from a list of three vertices.
-}
triangle3d : Decoder Triangle3d
triangle3d =
    Decode.map3 (\v1 v2 v3 -> Triangle3d ( v1, v2, v3 ))
        (Decode.index 0 point3d)
        (Decode.index 1 point3d)
        (Decode.index 2 point3d)


{-| Decodes a `BoundingBox2d` from an object with `minX`, `maxX`, `minY` and
`maxY` fields.
-}
boundingBox2d : Decoder BoundingBox2d
boundingBox2d =
    Decode.map4
        (\minX maxX minY maxY ->
            BoundingBox2d
                { minX = minX
                , maxX = maxX
                , minY = minY
                , maxY = maxY
                }
        )
        (Decode.field "minX" Decode.float)
        (Decode.field "maxX" Decode.float)
        (Decode.field "minY" Decode.float)
        (Decode.field "maxY" Decode.float)


{-| Decodes a `BoundingBox3d` from an object with `minX`, `maxX`, `minY`,
`maxY`, `minZ` and `maxZ` fields.
-}
boundingBox3d : Decoder BoundingBox3d
boundingBox3d =
    Decode.map6
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
        (Decode.field "minX" Decode.float)
        (Decode.field "maxX" Decode.float)
        (Decode.field "minY" Decode.float)
        (Decode.field "maxY" Decode.float)
        (Decode.field "minZ" Decode.float)
        (Decode.field "maxZ" Decode.float)


{-| Decodes a `Polyline2d` from a list of vertices.
-}
polyline2d : Decoder Polyline2d
polyline2d =
    Decode.map Polyline2d (Decode.list point2d)


{-| Decodes a `Polyline3d` from a list of vertices.
-}
polyline3d : Decoder Polyline3d
polyline3d =
    Decode.map Polyline3d (Decode.list point3d)


{-| Decodes a `Polygon2d` from a list of vertices.
-}
polygon2d : Decoder Polygon2d
polygon2d =
    Decode.map Polygon2d (Decode.list point2d)


{-| Decodes a `Circle2d` from an object with `centerPoint` and `radius` fields.
-}
circle2d : Decoder Circle2d
circle2d =
    Decode.map2
        (\centerPoint radius ->
            Circle2d { centerPoint = centerPoint, radius = radius }
        )
        (Decode.field "centerPoint" point2d)
        (Decode.field "radius" Decode.float)


{-| Decodes a `Circle3d` from an object with `centerPoint`, `axialDirection` and
`radius` fields.
-}
circle3d : Decoder Circle3d
circle3d =
    Decode.map3
        (\centerPoint axialDirection radius ->
            Circle3d
                { centerPoint = centerPoint
                , axialDirection = axialDirection
                , radius = radius
                }
        )
        (Decode.field "centerPoint" point3d)
        (Decode.field "axialDirection" direction3d)
        (Decode.field "radius" Decode.float)


{-| Decodes an `Arc2d` from an object with `centerPoint`, `startPoint` and
`sweptAngle` fields.
-}
arc2d : Decoder Arc2d
arc2d =
    Decode.map3
        (\centerPoint startPoint sweptAngle ->
            Arc2d
                { centerPoint = centerPoint
                , startPoint = startPoint
                , sweptAngle = sweptAngle
                }
        )
        (Decode.field "centerPoint" point2d)
        (Decode.field "startPoint" point2d)
        (Decode.field "sweptAngle" Decode.float)


{-| Decodes an `Arc3d` from an object with `axis`, `startPoint` and `sweptAngle`
fields.
-}
arc3d : Decoder Arc3d
arc3d =
    Decode.map3
        (\axis startPoint sweptAngle ->
            Arc3d
                { axis = axis
                , startPoint = startPoint
                , sweptAngle = sweptAngle
                }
        )
        (Decode.field "axis" axis3d)
        (Decode.field "startPoint" point3d)
        (Decode.field "sweptAngle" Decode.float)


{-| Decodes a `QuadraticSpline2d` from a list of three control points.
-}
quadraticSpline2d : Decoder QuadraticSpline2d
quadraticSpline2d =
    Decode.map3 (\p1 p2 p3 -> QuadraticSpline2d ( p1, p2, p3 ))
        (Decode.index 0 point2d)
        (Decode.index 1 point2d)
        (Decode.index 2 point2d)


{-| Decodes a `QuadraticSpline3d` from a list of three control points.
-}
quadraticSpline3d : Decoder QuadraticSpline3d
quadraticSpline3d =
    Decode.map3 (\p1 p2 p3 -> QuadraticSpline3d ( p1, p2, p3 ))
        (Decode.index 0 point3d)
        (Decode.index 1 point3d)
        (Decode.index 2 point3d)


{-| Decodes a `CubicSpline2d` from a list of four control points.
-}
cubicSpline2d : Decoder CubicSpline2d
cubicSpline2d =
    Decode.map4 (\p1 p2 p3 p4 -> CubicSpline2d ( p1, p2, p3, p4 ))
        (Decode.index 0 point2d)
        (Decode.index 1 point2d)
        (Decode.index 2 point2d)
        (Decode.index 3 point2d)


{-| Decodes a `CubicSpline3d` from a list of four control points.
-}
cubicSpline3d : Decoder CubicSpline3d
cubicSpline3d =
    Decode.map4 (\p1 p2 p3 p4 -> CubicSpline3d ( p1, p2, p3, p4 ))
        (Decode.index 0 point3d)
        (Decode.index 1 point3d)
        (Decode.index 2 point3d)
        (Decode.index 3 point3d)
