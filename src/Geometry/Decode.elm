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


module Geometry.Decode
    exposing
        ( axis2d
        , axis3d
        , boundingBox2d
        , boundingBox3d
        , circle2d
        , circle3d
        , cubicSpline2d
        , cubicSpline3d
        , direction2d
        , direction3d
        , ellipse2d
        , frame2d
        , frame3d
        , lineSegment2d
        , lineSegment3d
        , plane3d
        , point2d
        , point3d
        , polygon2d
        , polyline2d
        , polyline3d
        , quadraticSpline2d
        , quadraticSpline3d
        , sketchPlane3d
        , sphere3d
        , triangle2d
        , triangle3d
        , vector2d
        , vector3d
        )

{-| JSON decoders for the core OpenSolid types.

@docs vector2d, vector3d, direction2d, direction3d, point2d, point3d
@docs axis2d, axis3d, plane3d, frame2d, frame3d, sketchPlane3d
@docs lineSegment2d, lineSegment3d, triangle2d, triangle3d
@docs boundingBox2d, boundingBox3d
@docs polyline2d, polyline3d, polygon2d
@docs circle2d, circle3d, ellipse2d, sphere3d
@docs quadraticSpline2d, quadraticSpline3d, cubicSpline2d, cubicSpline3d

-}

import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Json.Decode as Decode exposing (Decoder)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| Decodes a `Vector2d` from an array of two floating-point components.
-}
vector2d : Decoder Vector2d
vector2d =
    let
        toVector x y =
            Vector2d.fromComponents ( x, y )
    in
    Decode.map2 toVector
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


{-| Decodes a `Vector3d` from an array of three floating-point components.
-}
vector3d : Decoder Vector3d
vector3d =
    let
        toVector x y z =
            Vector3d.fromComponents ( x, y, z )
    in
    Decode.map3 toVector
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)


{-| Decodes a `Direction2d` from an array of two floating-point components.
-}
direction2d : Decoder Direction2d
direction2d =
    let
        toDirection x y =
            Direction2d.unsafe ( x, y )
    in
    Decode.map2 toDirection
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


{-| Decodes a `Direction3d` from an array of three floating-point components.
-}
direction3d : Decoder Direction3d
direction3d =
    let
        toDirection x y z =
            Direction3d.unsafe ( x, y, z )
    in
    Decode.map3 toDirection
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)


{-| Decodes a `Point2d` from an array of two floating-point coordinates.
-}
point2d : Decoder Point2d
point2d =
    let
        toPoint x y =
            Point2d.fromCoordinates ( x, y )
    in
    Decode.map2 toPoint
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


{-| Decodes a `Point3d` from an array of three floating-point coordinates.
-}
point3d : Decoder Point3d
point3d =
    let
        toPoint x y z =
            Point3d.fromCoordinates ( x, y, z )
    in
    Decode.map3 toPoint
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)


{-| Decodes an `Axis2d` from an object with `originPoint` and `direction`
fields.
-}
axis2d : Decoder Axis2d
axis2d =
    Decode.map2 Axis2d.through
        (Decode.field "originPoint" point2d)
        (Decode.field "direction" direction2d)


{-| Decodes an `Axis3d` from an object with `originPoint` and `direction`
fields.
-}
axis3d : Decoder Axis3d
axis3d =
    Decode.map2 Axis3d.through
        (Decode.field "originPoint" point3d)
        (Decode.field "direction" direction3d)


{-| Decodes a `Plane3d` from an object with `originPoint` and `normalDirection`
fields.
-}
plane3d : Decoder Plane3d
plane3d =
    Decode.map2 Plane3d.through
        (Decode.field "originPoint" point3d)
        (Decode.field "normalDirection" direction3d)


{-| Decodes a `Frame2d` from an object with `originPoint`, `xDirection` and
`yDirection` fields.
-}
frame2d : Decoder Frame2d
frame2d =
    Decode.map3
        (\originPoint xDirection yDirection ->
            Types.Frame2d
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
            Types.Frame3d
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
            Types.SketchPlane3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = yDirection
                }
        )
        (Decode.field "originPoint" point3d)
        (Decode.field "xDirection" direction3d)
        (Decode.field "yDirection" direction3d)


{-| Decodes a `LineSegment2d` from an array of two endpoints.
-}
lineSegment2d : Decoder LineSegment2d
lineSegment2d =
    Decode.map2 LineSegment2d.from
        (Decode.index 0 point2d)
        (Decode.index 1 point2d)


{-| Decodes a `LineSegment3d` from an array of two endpoints.
-}
lineSegment3d : Decoder LineSegment3d
lineSegment3d =
    Decode.map2 LineSegment3d.from
        (Decode.index 0 point3d)
        (Decode.index 1 point3d)


{-| Decodes a `Triangle2d` from an array of three vertices.
-}
triangle2d : Decoder Triangle2d
triangle2d =
    Decode.map3 (\v1 v2 v3 -> Triangle2d.fromVertices ( v1, v2, v3 ))
        (Decode.index 0 point2d)
        (Decode.index 1 point2d)
        (Decode.index 2 point2d)


{-| Decodes a `Triangle3d` from an array of three vertices.
-}
triangle3d : Decoder Triangle3d
triangle3d =
    Decode.map3 (\v1 v2 v3 -> Triangle3d.fromVertices ( v1, v2, v3 ))
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
            BoundingBox2d.fromExtrema
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
            BoundingBox3d.fromExtrema
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


{-| Decodes a `Polyline2d` from an array of vertices.
-}
polyline2d : Decoder Polyline2d
polyline2d =
    Decode.map Polyline2d.fromVertices (Decode.list point2d)


{-| Decodes a `Polyline3d` from an array of vertices.
-}
polyline3d : Decoder Polyline3d
polyline3d =
    Decode.map Polyline3d.fromVertices (Decode.list point3d)


{-| Decodes a `Polygon2d` from either:

  - an array of vertices defining a single-loop polygon, or
  - an object with:
      - an `outerLoop` field which is an array of encoded `Point2d` values
      - an `innerLoops` field which is an array of arrays of encoded `Point2d`
        values

-}
polygon2d : Decoder Polygon2d
polygon2d =
    let
        decodeLoop =
            Decode.list point2d
    in
    Decode.oneOf
        [ Decode.map Polygon2d.singleLoop decodeLoop
        , Decode.map2
            (\outerLoop innerLoops ->
                Polygon2d.with
                    { outerLoop = outerLoop
                    , innerLoops = innerLoops
                    }
            )
            (Decode.field "outerLoop" decodeLoop)
            (Decode.field "innerLoops" (Decode.list decodeLoop))
        ]


{-| Decodes a `Circle2d` from an object with `centerPoint` and `radius` fields.
-}
circle2d : Decoder Circle2d
circle2d =
    Decode.map2 Circle2d.withRadius
        (Decode.field "radius" Decode.float)
        (Decode.field "centerPoint" point2d)


{-| Decodes an `Ellipse2d` from an object with `centerPoint`, `xDirection`,
`xRadius` and `yRadius` fields.
-}
ellipse2d : Decoder Ellipse2d
ellipse2d =
    Decode.map4
        (\centerPoint xDirection xRadius yRadius ->
            Ellipse2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = xRadius
                , yRadius = yRadius
                }
        )
        (Decode.field "centerPoint" point2d)
        (Decode.field "xDirection" direction2d)
        (Decode.field "xRadius" Decode.float)
        (Decode.field "yRadius" Decode.float)


{-| Decodes a `Circle3d` from an object with `centerPoint`, `axialDirection` and
`radius` fields.
-}
circle3d : Decoder Circle3d
circle3d =
    Decode.map3 Circle3d.withRadius
        (Decode.field "radius" Decode.float)
        (Decode.field "axialDirection" direction3d)
        (Decode.field "centerPoint" point3d)


{-| Decodes a `Sphere3d` from an object with `centerPoint` and `radius` fields.
-}
sphere3d : Decoder Sphere3d
sphere3d =
    Decode.map2 Sphere3d.withRadius
        (Decode.field "radius" Decode.float)
        (Decode.field "centerPoint" point3d)


{-| Decodes a `QuadraticSpline2d` from an array of three control points.
-}
quadraticSpline2d : Decoder QuadraticSpline2d
quadraticSpline2d =
    Decode.map3
        (\startPoint controlPoint endPoint ->
            QuadraticSpline2d.with
                { startPoint = startPoint
                , controlPoint = controlPoint
                , endPoint = endPoint
                }
        )
        (Decode.index 0 point2d)
        (Decode.index 1 point2d)
        (Decode.index 2 point2d)


{-| Decodes a `QuadraticSpline3d` from an array of three control points.
-}
quadraticSpline3d : Decoder QuadraticSpline3d
quadraticSpline3d =
    Decode.map3
        (\startPoint controlPoint endPoint ->
            QuadraticSpline3d.with
                { startPoint = startPoint
                , controlPoint = controlPoint
                , endPoint = endPoint
                }
        )
        (Decode.index 0 point3d)
        (Decode.index 1 point3d)
        (Decode.index 2 point3d)


{-| Decodes a `CubicSpline2d` from an array of four control points.
-}
cubicSpline2d : Decoder CubicSpline2d
cubicSpline2d =
    Decode.map4
        (\startPoint startControlPoint endControlPoint endPoint ->
            CubicSpline2d.with
                { startPoint = startPoint
                , startControlPoint = startControlPoint
                , endControlPoint = endControlPoint
                , endPoint = endPoint
                }
        )
        (Decode.index 0 point2d)
        (Decode.index 1 point2d)
        (Decode.index 2 point2d)
        (Decode.index 3 point2d)


{-| Decodes a `CubicSpline3d` from an array of four control points.
-}
cubicSpline3d : Decoder CubicSpline3d
cubicSpline3d =
    Decode.map4
        (\startPoint startControlPoint endControlPoint endPoint ->
            CubicSpline3d.with
                { startPoint = startPoint
                , startControlPoint = startControlPoint
                , endControlPoint = endControlPoint
                , endPoint = endPoint
                }
        )
        (Decode.index 0 point3d)
        (Decode.index 1 point3d)
        (Decode.index 2 point3d)
        (Decode.index 3 point3d)
