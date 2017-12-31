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


module OpenSolid.Geometry.Fuzz
    exposing
        ( arc2d
        , arc3d
        , axis2d
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
        , ellipticalArc2d
        , frame2d
        , frame3d
        , interval
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
        , scalar
        , sketchPlane3d
        , sphere3d
        , triangle2d
        , triangle3d
        , vector2d
        , vector3d
        )

import Fuzz exposing (Fuzzer)
import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.Arc3d as Arc3d exposing (Arc3d)
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Circle3d as Circle3d exposing (Circle3d)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.CubicSpline3d as CubicSpline3d exposing (CubicSpline3d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Ellipse2d as Ellipse2d exposing (Ellipse2d)
import OpenSolid.EllipticalArc2d as EllipticalArc2d exposing (EllipticalArc2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Interval as Interval exposing (Interval)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d)
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d exposing (QuadraticSpline3d)
import OpenSolid.Scalar as Scalar
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Sphere3d as Sphere3d exposing (Sphere3d)
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import OpenSolid.Triangle3d as Triangle3d exposing (Triangle3d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


scalar : Fuzzer Float
scalar =
    Fuzz.floatRange -10 10


positiveScalar : Fuzzer Float
positiveScalar =
    Fuzz.map abs scalar


interval : Fuzzer Interval
interval =
    Fuzz.map2 Scalar.hull scalar scalar


vector2d : Fuzzer Vector2d
vector2d =
    Fuzz.map Vector2d.fromComponents (Fuzz.tuple ( scalar, scalar ))


vector3d : Fuzzer Vector3d
vector3d =
    Fuzz.map Vector3d.fromComponents (Fuzz.tuple3 ( scalar, scalar, scalar ))


direction2d : Fuzzer Direction2d
direction2d =
    Fuzz.map Direction2d.fromAngle (Fuzz.floatRange -pi pi)


direction3d : Fuzzer Direction3d
direction3d =
    let
        phi =
            Fuzz.map acos (Fuzz.floatRange -1 1)

        theta =
            Fuzz.floatRange -pi pi

        direction phi theta =
            let
                r =
                    sin phi

                x =
                    r * cos theta

                y =
                    r * sin theta

                z =
                    cos phi
            in
            Direction3d.unsafe ( x, y, z )
    in
    Fuzz.map2 direction phi theta


point2d : Fuzzer Point2d
point2d =
    Fuzz.map Point2d.fromCoordinates (Fuzz.tuple ( scalar, scalar ))


point3d : Fuzzer Point3d
point3d =
    Fuzz.map Point3d.fromCoordinates (Fuzz.tuple3 ( scalar, scalar, scalar ))


axis2d : Fuzzer Axis2d
axis2d =
    let
        axis originPoint direction =
            Axis2d.with { originPoint = originPoint, direction = direction }
    in
    Fuzz.map2 axis point2d direction2d


axis3d : Fuzzer Axis3d
axis3d =
    let
        axis originPoint direction =
            Axis3d.with { originPoint = originPoint, direction = direction }
    in
    Fuzz.map2 axis point3d direction3d


plane3d : Fuzzer Plane3d
plane3d =
    let
        plane originPoint normalDirection =
            Plane3d.with
                { originPoint = originPoint
                , normalDirection = normalDirection
                }
    in
    Fuzz.map2 plane point3d direction3d


frame2d : Fuzzer Frame2d
frame2d =
    let
        frame originPoint xDirection rightHanded =
            let
                rightHandedFrame =
                    Frame2d.with
                        { originPoint = originPoint
                        , xDirection = xDirection
                        }
            in
            if rightHanded then
                rightHandedFrame
            else
                Frame2d.flipY rightHandedFrame
    in
    Fuzz.map3 frame point2d direction2d Fuzz.bool


frame3d : Fuzzer Frame3d
frame3d =
    let
        frame originPoint xDirection flipY flipZ =
            let
                ( yDirection, zDirection ) =
                    Direction3d.perpendicularBasis xDirection
            in
            Frame3d.unsafe
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection =
                    if flipY then
                        Direction3d.flip yDirection
                    else
                        yDirection
                , zDirection =
                    if flipZ then
                        Direction3d.flip zDirection
                    else
                        zDirection
                }
    in
    Fuzz.map4 frame point3d direction3d Fuzz.bool Fuzz.bool


sketchPlane3d : Fuzzer SketchPlane3d
sketchPlane3d =
    let
        sketchPlane originPoint xDirection =
            SketchPlane3d.unsafe
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = Direction3d.perpendicularTo xDirection
                }
    in
    Fuzz.map2 sketchPlane point3d direction3d


lineSegment2d : Fuzzer LineSegment2d
lineSegment2d =
    Fuzz.map LineSegment2d.fromEndpoints (Fuzz.tuple ( point2d, point2d ))


lineSegment3d : Fuzzer LineSegment3d
lineSegment3d =
    Fuzz.map LineSegment3d.fromEndpoints (Fuzz.tuple ( point3d, point3d ))


triangle2d : Fuzzer Triangle2d
triangle2d =
    Fuzz.map Triangle2d.fromVertices (Fuzz.tuple3 ( point2d, point2d, point2d ))


triangle3d : Fuzzer Triangle3d
triangle3d =
    Fuzz.map Triangle3d.fromVertices (Fuzz.tuple3 ( point3d, point3d, point3d ))


boundingBox2d : Fuzzer BoundingBox2d
boundingBox2d =
    let
        boundingBox xInterval yInterval =
            BoundingBox2d.with
                { minX = Interval.minValue xInterval
                , maxX = Interval.maxValue xInterval
                , minY = Interval.minValue yInterval
                , maxY = Interval.maxValue yInterval
                }
    in
    Fuzz.map2 boundingBox interval interval


boundingBox3d : Fuzzer BoundingBox3d
boundingBox3d =
    let
        boundingBox xInterval yInterval zInterval =
            BoundingBox3d.with
                { minX = Interval.minValue xInterval
                , maxX = Interval.maxValue xInterval
                , minY = Interval.minValue yInterval
                , maxY = Interval.maxValue yInterval
                , minZ = Interval.minValue zInterval
                , maxZ = Interval.maxValue zInterval
                }
    in
    Fuzz.map3 boundingBox interval interval interval


polyline2d : Fuzzer Polyline2d
polyline2d =
    Fuzz.map Polyline2d.fromVertices (Fuzz.list point2d)


polyline3d : Fuzzer Polyline3d
polyline3d =
    Fuzz.map Polyline3d.fromVertices (Fuzz.list point3d)


polygon2d : Fuzzer Polygon2d
polygon2d =
    Fuzz.map Polygon2d.fromVertices (Fuzz.list point2d)


circle2d : Fuzzer Circle2d
circle2d =
    let
        circle centerPoint radius =
            Circle2d.with { centerPoint = centerPoint, radius = radius }
    in
    Fuzz.map2 circle point2d positiveScalar


circle3d : Fuzzer Circle3d
circle3d =
    let
        circle centerPoint axialDirection radius =
            Circle3d.with
                { centerPoint = centerPoint
                , axialDirection = axialDirection
                , radius = radius
                }
    in
    Fuzz.map3 circle point3d direction3d positiveScalar


sphere3d : Fuzzer Sphere3d
sphere3d =
    let
        sphere centerPoint radius =
            Sphere3d.with { centerPoint = centerPoint, radius = radius }
    in
    Fuzz.map2 sphere point3d positiveScalar


arc2d : Fuzzer Arc2d
arc2d =
    let
        arc centerPoint startPoint sweptAngle =
            Arc2d.with
                { centerPoint = centerPoint
                , startPoint = startPoint
                , sweptAngle = sweptAngle
                }
    in
    Fuzz.map3 arc point2d point2d (Fuzz.floatRange (-3 * pi) (3 * pi))


arc3d : Fuzzer Arc3d
arc3d =
    let
        arc axis startPoint sweptAngle =
            Arc3d.around axis
                { startPoint = startPoint
                , sweptAngle = sweptAngle
                }
    in
    Fuzz.map3 arc axis3d point3d (Fuzz.floatRange (-3 * pi) (3 * pi))


quadraticSpline2d : Fuzzer QuadraticSpline2d
quadraticSpline2d =
    Fuzz.tuple3 ( point2d, point2d, point2d )
        |> Fuzz.map QuadraticSpline2d.fromControlPoints


quadraticSpline3d : Fuzzer QuadraticSpline3d
quadraticSpline3d =
    Fuzz.tuple3 ( point3d, point3d, point3d )
        |> Fuzz.map QuadraticSpline3d.fromControlPoints


cubicSpline2d : Fuzzer CubicSpline2d
cubicSpline2d =
    Fuzz.tuple4 ( point2d, point2d, point2d, point2d )
        |> Fuzz.map CubicSpline2d.fromControlPoints


cubicSpline3d : Fuzzer CubicSpline3d
cubicSpline3d =
    Fuzz.tuple4 ( point3d, point3d, point3d, point3d )
        |> Fuzz.map CubicSpline3d.fromControlPoints


ellipse2d : Fuzzer Ellipse2d
ellipse2d =
    let
        ellipse centerPoint xDirection xRadius yRadius =
            Ellipse2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = xRadius
                , yRadius = yRadius
                }
    in
    Fuzz.map4 ellipse point2d direction2d positiveScalar positiveScalar


ellipticalArc2d : Fuzzer EllipticalArc2d
ellipticalArc2d =
    let
        angle =
            Fuzz.floatRange -(2 * pi) (2 * pi)

        ellipticalArc ( centerPoint, xDirection ) ( xRadius, yRadius ) ( startAngle, sweptAngle ) =
            EllipticalArc2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = xRadius
                , yRadius = yRadius
                , startAngle = startAngle
                , sweptAngle = sweptAngle
                }
    in
    Fuzz.map3 ellipticalArc
        (Fuzz.tuple ( point2d, direction2d ))
        (Fuzz.tuple ( positiveScalar, positiveScalar ))
        (Fuzz.tuple ( angle, angle ))
