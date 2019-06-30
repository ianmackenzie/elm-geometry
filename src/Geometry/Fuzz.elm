--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Fuzz exposing
    ( angle
    , arc2d
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
    , length
    , lineSegment2d
    , lineSegment3d
    , parameterValue
    , plane3d
    , point2d
    , point3d
    , polygon2d
    , polyline2d
    , polyline3d
    , positiveLength
    , quadraticSpline2d
    , quadraticSpline3d
    , rectangle2d
    , scale
    , sketchPlane3d
    , sphere3d
    , triangle2d
    , triangle3d
    , vector2d
    , vector3d
    )

import Angle exposing (Angle)
import Arc2d
import Arc3d
import Axis2d
import Axis3d
import BoundingBox2d
import BoundingBox3d
import Circle2d
import Circle3d
import CubicSpline2d
import CubicSpline3d
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction2d
import Direction3d
import Ellipse2d
import EllipticalArc2d
import Frame2d
import Frame3d
import Fuzz exposing (Fuzzer)
import Geometry.Test exposing (..)
import Length exposing (Length, meters)
import LineSegment2d
import LineSegment3d
import Plane3d
import Point2d
import Point3d
import Polygon2d
import Polygon2d.Random as Random
import Polyline2d
import Polyline3d
import QuadraticSpline2d
import QuadraticSpline3d
import Quantity exposing (Quantity)
import Rectangle2d
import Shrink
import SketchPlane3d
import Sphere3d
import Triangle2d
import Triangle3d
import Vector2d
import Vector3d


length : Fuzzer Length
length =
    Fuzz.map Length.meters (Fuzz.floatRange -10 10)


scale : Fuzzer Float
scale =
    Fuzz.floatRange -10 10


angle : Fuzzer Angle
angle =
    Fuzz.map Angle.radians (Fuzz.floatRange (-2 * pi) (2 * pi))


positiveLength : Fuzzer Length
positiveLength =
    Fuzz.map Quantity.abs length


parameterValue : Fuzzer ParameterValue
parameterValue =
    Fuzz.map ParameterValue.clamped (Fuzz.floatRange 0 1)


vector2d : Fuzzer (Vector2d coordinates)
vector2d =
    Fuzz.map2 Vector2d.xy length length


vector3d : Fuzzer (Vector3d coordinates)
vector3d =
    Fuzz.map3 Vector3d.xyz length length length


direction2d : Fuzzer (Direction2d coordinates)
direction2d =
    Fuzz.map Direction2d.fromAngle angle


direction3d : Fuzzer (Direction3d coordinates)
direction3d =
    let
        phiFuzzer =
            Fuzz.map (acos >> Angle.radians) (Fuzz.floatRange -1 1)

        thetaFuzzer =
            Fuzz.map Angle.radians (Fuzz.floatRange -pi pi)

        toDirection phi theta =
            let
                r =
                    Angle.sin phi
            in
            Direction3d.unsafe
                { x = r * Angle.cos theta
                , y = r * Angle.sin theta
                , z = Angle.cos phi
                }
    in
    Fuzz.map2 toDirection phiFuzzer thetaFuzzer


point2d : Fuzzer (Point2d coordinates)
point2d =
    Fuzz.map2 Point2d.xy length length


point3d : Fuzzer (Point3d coordinates)
point3d =
    Fuzz.map3 Point3d.xyz length length length


axis2d : Fuzzer (Axis2d coordinates)
axis2d =
    Fuzz.map2 Axis2d.through point2d direction2d


axis3d : Fuzzer (Axis3d coordinates)
axis3d =
    Fuzz.map2 Axis3d.through point3d direction3d


plane3d : Fuzzer (Plane3d coordinates)
plane3d =
    Fuzz.map2 Plane3d.through point3d direction3d


frame2d : Fuzzer (Frame2d coordinates)
frame2d =
    let
        frame originPoint xDirection rightHanded =
            let
                rightHandedFrame =
                    Frame2d.withXDirection xDirection originPoint
            in
            if rightHanded then
                rightHandedFrame

            else
                Frame2d.reverseY rightHandedFrame
    in
    Fuzz.map3 frame point2d direction2d Fuzz.bool


frame3d : Fuzzer (Frame3d coordinates)
frame3d =
    let
        frame originPoint xDirection reverseY reverseZ =
            let
                ( yDirection, zDirection ) =
                    Direction3d.perpendicularBasis xDirection
            in
            Frame3d.unsafe
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection =
                    if reverseY then
                        Direction3d.reverse yDirection

                    else
                        yDirection
                , zDirection =
                    if reverseZ then
                        Direction3d.reverse zDirection

                    else
                        zDirection
                }
    in
    Fuzz.map4 frame point3d direction3d Fuzz.bool Fuzz.bool


sketchPlane3d : Fuzzer (SketchPlane3d coordinates)
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


lineSegment2d : Fuzzer (LineSegment2d coordinates)
lineSegment2d =
    Fuzz.map2 LineSegment2d.from point2d point2d


lineSegment3d : Fuzzer (LineSegment3d coordinates)
lineSegment3d =
    Fuzz.map2 LineSegment3d.from point3d point3d


triangle2d : Fuzzer (Triangle2d coordinates)
triangle2d =
    Fuzz.map3 Triangle2d.fromVertices point2d point2d point2d


triangle3d : Fuzzer (Triangle3d coordinates)
triangle3d =
    Fuzz.map3 Triangle3d.fromVertices point3d point3d point3d


boundingBox2d : Fuzzer (BoundingBox2d coordinates)
boundingBox2d =
    Fuzz.map2 BoundingBox2d.from point2d point2d


boundingBox3d : Fuzzer (BoundingBox3d coordinates)
boundingBox3d =
    Fuzz.map2 BoundingBox3d.from point3d point3d


polyline2d : Fuzzer (Polyline2d coordinates)
polyline2d =
    Fuzz.map Polyline2d.fromVertices (Fuzz.list point2d)


polyline3d : Fuzzer (Polyline3d coordinates)
polyline3d =
    Fuzz.map Polyline3d.fromVertices (Fuzz.list point3d)


polygon2d : Fuzzer (Polygon2d coordinates)
polygon2d =
    let
        boundingBox =
            BoundingBox2d.fromExtrema
                { minX = meters -10
                , maxX = meters 10
                , minY = meters -10
                , maxY = meters 10
                }
    in
    Fuzz.custom (Random.polygon2d boundingBox) Shrink.noShrink


circle2d : Fuzzer (Circle2d coordinates)
circle2d =
    Fuzz.map2 Circle2d.withRadius positiveLength point2d


circle3d : Fuzzer (Circle3d coordinates)
circle3d =
    Fuzz.map3 Circle3d.withRadius positiveLength direction3d point3d


sphere3d : Fuzzer (Sphere3d coordinates)
sphere3d =
    Fuzz.map2 Sphere3d.withRadius positiveLength point3d


arc2d : Fuzzer (Arc2d coordinates)
arc2d =
    Fuzz.map3 Arc2d.from
        point2d
        point2d
        (Fuzz.oneOf
            [ Fuzz.floatRange -359 359
            , Fuzz.floatRange 361 719
            , Fuzz.floatRange -719 -361
            ]
            |> Fuzz.map Angle.degrees
        )


arc3d : Fuzzer (Arc3d coordinates)
arc3d =
    Fuzz.map2 Arc3d.on sketchPlane3d arc2d


quadraticSpline2d : Fuzzer (QuadraticSpline2d coordinates)
quadraticSpline2d =
    Fuzz.map3 QuadraticSpline2d.fromControlPoints point2d point2d point2d


quadraticSpline3d : Fuzzer (QuadraticSpline3d coordinates)
quadraticSpline3d =
    Fuzz.map3 QuadraticSpline3d.fromControlPoints point3d point3d point3d


cubicSpline2d : Fuzzer (CubicSpline2d coordinates)
cubicSpline2d =
    Fuzz.map4 CubicSpline2d.fromControlPoints point2d point2d point2d point2d


cubicSpline3d : Fuzzer (CubicSpline3d coordinates)
cubicSpline3d =
    Fuzz.map4 CubicSpline3d.fromControlPoints point3d point3d point3d point3d


ellipse2d : Fuzzer (Ellipse2d coordinates)
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
    Fuzz.map4 ellipse point2d direction2d positiveLength positiveLength


ellipticalArc2d : Fuzzer (EllipticalArc2d coordinates)
ellipticalArc2d =
    let
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
        (Fuzz.tuple ( positiveLength, positiveLength ))
        (Fuzz.tuple ( angle, angle ))


rectangle2d : Fuzzer (Rectangle2d coordinates)
rectangle2d =
    let
        rectangle frame width height =
            Rectangle2d.centeredOn frame ( width, height )
    in
    Fuzz.map3 rectangle frame2d positiveLength positiveLength
