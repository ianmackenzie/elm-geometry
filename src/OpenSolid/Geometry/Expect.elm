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


module OpenSolid.Geometry.Expect
    exposing
        ( Comparison
        , angle
        , angleWithin
        , approximately
        , arc2d
        , arc3d
        , axis2d
        , axis3d
        , boundingBox2d
        , boundingBox2dWithin
        , boundingBox3d
        , boundingBox3dWithin
        , circle2d
        , circle3d
        , cubicSpline2d
        , cubicSpline3d
        , direction2d
        , direction2dWithin
        , direction3d
        , direction3dWithin
        , expect
        , frame2d
        , frame3d
        , lineSegment2d
        , lineSegment2dWithin
        , lineSegment3d
        , lineSegment3dWithin
        , plane3d
        , point2d
        , point2dWithin
        , point3d
        , point3dWithin
        , polygon2d
        , polygon2dWithin
        , polyline2d
        , polyline2dWithin
        , polyline3d
        , polyline3dWithin
        , quadraticSpline2d
        , quadraticSpline3d
        , sketchPlane3d
        , sphere3d
        , triangle2d
        , triangle2dWithin
        , triangle3d
        , triangle3dWithin
        , validDirection2d
        , validDirection3d
        , validFrame2d
        , validFrame3d
        , valueIn
        , vector2d
        , vector2dWithin
        , vector3d
        , vector3dWithin
        )

import Expect exposing (Expectation)
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


type alias Comparison a =
    a -> a -> Bool


expect : Comparison a -> (a -> a -> Expectation)
expect comparison first second =
    if comparison first second then
        Expect.pass
    else
        let
            message =
                "Expected " ++ toString first ++ ", got " ++ toString second
        in
        Expect.fail message


by : Comparison b -> (a -> b) -> Comparison a
by comparison property first second =
    comparison (property first) (property second)


allOf : List (Comparison a) -> Comparison a
allOf comparisons first second =
    List.all (\comparison -> comparison first second) comparisons


listOf : Comparison a -> Comparison (List a)
listOf comparison firstList secondList =
    case ( firstList, secondList ) of
        ( [], [] ) ->
            True

        ( _, [] ) ->
            False

        ( [], _ ) ->
            False

        ( firstHead :: firstTail, secondHead :: secondTail ) ->
            comparison firstHead secondHead
                && listOf comparison firstTail secondTail


defaultTolerance : Float
defaultTolerance =
    1.0e-12


approximately : Float -> Float -> Expectation
approximately =
    Expect.within (Expect.Absolute defaultTolerance)


angle : Float -> Float -> Expectation
angle =
    angleWithin defaultTolerance


angleWithin : Float -> Float -> Float -> Expectation
angleWithin tolerance =
    let
        comparison firstAngle secondAngle =
            let
                difference =
                    secondAngle - firstAngle
            in
            abs (atan2 (sin difference) (cos difference)) <= tolerance
    in
    expect comparison


valueIn : Interval -> Float -> Expectation
valueIn interval value =
    let
        { minValue, maxValue } =
            Interval.extrema interval

        tolerance =
            defaultTolerance
    in
    if minValue - tolerance <= value && value <= maxValue + tolerance then
        Expect.pass
    else
        Expect.fail
            (toString value
                ++ " is not contained in the interval ["
                ++ toString minValue
                ++ ","
                ++ toString maxValue
                ++ "]"
            )


vector2d : Vector2d -> Vector2d -> Expectation
vector2d =
    vector2dWithin defaultTolerance


vector2dWithin : Float -> Vector2d -> Vector2d -> Expectation
vector2dWithin tolerance =
    expect (Vector2d.equalWithin tolerance)


vector3d : Vector3d -> Vector3d -> Expectation
vector3d =
    vector3dWithin defaultTolerance


vector3dWithin : Float -> Vector3d -> Vector3d -> Expectation
vector3dWithin tolerance =
    expect (Vector3d.equalWithin tolerance)


validDirection2d : Direction2d -> Expectation
validDirection2d direction =
    let
        length =
            Vector2d.length (Direction2d.toVector direction)
    in
    if abs (length - 1) <= defaultTolerance then
        Expect.pass
    else
        Expect.fail
            ("Expected "
                ++ toString direction
                ++ " to have length 1, but actually has length "
                ++ toString length
            )


direction2d : Direction2d -> Direction2d -> Expectation
direction2d =
    direction2dWithin defaultTolerance


direction2dWithin : Float -> Direction2d -> Direction2d -> Expectation
direction2dWithin tolerance =
    expect (Direction2d.equalWithin tolerance)


validDirection3d : Direction3d -> Expectation
validDirection3d direction =
    let
        length =
            Vector3d.length (Direction3d.toVector direction)
    in
    if abs (length - 1) <= defaultTolerance then
        Expect.pass
    else
        Expect.fail
            ("Expected "
                ++ toString direction
                ++ " to have length 1, but actually has length "
                ++ toString length
            )


direction3d : Direction3d -> Direction3d -> Expectation
direction3d =
    direction3dWithin defaultTolerance


direction3dWithin : Float -> Direction3d -> Direction3d -> Expectation
direction3dWithin tolerance =
    expect (Direction3d.equalWithin tolerance)


point2d : Point2d -> Point2d -> Expectation
point2d =
    point2dWithin defaultTolerance


point2dWithin : Float -> Point2d -> Point2d -> Expectation
point2dWithin tolerance =
    expect (Point2d.equalWithin tolerance)


point3d : Point3d -> Point3d -> Expectation
point3d =
    point3dWithin defaultTolerance


point3dWithin : Float -> Point3d -> Point3d -> Expectation
point3dWithin tolerance =
    expect (Point3d.equalWithin tolerance)


axis2d : Axis2d -> Axis2d -> Expectation
axis2d =
    expect
        (allOf
            [ by (Point2d.equalWithin defaultTolerance) Axis2d.originPoint
            , by (Direction2d.equalWithin defaultTolerance) Axis2d.direction
            ]
        )


axis3d : Axis3d -> Axis3d -> Expectation
axis3d =
    expect
        (allOf
            [ by (Point3d.equalWithin defaultTolerance) Axis3d.originPoint
            , by (Direction3d.equalWithin defaultTolerance) Axis3d.direction
            ]
        )


plane3d : Plane3d -> Plane3d -> Expectation
plane3d =
    expect
        (allOf
            [ by (Point3d.equalWithin defaultTolerance)
                Plane3d.originPoint
            , by (Direction3d.equalWithin defaultTolerance)
                Plane3d.normalDirection
            ]
        )


validFrame2d : Frame2d -> Expectation
validFrame2d =
    Expect.all
        [ Frame2d.xDirection >> validDirection2d
        , Frame2d.yDirection >> validDirection2d
        , \frame ->
            let
                xDirection =
                    Frame2d.xDirection frame

                yDirection =
                    Frame2d.yDirection frame

                parallelComponent =
                    Direction2d.componentIn xDirection yDirection
            in
            if abs parallelComponent <= defaultTolerance then
                Expect.pass
            else
                Expect.fail
                    ("Expected perpendicular basis directions, got "
                        ++ toString xDirection
                        ++ ", "
                        ++ toString yDirection
                    )
        ]


frame2d : Frame2d -> Frame2d -> Expectation
frame2d =
    expect
        (allOf
            [ by (Point2d.equalWithin defaultTolerance) Frame2d.originPoint
            , by (Direction2d.equalWithin defaultTolerance) Frame2d.xDirection
            , by (Direction2d.equalWithin defaultTolerance) Frame2d.yDirection
            ]
        )


validFrame3d : Frame3d -> Expectation
validFrame3d =
    Expect.all
        [ Frame3d.xDirection >> validDirection3d
        , Frame3d.yDirection >> validDirection3d
        , Frame3d.zDirection >> validDirection3d
        , \frame ->
            let
                xDirection =
                    Frame3d.xDirection frame

                yDirection =
                    Frame3d.yDirection frame

                zDirection =
                    Frame3d.zDirection frame

                xyComponent =
                    Direction3d.componentIn xDirection yDirection

                yzComponent =
                    Direction3d.componentIn yDirection zDirection

                zxComponent =
                    Direction3d.componentIn zDirection xDirection
            in
            if
                (abs xyComponent <= defaultTolerance)
                    && (abs yzComponent <= defaultTolerance)
                    && (abs zxComponent <= defaultTolerance)
            then
                Expect.pass
            else
                Expect.fail
                    ("Expected perpendicular basis directions, got "
                        ++ toString xDirection
                        ++ ", "
                        ++ toString yDirection
                        ++ ", "
                        ++ toString zDirection
                    )
        ]


frame3d : Frame3d -> Frame3d -> Expectation
frame3d =
    expect
        (allOf
            [ by (Point3d.equalWithin defaultTolerance) Frame3d.originPoint
            , by (Direction3d.equalWithin defaultTolerance) Frame3d.xDirection
            , by (Direction3d.equalWithin defaultTolerance) Frame3d.yDirection
            , by (Direction3d.equalWithin defaultTolerance) Frame3d.zDirection
            ]
        )


sketchPlane3d : SketchPlane3d -> SketchPlane3d -> Expectation
sketchPlane3d =
    expect
        (allOf
            [ by (Point3d.equalWithin defaultTolerance)
                SketchPlane3d.originPoint
            , by (Direction3d.equalWithin defaultTolerance)
                SketchPlane3d.xDirection
            , by (Direction3d.equalWithin defaultTolerance)
                SketchPlane3d.yDirection
            ]
        )


lineSegment2d : LineSegment2d -> LineSegment2d -> Expectation
lineSegment2d =
    lineSegment2dWithin defaultTolerance


lineSegment2dWithin : Float -> LineSegment2d -> LineSegment2d -> Expectation
lineSegment2dWithin tolerance =
    expect
        (allOf
            [ by (Point2d.equalWithin tolerance) LineSegment2d.startPoint
            , by (Point2d.equalWithin tolerance) LineSegment2d.endPoint
            ]
        )


lineSegment3d : LineSegment3d -> LineSegment3d -> Expectation
lineSegment3d =
    lineSegment3dWithin defaultTolerance


lineSegment3dWithin : Float -> LineSegment3d -> LineSegment3d -> Expectation
lineSegment3dWithin tolerance =
    expect
        (allOf
            [ by (Point3d.equalWithin tolerance) LineSegment3d.startPoint
            , by (Point3d.equalWithin tolerance) LineSegment3d.endPoint
            ]
        )


triangle2d : Triangle2d -> Triangle2d -> Expectation
triangle2d =
    triangle2dWithin defaultTolerance


triangle2dWithin : Float -> Triangle2d -> Triangle2d -> Expectation
triangle2dWithin tolerance =
    let
        comparison firstTriangle secondTriangle =
            let
                ( firstVertex1, firstVertex2, firstVertex3 ) =
                    Triangle2d.vertices firstTriangle

                ( secondVertex1, secondVertex2, secondVertex3 ) =
                    Triangle2d.vertices secondTriangle

                equalPoints =
                    Point2d.equalWithin tolerance
            in
            equalPoints firstVertex1 secondVertex1
                && equalPoints firstVertex2 secondVertex2
                && equalPoints firstVertex3 secondVertex3
    in
    expect comparison


triangle3d : Triangle3d -> Triangle3d -> Expectation
triangle3d =
    triangle3dWithin defaultTolerance


triangle3dWithin : Float -> Triangle3d -> Triangle3d -> Expectation
triangle3dWithin tolerance =
    let
        comparison firstTriangle secondTriangle =
            let
                ( firstVertex1, firstVertex2, firstVertex3 ) =
                    Triangle3d.vertices firstTriangle

                ( secondVertex1, secondVertex2, secondVertex3 ) =
                    Triangle3d.vertices secondTriangle

                equalPoints =
                    Point3d.equalWithin tolerance
            in
            equalPoints firstVertex1 secondVertex1
                && equalPoints firstVertex2 secondVertex2
                && equalPoints firstVertex3 secondVertex3
    in
    expect comparison


boundingBox2d : BoundingBox2d -> BoundingBox2d -> Expectation
boundingBox2d =
    boundingBox2dWithin defaultTolerance


boundingBox2dWithin : Float -> BoundingBox2d -> BoundingBox2d -> Expectation
boundingBox2dWithin tolerance =
    expect
        (allOf
            [ by (Scalar.equalWithin tolerance) BoundingBox2d.minX
            , by (Scalar.equalWithin tolerance) BoundingBox2d.maxX
            , by (Scalar.equalWithin tolerance) BoundingBox2d.minY
            , by (Scalar.equalWithin tolerance) BoundingBox2d.maxY
            ]
        )


boundingBox3d : BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3d =
    boundingBox3dWithin defaultTolerance


boundingBox3dWithin : Float -> BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3dWithin tolerance =
    expect
        (allOf
            [ by (Scalar.equalWithin tolerance) BoundingBox3d.minX
            , by (Scalar.equalWithin tolerance) BoundingBox3d.maxX
            , by (Scalar.equalWithin tolerance) BoundingBox3d.minY
            , by (Scalar.equalWithin tolerance) BoundingBox3d.maxY
            , by (Scalar.equalWithin tolerance) BoundingBox3d.minZ
            , by (Scalar.equalWithin tolerance) BoundingBox3d.maxZ
            ]
        )


polyline2d : Polyline2d -> Polyline2d -> Expectation
polyline2d =
    polyline2dWithin defaultTolerance


polyline2dWithin : Float -> Polyline2d -> Polyline2d -> Expectation
polyline2dWithin tolerance =
    expect (by (listOf (Point2d.equalWithin tolerance)) Polyline2d.vertices)


polyline3d : Polyline3d -> Polyline3d -> Expectation
polyline3d =
    polyline3dWithin defaultTolerance


polyline3dWithin : Float -> Polyline3d -> Polyline3d -> Expectation
polyline3dWithin tolerance =
    expect (by (listOf (Point3d.equalWithin tolerance)) Polyline3d.vertices)


polygon2d : Polygon2d -> Polygon2d -> Expectation
polygon2d =
    polygon2dWithin defaultTolerance


polygon2dWithin : Float -> Polygon2d -> Polygon2d -> Expectation
polygon2dWithin tolerance =
    expect (by (listOf (Point2d.equalWithin tolerance)) Polygon2d.vertices)


circle2d : Circle2d -> Circle2d -> Expectation
circle2d =
    expect
        (allOf
            [ by (Point2d.equalWithin defaultTolerance) Circle2d.centerPoint
            , by (Scalar.equalWithin defaultTolerance) Circle2d.radius
            ]
        )


circle3d : Circle3d -> Circle3d -> Expectation
circle3d =
    expect
        (allOf
            [ by (Point3d.equalWithin defaultTolerance)
                Circle3d.centerPoint
            , by (Direction3d.equalWithin defaultTolerance)
                (Circle3d.axis >> Axis3d.direction)
            , by (Scalar.equalWithin defaultTolerance)
                Circle3d.radius
            ]
        )


sphere3d : Sphere3d -> Sphere3d -> Expectation
sphere3d =
    expect
        (allOf
            [ by (Point3d.equalWithin defaultTolerance) Sphere3d.centerPoint
            , by (Scalar.equalWithin defaultTolerance) Sphere3d.radius
            ]
        )


arc2d : Arc2d -> Arc2d -> Expectation
arc2d =
    expect
        (allOf
            [ by (Point2d.equalWithin defaultTolerance) Arc2d.centerPoint
            , by (Point2d.equalWithin defaultTolerance) Arc2d.startPoint
            , by (Scalar.equalWithin defaultTolerance) Arc2d.sweptAngle
            ]
        )


arc3d : Arc3d -> Arc3d -> Expectation
arc3d =
    expect
        (allOf
            [ by (Point3d.equalWithin defaultTolerance) Arc3d.centerPoint
            , by (Direction3d.equalWithin defaultTolerance)
                (Arc3d.axis >> Axis3d.direction)
            , by (Point3d.equalWithin defaultTolerance) Arc3d.startPoint
            , by (Scalar.equalWithin defaultTolerance) Arc3d.sweptAngle
            ]
        )


quadraticSpline2d : QuadraticSpline2d -> QuadraticSpline2d -> Expectation
quadraticSpline2d =
    expect
        (\firstSpline secondSpline ->
            let
                ( p1, p2, p3 ) =
                    QuadraticSpline2d.controlPoints firstSpline

                ( q1, q2, q3 ) =
                    QuadraticSpline2d.controlPoints secondSpline

                equal =
                    Point2d.equalWithin defaultTolerance
            in
            equal p1 q1 && equal p2 q2 && equal p3 q3
        )


quadraticSpline3d : QuadraticSpline3d -> QuadraticSpline3d -> Expectation
quadraticSpline3d =
    expect
        (\firstSpline secondSpline ->
            let
                ( p1, p2, p3 ) =
                    QuadraticSpline3d.controlPoints firstSpline

                ( q1, q2, q3 ) =
                    QuadraticSpline3d.controlPoints secondSpline

                equal =
                    Point3d.equalWithin defaultTolerance
            in
            equal p1 q1 && equal p2 q2 && equal p3 q3
        )


cubicSpline2d : CubicSpline2d -> CubicSpline2d -> Expectation
cubicSpline2d =
    expect
        (\firstSpline secondSpline ->
            let
                ( p1, p2, p3, p4 ) =
                    CubicSpline2d.controlPoints firstSpline

                ( q1, q2, q3, q4 ) =
                    CubicSpline2d.controlPoints secondSpline

                equal =
                    Point2d.equalWithin defaultTolerance
            in
            equal p1 q1 && equal p2 q2 && equal p3 q3 && equal p4 q4
        )


cubicSpline3d : CubicSpline3d -> CubicSpline3d -> Expectation
cubicSpline3d =
    expect
        (\firstSpline secondSpline ->
            let
                ( p1, p2, p3, p4 ) =
                    CubicSpline3d.controlPoints firstSpline

                ( q1, q2, q3, q4 ) =
                    CubicSpline3d.controlPoints secondSpline

                equal =
                    Point3d.equalWithin defaultTolerance
            in
            equal p1 q1 && equal p2 q2 && equal p3 q3 && equal p4 q4
        )
