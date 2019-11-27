--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Expect exposing
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
    , cylinder3d
    , direction2d
    , direction2dWithin
    , direction3d
    , direction3dWithin
    , exactly
    , expect
    , float
    , frame2d
    , frame3d
    , just
    , lineSegment2d
    , lineSegment2dWithin
    , lineSegment3d
    , lineSegment3dWithin
    , maybe
    , plane3d
    , point2d
    , point2dContainedIn
    , point2dWithin
    , point3d
    , point3dContainedIn
    , point3dWithin
    , polygon2d
    , polygon2dWithin
    , polyline2d
    , polyline2dWithin
    , polyline3d
    , polyline3dWithin
    , quadraticSpline2d
    , quadraticSpline3d
    , quantityAtLeast
    , quantityAtMost
    , quantityGreaterThan
    , quantityLessThan
    , quantityWithin
    , roundTrip
    , sketchPlane3d
    , sphere3d
    , triangle2d
    , triangle2dWithin
    , triangle3d
    , triangle3dWithin
    , validBoundingBox2d
    , validBoundingBox3d
    , validDirection2d
    , validDirection3d
    , validFrame2d
    , validFrame3d
    , validSketchPlane3d
    , vector2d
    , vector2dWithin
    , vector3d
    , vector3dWithin
    )

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d exposing (Cylinder3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Expect exposing (Expectation)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Length exposing (Length)
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
import Quantity exposing (Quantity(..))
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


type alias Comparison a =
    a -> a -> Bool


expect : Comparison a -> (a -> a -> Expectation)
expect comparison first second =
    if comparison first second then
        Expect.pass

    else
        let
            message =
                "Expected " ++ Debug.toString first ++ ", got " ++ Debug.toString second
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


just : (expected -> actual -> Expectation) -> expected -> Maybe actual -> Expectation
just expect_ expectedValue actualMaybe =
    case actualMaybe of
        Just actualValue ->
            actualValue |> expect_ expectedValue

        Nothing ->
            Expect.fail "Expected a Just but got Nothing"


maybe : (expected -> actual -> Expectation) -> Maybe expected -> Maybe actual -> Expectation
maybe expect_ expectedMaybe actualMaybe =
    case ( expectedMaybe, actualMaybe ) of
        ( Just expectedValue, Just actualValue ) ->
            actualValue |> expect_ expectedValue

        ( Just _, Nothing ) ->
            Expect.fail "Expected a Just but got Nothing"

        ( Nothing, Just _ ) ->
            Expect.fail "Expected Nothing but got a Just"

        ( Nothing, Nothing ) ->
            Expect.pass


roundTrip : (a -> a -> Expectation) -> (a -> a) -> a -> Expectation
roundTrip comparison function value =
    comparison value (function value)


defaultTolerance : Float
defaultTolerance =
    1.0e-12


quantityWithin : Quantity Float units -> Quantity Float units -> Quantity Float units -> Expectation
quantityWithin (Quantity tolerance) (Quantity first) (Quantity second) =
    Expect.within (Expect.Absolute tolerance) first second


approximately : Quantity Float units -> Quantity Float units -> Expectation
approximately (Quantity first) (Quantity second) =
    Expect.within (Expect.AbsoluteOrRelative defaultTolerance defaultTolerance)
        first
        second


exactly : Float -> Float -> Expectation
exactly actual expected =
    actual |> Expect.within (Expect.Absolute 0.0) expected


float : Float -> Float -> Expectation
float first second =
    Expect.within (Expect.AbsoluteOrRelative defaultTolerance defaultTolerance)
        first
        second


quantityGreaterThan : Quantity Float units -> Quantity Float units -> Expectation
quantityGreaterThan (Quantity y) (Quantity x) =
    x |> Expect.greaterThan y


quantityLessThan : Quantity Float units -> Quantity Float units -> Expectation
quantityLessThan (Quantity y) (Quantity x) =
    x |> Expect.lessThan y


quantityAtMost : Quantity Float units -> Quantity Float units -> Expectation
quantityAtMost (Quantity y) (Quantity x) =
    x |> Expect.atMost y


quantityAtLeast : Quantity Float units -> Quantity Float units -> Expectation
quantityAtLeast (Quantity y) (Quantity x) =
    x |> Expect.atLeast y


absoluteToleranceFor : Quantity Float units -> Quantity Float units
absoluteToleranceFor magnitude =
    Quantity.max (Quantity defaultTolerance)
        (Quantity.abs magnitude |> Quantity.multiplyBy defaultTolerance)


angle : Angle -> Angle -> Expectation
angle first second =
    angleWithin (Angle.radians defaultTolerance) first second


angleWithin : Angle -> Angle -> Angle -> Expectation
angleWithin tolerance =
    let
        comparison firstAngle secondAngle =
            let
                difference =
                    secondAngle |> Quantity.minus firstAngle

                sine =
                    Angle.sin difference

                cosine =
                    Angle.cos difference

                delta =
                    Angle.radians (atan2 sine cosine)
            in
            Quantity.abs delta |> Quantity.lessThanOrEqualTo tolerance
    in
    expect comparison


vector2d : Vector2d units coordinates -> Vector2d units coordinates -> Expectation
vector2d first second =
    vector2dWithin (absoluteToleranceFor (Vector2d.length first)) first second


vector2dWithin : Quantity Float units -> Vector2d units coordinates -> Vector2d units coordinates -> Expectation
vector2dWithin tolerance =
    expect (Vector2d.equalWithin tolerance)


vector3d : Vector3d units coordinates -> Vector3d units coordinates -> Expectation
vector3d first second =
    vector3dWithin (absoluteToleranceFor (Vector3d.length first)) first second


vector3dWithin : Quantity Float units -> Vector3d units coordinates -> Vector3d units coordinates -> Expectation
vector3dWithin tolerance =
    expect (Vector3d.equalWithin tolerance)


validDirection2d : Direction2d coordinates -> Expectation
validDirection2d direction =
    let
        x =
            Direction2d.xComponent direction

        y =
            Direction2d.yComponent direction

        length =
            sqrt (x ^ 2 + y ^ 2)
    in
    if abs (length - 1) <= defaultTolerance then
        Expect.pass

    else
        Expect.fail
            ("Expected "
                ++ Debug.toString direction
                ++ " to have length 1, but actually has length "
                ++ String.fromFloat length
            )


direction2d : Direction2d coordinates -> Direction2d coordinates -> Expectation
direction2d =
    direction2dWithin (Angle.radians defaultTolerance)


direction2dWithin : Angle -> Direction2d coordinates -> Direction2d coordinates -> Expectation
direction2dWithin tolerance =
    expect (Direction2d.equalWithin tolerance)


validDirection3d : Direction3d coordinates -> Expectation
validDirection3d direction =
    let
        x =
            Direction3d.xComponent direction

        y =
            Direction3d.yComponent direction

        z =
            Direction3d.zComponent direction

        length =
            sqrt (x ^ 2 + y ^ 2 + z ^ 2)
    in
    if abs (length - 1) <= defaultTolerance then
        Expect.pass

    else
        Expect.fail
            ("Expected "
                ++ Debug.toString direction
                ++ " to have length 1, but actually has length "
                ++ String.fromFloat length
            )


direction3d : Direction3d coordinates -> Direction3d coordinates -> Expectation
direction3d =
    direction3dWithin (Angle.radians defaultTolerance)


direction3dWithin : Angle -> Direction3d coordinates -> Direction3d coordinates -> Expectation
direction3dWithin tolerance =
    expect (Direction3d.equalWithin tolerance)


point2dTolerance : Point2d units coordinates -> Quantity Float units
point2dTolerance point =
    absoluteToleranceFor (Point2d.distanceFrom Point2d.origin point)


point2d : Point2d units coordinates -> Point2d units coordinates -> Expectation
point2d first second =
    point2dWithin (point2dTolerance first) first second


point2dWithin : Quantity Float units -> Point2d units coordinates -> Point2d units coordinates -> Expectation
point2dWithin tolerance =
    expect (Point2d.equalWithin tolerance)


point3dTolerance : Point3d units coordinates -> Quantity Float units
point3dTolerance point =
    absoluteToleranceFor (Point3d.distanceFrom Point3d.origin point)


point3d : Point3d units coordinates -> Point3d units coordinates -> Expectation
point3d first second =
    point3dWithin (point3dTolerance first) first second


point3dWithin : Quantity Float units -> Point3d units coordinates -> Point3d units coordinates -> Expectation
point3dWithin tolerance =
    expect (Point3d.equalWithin tolerance)


axis2d : Axis2d units coordinates -> Axis2d units coordinates -> Expectation
axis2d first =
    Expect.all
        [ Axis2d.originPoint >> point2d (Axis2d.originPoint first)
        , Axis2d.direction >> direction2d (Axis2d.direction first)
        ]


axis3d : Axis3d units coordinates -> Axis3d units coordinates -> Expectation
axis3d first =
    Expect.all
        [ Axis3d.originPoint >> point3d (Axis3d.originPoint first)
        , Axis3d.direction >> direction3d (Axis3d.direction first)
        ]


plane3d : Plane3d units coordinates -> Plane3d units coordinates -> Expectation
plane3d first =
    Expect.all
        [ Plane3d.originPoint >> point3d (Plane3d.originPoint first)
        , Plane3d.normalDirection >> direction3d (Plane3d.normalDirection first)
        ]


validFrame2d : Frame2d units coordinates defines -> Expectation
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
                        ++ Debug.toString xDirection
                        ++ ", "
                        ++ Debug.toString yDirection
                    )
        ]


frame2d : Frame2d units coordinates defines -> Frame2d units coordinates defines -> Expectation
frame2d first =
    Expect.all
        [ Frame2d.originPoint >> point2d (Frame2d.originPoint first)
        , Frame2d.xDirection >> direction2d (Frame2d.xDirection first)
        , Frame2d.yDirection >> direction2d (Frame2d.yDirection first)
        ]


validFrame3d : Frame3d units coordinates defines -> Expectation
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
                        ++ Debug.toString xDirection
                        ++ ", "
                        ++ Debug.toString yDirection
                        ++ ", "
                        ++ Debug.toString zDirection
                    )
        ]


frame3d : Frame3d units coordinates defines -> Frame3d units coordinates defines -> Expectation
frame3d first =
    Expect.all
        [ Frame3d.originPoint >> point3d (Frame3d.originPoint first)
        , Frame3d.xDirection >> direction3d (Frame3d.xDirection first)
        , Frame3d.yDirection >> direction3d (Frame3d.yDirection first)
        , Frame3d.zDirection >> direction3d (Frame3d.zDirection first)
        ]


sketchPlane3d : SketchPlane3d units coordinates defines -> SketchPlane3d units coordinates defines -> Expectation
sketchPlane3d first =
    Expect.all
        [ SketchPlane3d.originPoint
            >> point3d (SketchPlane3d.originPoint first)
        , SketchPlane3d.xDirection
            >> direction3d (SketchPlane3d.xDirection first)
        , SketchPlane3d.yDirection
            >> direction3d (SketchPlane3d.yDirection first)
        ]


validSketchPlane3d : SketchPlane3d units coordinates defines -> Expectation
validSketchPlane3d =
    Expect.all
        [ SketchPlane3d.xDirection >> validDirection3d
        , SketchPlane3d.yDirection >> validDirection3d
        , \sketchPlane ->
            let
                xDirection =
                    SketchPlane3d.xDirection sketchPlane

                yDirection =
                    SketchPlane3d.yDirection sketchPlane

                parallelComponent =
                    Direction3d.componentIn xDirection yDirection
            in
            if abs parallelComponent <= defaultTolerance then
                Expect.pass

            else
                Expect.fail
                    ("Expected perpendicular basis directions, got "
                        ++ Debug.toString xDirection
                        ++ ", "
                        ++ Debug.toString yDirection
                    )
        ]


lineSegment2d : LineSegment2d units coordinates -> LineSegment2d units coordinates -> Expectation
lineSegment2d first =
    Expect.all
        [ LineSegment2d.startPoint
            >> point2d (LineSegment2d.startPoint first)
        , LineSegment2d.endPoint
            >> point2d (LineSegment2d.endPoint first)
        ]


lineSegment2dWithin : Quantity Float units -> LineSegment2d units coordinates -> LineSegment2d units coordinates -> Expectation
lineSegment2dWithin tolerance first =
    Expect.all
        [ LineSegment2d.startPoint
            >> point2dWithin tolerance (LineSegment2d.startPoint first)
        , LineSegment2d.endPoint
            >> point2dWithin tolerance (LineSegment2d.endPoint first)
        ]


lineSegment3d : LineSegment3d units coordinates -> LineSegment3d units coordinates -> Expectation
lineSegment3d first =
    Expect.all
        [ LineSegment3d.startPoint
            >> point3d (LineSegment3d.startPoint first)
        , LineSegment3d.endPoint
            >> point3d (LineSegment3d.endPoint first)
        ]


lineSegment3dWithin : Quantity Float units -> LineSegment3d units coordinates -> LineSegment3d units coordinates -> Expectation
lineSegment3dWithin tolerance first =
    Expect.all
        [ LineSegment3d.startPoint
            >> point3dWithin tolerance (LineSegment3d.startPoint first)
        , LineSegment3d.endPoint
            >> point3dWithin tolerance (LineSegment3d.endPoint first)
        ]


triangle2dBy : (Point2d units coordinates -> Point2d units coordinates -> Expectation) -> Triangle2d units coordinates -> Triangle2d units coordinates -> Expectation
triangle2dBy equalTo first =
    let
        vertex1 triangle =
            let
                ( vertex, _, _ ) =
                    Triangle2d.vertices triangle
            in
            vertex

        vertex2 triangle =
            let
                ( _, vertex, _ ) =
                    Triangle2d.vertices triangle
            in
            vertex

        vertex3 triangle =
            let
                ( _, _, vertex ) =
                    Triangle2d.vertices triangle
            in
            vertex
    in
    Expect.all
        [ vertex1 >> equalTo (vertex1 first)
        , vertex2 >> equalTo (vertex2 first)
        , vertex3 >> equalTo (vertex3 first)
        ]


triangle2d : Triangle2d units coordinates -> Triangle2d units coordinates -> Expectation
triangle2d =
    triangle2dBy point2d


triangle2dWithin : Quantity Float units -> Triangle2d units coordinates -> Triangle2d units coordinates -> Expectation
triangle2dWithin tolerance =
    triangle2dBy (point2dWithin tolerance)


triangle3dBy : (Point3d units coordinates -> Point3d units coordinates -> Expectation) -> Triangle3d units coordinates -> Triangle3d units coordinates -> Expectation
triangle3dBy equalTo first =
    let
        vertex1 triangle =
            let
                ( vertex, _, _ ) =
                    Triangle3d.vertices triangle
            in
            vertex

        vertex2 triangle =
            let
                ( _, vertex, _ ) =
                    Triangle3d.vertices triangle
            in
            vertex

        vertex3 triangle =
            let
                ( _, _, vertex ) =
                    Triangle3d.vertices triangle
            in
            vertex
    in
    Expect.all
        [ vertex1 >> equalTo (vertex1 first)
        , vertex2 >> equalTo (vertex2 first)
        , vertex3 >> equalTo (vertex3 first)
        ]


triangle3d : Triangle3d units coordinates -> Triangle3d units coordinates -> Expectation
triangle3d =
    triangle3dBy point3d


triangle3dWithin : Quantity Float units -> Triangle3d units coordinates -> Triangle3d units coordinates -> Expectation
triangle3dWithin tolerance =
    triangle3dBy (point3dWithin tolerance)


boundingBox2dBy : (Quantity Float units -> Quantity Float units -> Expectation) -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Expectation
boundingBox2dBy equalTo first =
    Expect.all
        [ BoundingBox2d.minX >> equalTo (BoundingBox2d.minX first)
        , BoundingBox2d.maxX >> equalTo (BoundingBox2d.maxX first)
        , BoundingBox2d.minY >> equalTo (BoundingBox2d.minY first)
        , BoundingBox2d.maxY >> equalTo (BoundingBox2d.maxY first)
        ]


boundingBox2d : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Expectation
boundingBox2d =
    boundingBox2dBy approximately


boundingBox2dWithin : Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Expectation
boundingBox2dWithin tolerance =
    boundingBox2dBy (quantityWithin tolerance)


point2dContainedIn : BoundingBox2d units coordinates -> Point2d units coordinates -> Expectation
point2dContainedIn box point =
    let
        extrema =
            BoundingBox2d.extrema box

        ( width, height ) =
            BoundingBox2d.dimensions box

        xOffset =
            absoluteToleranceFor width

        yOffset =
            absoluteToleranceFor height

        tolerantBox =
            BoundingBox2d.fromExtrema
                { minX = extrema.minX |> Quantity.minus xOffset
                , minY = extrema.minY |> Quantity.minus yOffset
                , maxX = extrema.maxX |> Quantity.plus xOffset
                , maxY = extrema.maxY |> Quantity.plus yOffset
                }
    in
    BoundingBox2d.contains point tolerantBox
        |> Expect.true
            ("Expected point "
                ++ Debug.toString point
                ++ " to be within bounding box "
                ++ Debug.toString box
                ++ "."
            )


boundingBox3dBy : (Quantity Float units -> Quantity Float units -> Expectation) -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Expectation
boundingBox3dBy equalTo first =
    Expect.all
        [ BoundingBox3d.minX >> equalTo (BoundingBox3d.minX first)
        , BoundingBox3d.maxX >> equalTo (BoundingBox3d.maxX first)
        , BoundingBox3d.minY >> equalTo (BoundingBox3d.minY first)
        , BoundingBox3d.maxY >> equalTo (BoundingBox3d.maxY first)
        , BoundingBox3d.minZ >> equalTo (BoundingBox3d.minZ first)
        , BoundingBox3d.maxZ >> equalTo (BoundingBox3d.maxZ first)
        ]


boundingBox3d : BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Expectation
boundingBox3d =
    boundingBox3dBy approximately


boundingBox3dWithin : Quantity Float units -> BoundingBox3d units coordinates -> BoundingBox3d units coordinates -> Expectation
boundingBox3dWithin tolerance =
    boundingBox3dBy (quantityWithin tolerance)


point3dContainedIn : BoundingBox3d units coordinates -> Point3d units coordinates -> Expectation
point3dContainedIn box point =
    let
        extrema =
            BoundingBox3d.extrema box

        ( width, height, depth ) =
            BoundingBox3d.dimensions box

        xOffset =
            absoluteToleranceFor width

        yOffset =
            absoluteToleranceFor height

        zOffset =
            absoluteToleranceFor depth

        tolerantBox =
            BoundingBox3d.fromExtrema
                { minX = extrema.minX |> Quantity.minus xOffset
                , minY = extrema.minY |> Quantity.minus yOffset
                , minZ = extrema.minZ |> Quantity.minus zOffset
                , maxX = extrema.maxX |> Quantity.plus xOffset
                , maxY = extrema.maxY |> Quantity.plus yOffset
                , maxZ = extrema.maxZ |> Quantity.plus zOffset
                }
    in
    BoundingBox3d.contains point tolerantBox
        |> Expect.true
            ("Expected point "
                ++ Debug.toString point
                ++ " to be within bounding box "
                ++ Debug.toString box
                ++ "."
            )


polyline2dBy : (Point2d units coordinates -> Point2d units coordinates -> Expectation) -> Polyline2d units coordinates -> Polyline2d units coordinates -> Expectation
polyline2dBy equalTo first =
    Expect.all
        [ Polyline2d.vertices
            >> List.length
            >> Expect.equal (List.length (Polyline2d.vertices first))
        , \second ->
            Expect.all
                (List.map2
                    (\firstVertex secondVertex ->
                        \_ -> secondVertex |> equalTo firstVertex
                    )
                    (Polyline2d.vertices first)
                    (Polyline2d.vertices second)
                )
                second
        ]


polyline2d : Polyline2d units coordinates -> Polyline2d units coordinates -> Expectation
polyline2d =
    polyline2dBy point2d


polyline2dWithin : Quantity Float units -> Polyline2d units coordinates -> Polyline2d units coordinates -> Expectation
polyline2dWithin tolerance =
    polyline2dBy (point2dWithin tolerance)


polyline3dBy : (Point3d units coordinates -> Point3d units coordinates -> Expectation) -> Polyline3d units coordinates -> Polyline3d units coordinates -> Expectation
polyline3dBy equalPoints first =
    Expect.all
        [ Polyline3d.vertices
            >> List.length
            >> Expect.equal (List.length (Polyline3d.vertices first))
        , \second ->
            Expect.all
                (List.map2
                    (\firstVertex secondVertex ->
                        \_ -> equalPoints firstVertex secondVertex
                    )
                    (Polyline3d.vertices first)
                    (Polyline3d.vertices second)
                )
                second
        ]


polyline3d : Polyline3d units coordinates -> Polyline3d units coordinates -> Expectation
polyline3d =
    polyline3dBy point3d


polyline3dWithin : Quantity Float units -> Polyline3d units coordinates -> Polyline3d units coordinates -> Expectation
polyline3dWithin tolerance =
    polyline3dBy (point3dWithin tolerance)


polygon2dBy : (Point2d units coordinates -> Point2d units coordinates -> Expectation) -> Polygon2d units coordinates -> Polygon2d units coordinates -> Expectation
polygon2dBy equalPoints first =
    Expect.all
        [ Polygon2d.vertices
            >> List.length
            >> Expect.equal (List.length (Polygon2d.vertices first))
        , \second ->
            Expect.all
                (List.map2
                    (\firstVertex secondVertex ->
                        \_ -> equalPoints firstVertex secondVertex
                    )
                    (Polygon2d.vertices first)
                    (Polygon2d.vertices second)
                )
                second
        ]


polygon2d : Polygon2d units coordinates -> Polygon2d units coordinates -> Expectation
polygon2d =
    polygon2dBy point2d


polygon2dWithin : Quantity Float units -> Polygon2d units coordinates -> Polygon2d units coordinates -> Expectation
polygon2dWithin tolerance =
    polygon2dBy (point2dWithin tolerance)


circle2d : Circle2d units coordinates -> Circle2d units coordinates -> Expectation
circle2d first =
    Expect.all
        [ Circle2d.centerPoint >> point2d (Circle2d.centerPoint first)
        , Circle2d.radius >> approximately (Circle2d.radius first)
        ]


circle3d : Circle3d units coordinates -> Circle3d units coordinates -> Expectation
circle3d first =
    Expect.all
        [ Circle3d.centerPoint >> point3d (Circle3d.centerPoint first)
        , Circle3d.axialDirection >> direction3d (Circle3d.axialDirection first)
        , Circle3d.radius >> approximately (Circle3d.radius first)
        ]


sphere3d : Sphere3d units coordinates -> Sphere3d units coordinates -> Expectation
sphere3d first =
    Expect.all
        [ Sphere3d.centerPoint >> point3d (Sphere3d.centerPoint first)
        , Sphere3d.radius >> approximately (Sphere3d.radius first)
        ]


cylinder3d : Cylinder3d units coordinates -> Cylinder3d units coordinates -> Expectation
cylinder3d first =
    Expect.all
        [ Cylinder3d.axis >> axis3d (Cylinder3d.axis first)
        , Cylinder3d.radius >> approximately (Cylinder3d.radius first)
        , Cylinder3d.length >> approximately (Cylinder3d.length first)
        ]


arc2d : Arc2d units coordinates -> Arc2d units coordinates -> Expectation
arc2d first =
    Expect.all
        [ Arc2d.startPoint >> point2d (Arc2d.startPoint first)
        , Arc2d.endPoint >> point2d (Arc2d.endPoint first)
        , Arc2d.sweptAngle >> approximately (Arc2d.sweptAngle first)
        ]


arc3d : Arc3d units coordinates -> Arc3d units coordinates -> Expectation
arc3d first =
    Expect.all
        [ Arc3d.startPoint >> point3d (Arc3d.startPoint first)
        , Arc3d.endPoint >> point3d (Arc3d.endPoint first)
        , Arc3d.sweptAngle >> approximately (Arc3d.sweptAngle first)
        , Arc3d.axialDirection >> direction3d (Arc3d.axialDirection first)
        ]


quadraticSpline2d : QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates -> Expectation
quadraticSpline2d first =
    Expect.all
        [ QuadraticSpline2d.firstControlPoint
            >> point2d (QuadraticSpline2d.firstControlPoint first)
        , QuadraticSpline2d.secondControlPoint
            >> point2d (QuadraticSpline2d.secondControlPoint first)
        , QuadraticSpline2d.thirdControlPoint
            >> point2d (QuadraticSpline2d.thirdControlPoint first)
        ]


quadraticSpline3d : QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates -> Expectation
quadraticSpline3d first =
    Expect.all
        [ QuadraticSpline3d.firstControlPoint
            >> point3d (QuadraticSpline3d.firstControlPoint first)
        , QuadraticSpline3d.secondControlPoint
            >> point3d (QuadraticSpline3d.secondControlPoint first)
        , QuadraticSpline3d.thirdControlPoint
            >> point3d (QuadraticSpline3d.thirdControlPoint first)
        ]


cubicSpline2d : CubicSpline2d units coordinates -> CubicSpline2d units coordinates -> Expectation
cubicSpline2d first =
    Expect.all
        [ CubicSpline2d.firstControlPoint
            >> point2d (CubicSpline2d.firstControlPoint first)
        , CubicSpline2d.secondControlPoint
            >> point2d (CubicSpline2d.secondControlPoint first)
        , CubicSpline2d.thirdControlPoint
            >> point2d (CubicSpline2d.thirdControlPoint first)
        , CubicSpline2d.fourthControlPoint
            >> point2d (CubicSpline2d.fourthControlPoint first)
        ]


cubicSpline3d : CubicSpline3d units coordinates -> CubicSpline3d units coordinates -> Expectation
cubicSpline3d first =
    Expect.all
        [ CubicSpline3d.firstControlPoint
            >> point3d (CubicSpline3d.firstControlPoint first)
        , CubicSpline3d.secondControlPoint
            >> point3d (CubicSpline3d.secondControlPoint first)
        , CubicSpline3d.thirdControlPoint
            >> point3d (CubicSpline3d.thirdControlPoint first)
        , CubicSpline3d.fourthControlPoint
            >> point3d (CubicSpline3d.fourthControlPoint first)
        ]


validBoundingBox2d : BoundingBox2d units coordinates -> Expectation
validBoundingBox2d boundingBox =
    let
        extrema =
            BoundingBox2d.extrema boundingBox

        { minX, maxX, minY, maxY } =
            extrema
    in
    if not (minX |> Quantity.lessThanOrEqualTo maxX) then
        Expect.fail ("Expected bounding box with extrema " ++ Debug.toString extrema ++ " to have minX <= maxX")

    else if not (minY |> Quantity.lessThanOrEqualTo maxY) then
        Expect.fail ("Expected bounding box with extrema " ++ Debug.toString extrema ++ " to have minY <= maxY")

    else
        Expect.pass


validBoundingBox3d : BoundingBox3d units coordinates -> Expectation
validBoundingBox3d boundingBox =
    let
        extrema =
            BoundingBox3d.extrema boundingBox

        { minX, maxX, minY, maxY, minZ, maxZ } =
            extrema
    in
    if not (minX |> Quantity.lessThanOrEqualTo maxX) then
        Expect.fail ("Expected bounding box with extrema " ++ Debug.toString extrema ++ " to have minX <= maxX")

    else if not (minY |> Quantity.lessThanOrEqualTo maxY) then
        Expect.fail ("Expected bounding box with extrema " ++ Debug.toString extrema ++ " to have minY <= maxY")

    else if not (minZ |> Quantity.lessThanOrEqualTo maxZ) then
        Expect.fail ("Expected bounding box with extrema " ++ Debug.toString extrema ++ " to have minZ <= maxZ")

    else
        Expect.pass
