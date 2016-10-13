{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Expect
    exposing
        ( Comparison
        , expect
        , approximately
        , within
        , angle
        , angleWithin
        , vector2d
        , vector2dWithin
        , vector3d
        , vector3dWithin
        , direction2d
        , direction2dWithin
        , direction3d
        , direction3dWithin
        , point2d
        , point2dWithin
        , point3d
        , point3dWithin
        , axis2d
        , axis3d
        , plane3d
        , frame2d
        , frame3d
        , sketchPlane3d
        , lineSegment2d
        , lineSegment2dWithin
        , lineSegment3d
        , lineSegment3dWithin
        , triangle2d
        , triangle2dWithin
        , triangle3d
        , triangle3dWithin
        , boundingBox2d
        , boundingBox2dWithin
        , boundingBox3d
        , boundingBox3dWithin
        )

import Expect exposing (Expectation)
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
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.BoundingBox3d as BoundingBox3d


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


equalWithin : Float -> Float -> Float -> Bool
equalWithin tolerance firstValue secondValue =
    abs (firstValue - secondValue) <= tolerance


by : Comparison b -> (a -> b) -> Comparison a
by comparison property first second =
    comparison (property first) (property second)


allOf : List (Comparison a) -> Comparison a
allOf comparisons first second =
    List.all (\comparison -> comparison first second) comparisons


defaultTolerance : Float
defaultTolerance =
    1.0e-12


approximately : Float -> Float -> Expectation
approximately =
    within defaultTolerance


within : Float -> Float -> Float -> Expectation
within tolerance =
    expect (equalWithin tolerance)


angle : Float -> Float -> Expectation
angle =
    angleWithin defaultTolerance


angleWithin : Float -> Float -> Float -> Expectation
angleWithin tolerance =
    let
        comparison first second =
            let
                difference =
                    second - first
            in
                abs (atan2 (sin difference) (cos difference)) <= tolerance
    in
        expect comparison


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


direction2d : Direction2d -> Direction2d -> Expectation
direction2d =
    direction2dWithin defaultTolerance


direction2dWithin : Float -> Direction2d -> Direction2d -> Expectation
direction2dWithin tolerance =
    expect (Direction2d.equalWithin tolerance)


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


frame2d : Frame2d -> Frame2d -> Expectation
frame2d =
    expect
        (allOf
            [ by (Point2d.equalWithin defaultTolerance) Frame2d.originPoint
            , by (Direction2d.equalWithin defaultTolerance) Frame2d.xDirection
            , by (Direction2d.equalWithin defaultTolerance) Frame2d.yDirection
            ]
        )


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
            [ by (equalWithin tolerance) BoundingBox2d.minX
            , by (equalWithin tolerance) BoundingBox2d.maxX
            , by (equalWithin tolerance) BoundingBox2d.minY
            , by (equalWithin tolerance) BoundingBox2d.maxY
            ]
        )


boundingBox3d : BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3d =
    boundingBox3dWithin defaultTolerance


boundingBox3dWithin : Float -> BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3dWithin tolerance =
    expect
        (allOf
            [ by (equalWithin tolerance) BoundingBox3d.minX
            , by (equalWithin tolerance) BoundingBox3d.maxX
            , by (equalWithin tolerance) BoundingBox3d.minY
            , by (equalWithin tolerance) BoundingBox3d.maxY
            , by (equalWithin tolerance) BoundingBox3d.minZ
            , by (equalWithin tolerance) BoundingBox3d.maxZ
            ]
        )
