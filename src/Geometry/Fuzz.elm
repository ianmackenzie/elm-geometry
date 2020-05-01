--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Fuzz exposing
    ( scale, parameterValue
    , length, positiveLength, angle, quantityRange
    , point2d, point3d, vector2d, vector3d, direction2d, direction3d, boundingBox2d, boundingBox3d
    , axis2d, axis3d, frame2d, frame3d, plane3d, sketchPlane3d
    , lineSegment2d, lineSegment3d, triangle2d, triangle3d, rectangle2d, block3d, polyline2d, polyline3d, polygon2d
    , arc2d, arc3d, circle2d, circle3d, cubicSpline2d, cubicSpline3d, cylinder3d, cone3d, ellipse2d, ellipticalArc2d, quadraticSpline2d, quadraticSpline3d, sphere3d
    )

{-| A collection of [`Fuzzer`](https://package.elm-lang.org/packages/elm-explorations/test/latest/Fuzz)s
for [`elm-geometry`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/)
values.

These fuzzers generally generate values in the range -10 to +10 meters. For
example, `point2d` will generate a point with coordinate values in this range
and `sphere3d` will generate a sphere with a center point that has coordinates
in this range, and a radius also in this range (although note that this means
points on the _surface_ of the sphere may have coordinates outside this range).

This module has been designed so that in most cases you can use

    import Geometry.Fuzz as Fuzz

to 'merge' it with the `Fuzz` module from `elm-explorations/test`, without
running into any naming conflicts.


# `Float` values

@docs scale, parameterValue


# `Quantity` values

@docs length, positiveLength, angle, quantityRange


# Primitives

@docs point2d, point3d, vector2d, vector3d, direction2d, direction3d, boundingBox2d, boundingBox3d


# Datums

@docs axis2d, axis3d, frame2d, frame3d, plane3d, sketchPlane3d


# Simple geometry

@docs lineSegment2d, lineSegment3d, triangle2d, triangle3d, rectangle2d, block3d, polyline2d, polyline3d, polygon2d


# Complex geometry

@docs arc2d, arc3d, circle2d, circle3d, cubicSpline2d, cubicSpline3d, cylinder3d, cone3d, ellipse2d, ellipticalArc2d, quadraticSpline2d, quadraticSpline3d, sphere3d

-}

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import Cone3d exposing (Cone3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d exposing (Cylinder3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Fuzz exposing (Fuzzer)
import Length exposing (Length, Meters, meters)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Polygon2d.Random as Random
import Polyline2d exposing (Polyline2d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Shrink
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| Generate a random `Length` in the range -10 to +10 meters.
-}
length : Fuzzer Length
length =
    Fuzz.map Length.meters (Fuzz.floatRange -10 10)


{-| Generate a random `Float` scaling factor in the range -10 to +10.
-}
scale : Fuzzer Float
scale =
    Fuzz.floatRange -10 10


{-| Generate a random angle in the range -360 to +360 degrees.
-}
angle : Fuzzer Angle
angle =
    Fuzz.map Angle.radians (Fuzz.floatRange (-2 * pi) (2 * pi))


{-| Generate a random positive length in the range 0 to 10 meters.
-}
positiveLength : Fuzzer Length
positiveLength =
    Fuzz.map Quantity.abs length


{-| Generate a random `Float` value in the range 0 to 1.
-}
parameterValue : Fuzzer Float
parameterValue =
    Fuzz.floatRange 0 1


{-| Generate a random `Quantity` between the two given values.
-}
quantityRange : Quantity Float units -> Quantity Float units -> Fuzzer (Quantity Float units)
quantityRange start end =
    Fuzz.map (Quantity.interpolateFrom start end) parameterValue


{-| Generate a random `Vector2d`.
-}
vector2d : Fuzzer (Vector2d Meters coordinates)
vector2d =
    Fuzz.map2 Vector2d.xy length length


{-| Generate a random `Vector3d`.
-}
vector3d : Fuzzer (Vector3d Meters coordinates)
vector3d =
    Fuzz.map3 Vector3d.xyz length length length


{-| Generate a random `Direction2d`.
-}
direction2d : Fuzzer (Direction2d coordinates)
direction2d =
    Fuzz.map Direction2d.fromAngle angle


{-| Generate a random `Direction3d`.
-}
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


{-| Generate a random `Point2d`.
-}
point2d : Fuzzer (Point2d Meters coordinates)
point2d =
    Fuzz.map2 Point2d.xy length length


{-| Generate a random `Point3d`.
-}
point3d : Fuzzer (Point3d Meters coordinates)
point3d =
    Fuzz.map3 Point3d.xyz length length length


{-| Generate a random `Axis2d`.
-}
axis2d : Fuzzer (Axis2d Meters coordinates)
axis2d =
    Fuzz.map2 Axis2d.through point2d direction2d


{-| Generate a random `Axis3d`.
-}
axis3d : Fuzzer (Axis3d Meters coordinates)
axis3d =
    Fuzz.map2 Axis3d.through point3d direction3d


{-| Generate a random `Plane3d`.
-}
plane3d : Fuzzer (Plane3d Meters coordinates)
plane3d =
    Fuzz.map2 Plane3d.through point3d direction3d


{-| Generate a random `Frame2d`. Note that the generated frame may be [either
left-handed or right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).
-}
frame2d : Fuzzer (Frame2d Meters coordinates { defines : localCoordinates })
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


{-| Generate a random `Frame3d`. Note that the generated frame may be [either
left-handed or right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).
-}
frame3d : Fuzzer (Frame3d Meters coordinates { defines : localCoordinates })
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


{-| Generate a random `SketchPlane3d`.
-}
sketchPlane3d : Fuzzer (SketchPlane3d Meters coordinates { defines : sketchCoordinates })
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


{-| Generate a random `LineSegment2d`.
-}
lineSegment2d : Fuzzer (LineSegment2d Meters coordinates)
lineSegment2d =
    Fuzz.map2 LineSegment2d.from point2d point2d


{-| Generate a random `LineSegment3d`.
-}
lineSegment3d : Fuzzer (LineSegment3d Meters coordinates)
lineSegment3d =
    Fuzz.map2 LineSegment3d.from point3d point3d


{-| Generate a random `Triangle2d`. Note that the triangle's vertices may be in
either clockwise or counterclockwise order.
-}
triangle2d : Fuzzer (Triangle2d Meters coordinates)
triangle2d =
    Fuzz.map3 Triangle2d.from point2d point2d point2d


{-| Generate a random `Triangle3d`.
-}
triangle3d : Fuzzer (Triangle3d Meters coordinates)
triangle3d =
    Fuzz.map3 Triangle3d.from point3d point3d point3d


{-| Generate a random `BoundingBox2d`.
-}
boundingBox2d : Fuzzer (BoundingBox2d Meters coordinates)
boundingBox2d =
    Fuzz.map2 BoundingBox2d.from point2d point2d


{-| Generate a random `BoundingBox3d`.
-}
boundingBox3d : Fuzzer (BoundingBox3d Meters coordinates)
boundingBox3d =
    Fuzz.map2 BoundingBox3d.from point3d point3d


{-| Generate a random `Polyline2d`.
-}
polyline2d : Fuzzer (Polyline2d Meters coordinates)
polyline2d =
    Fuzz.map Polyline2d.fromVertices (Fuzz.list point2d)


{-| Generate a random `Polyline3d`.
-}
polyline3d : Fuzzer (Polyline3d Meters coordinates)
polyline3d =
    Fuzz.map Polyline3d.fromVertices (Fuzz.list point3d)


{-| Generate a random `Polygon2d`. The polygon will be one of a few general
forms, but with randomized vertex positions:

  - Square
  - Square with hole
  - Square with two holes
  - Square with two interlocking holes
  - Circle with hole
  - L shape

-}
polygon2d : Fuzzer (Polygon2d Meters coordinates)
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


{-| Generate a random `Circle2d`.
-}
circle2d : Fuzzer (Circle2d Meters coordinates)
circle2d =
    Fuzz.map2 Circle2d.withRadius positiveLength point2d


{-| Generate a random `Circle3d`.
-}
circle3d : Fuzzer (Circle3d Meters coordinates)
circle3d =
    Fuzz.map3 Circle3d.withRadius positiveLength direction3d point3d


{-| Generate a random `Sphere3d`.
-}
sphere3d : Fuzzer (Sphere3d Meters coordinates)
sphere3d =
    Fuzz.map2 Sphere3d.withRadius positiveLength point3d


{-| Generate a random `Arc2d`.
-}
arc2d : Fuzzer (Arc2d Meters coordinates)
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


{-| Generate a random `Arc3d`.
-}
arc3d : Fuzzer (Arc3d Meters coordinates)
arc3d =
    Fuzz.map2 Arc3d.on sketchPlane3d arc2d


{-| Generate a random `QuadraticSpline2d`.
-}
quadraticSpline2d : Fuzzer (QuadraticSpline2d Meters coordinates)
quadraticSpline2d =
    Fuzz.map3 QuadraticSpline2d.fromControlPoints point2d point2d point2d


{-| Generate a random `QuadraticSpline3d`.
-}
quadraticSpline3d : Fuzzer (QuadraticSpline3d Meters coordinates)
quadraticSpline3d =
    Fuzz.map3 QuadraticSpline3d.fromControlPoints point3d point3d point3d


{-| Generate a random `CubicSpline2d`.
-}
cubicSpline2d : Fuzzer (CubicSpline2d Meters coordinates)
cubicSpline2d =
    Fuzz.map4 CubicSpline2d.fromControlPoints point2d point2d point2d point2d


{-| Generate a random `CubicSpline3d`.
-}
cubicSpline3d : Fuzzer (CubicSpline3d Meters coordinates)
cubicSpline3d =
    Fuzz.map4 CubicSpline3d.fromControlPoints point3d point3d point3d point3d


{-| Generate a random `Ellipse2d`.
-}
ellipse2d : Fuzzer (Ellipse2d Meters coordinates)
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


{-| Generate a random `EllipticalArc2d`.
-}
ellipticalArc2d : Fuzzer (EllipticalArc2d Meters coordinates)
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


{-| Generate a random `Rectangle2d`.
-}
rectangle2d : Fuzzer (Rectangle2d Meters coordinates)
rectangle2d =
    let
        rectangle axes width height =
            Rectangle2d.centeredOn axes ( width, height )
    in
    Fuzz.map3 rectangle frame2d positiveLength positiveLength


{-| Generate a random `Block3d`.
-}
block3d : Fuzzer (Block3d Meters coordinates)
block3d =
    let
        block axes xDim yDim zDim =
            Block3d.centeredOn axes ( xDim, yDim, zDim )
    in
    Fuzz.map4 block frame3d positiveLength positiveLength positiveLength


{-| Generate a random `Cylinder3d`.
-}
cylinder3d : Fuzzer (Cylinder3d Meters coordinates)
cylinder3d =
    let
        cylinder centerPoint direction cylinderLength cylinderRadius =
            Cylinder3d.centeredOn centerPoint direction <|
                { length = cylinderLength
                , radius = cylinderRadius
                }
    in
    Fuzz.map4 cylinder point3d direction3d positiveLength positiveLength


{-| Generate a random `Cone3d`.
-}
cone3d : Fuzzer (Cone3d Meters coordinates)
cone3d =
    let
        cone basePoint direction coneLength coneRadius =
            Cone3d.startingAt basePoint direction <|
                { length = coneLength
                , radius = coneRadius
                }
    in
    Fuzz.map4 cone point3d direction3d positiveLength positiveLength
