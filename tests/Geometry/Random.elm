module Geometry.Random exposing
    ( angle
    , arc2d
    , arc3d
    , axis2d
    , axis3d
    , cubicSpline2d
    , cubicSpline3d
    , direction2d
    , direction3d
    , ellipse2d
    , ellipticalArc2d
    , ellipticalArc3d
    , frame2d
    , frame3d
    , length
    , lineSegment2d
    , lineSegment3d
    , parameterValue
    , plane3d
    , point2d
    , point3d
    , positiveLength
    , quadraticSpline2d
    , quadraticSpline3d
    , rationalCubicSpline2d
    , rationalCubicSpline3d
    , rationalQuadraticSpline2d
    , rationalQuadraticSpline3d
    , scale
    , sketchPlane3d
    , spline2d
    , spline3d
    , triangle2d
    , triangle3d
    , unitlessInterval
    , unitlessQuantity
    , vector2d
    , vector3d
    , vectorBoundingBox2d
    , vectorBoundingBox3d
    )

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import EllipticalArc3d exposing (EllipticalArc3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity, Unitless)
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import RationalCubicSpline2d exposing (RationalCubicSpline2d)
import RationalCubicSpline3d exposing (RationalCubicSpline3d)
import RationalQuadraticSpline2d exposing (RationalQuadraticSpline2d)
import RationalQuadraticSpline3d exposing (RationalQuadraticSpline3d)
import SketchPlane3d exposing (SketchPlane3d)
import Spline2d exposing (Spline2d)
import Spline3d exposing (Spline3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)
import VectorBoundingBox2d exposing (VectorBoundingBox2d)
import VectorBoundingBox3d exposing (VectorBoundingBox3d)


parameterValue : Generator Float
parameterValue =
    Random.weighted
        ( 1, Random.float 0 1 )
        [ ( 1, Random.constant 0 )
        , ( 1, Random.constant 1 )
        ]
        |> Random.andThen identity


scale : Generator Float
scale =
    Random.weighted
        ( 3, Random.float -10 10 )
        [ ( 1, Random.constant 0 ) ]
        |> Random.andThen identity


length : Generator Length
length =
    Random.map Length.meters (Random.float -10 10)


unitlessQuantity : Generator (Quantity Float Unitless)
unitlessQuantity =
    Random.map Quantity.float scale


unitlessInterval : Generator (Interval Float Unitless)
unitlessInterval =
    Random.map2 Interval.from unitlessQuantity unitlessQuantity


positiveLength : Generator Length
positiveLength =
    Random.map Quantity.abs length


angle : Generator Angle
angle =
    Random.map Angle.radians (Random.float (-2 * pi) (2 * pi))


direction2d : Generator (Direction2d coordinates)
direction2d =
    Random.map Direction2d.fromAngle angle


direction3d : Generator (Direction3d coordinates)
direction3d =
    let
        phiGenerator =
            Random.map (acos >> Angle.radians) (Random.float -1 1)

        thetaGenerator =
            Random.map Angle.radians (Random.float -pi pi)

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
    Random.map2 toDirection phiGenerator thetaGenerator


vector2d : Generator (Vector2d Meters coordinates)
vector2d =
    Random.map2 Vector2d.xy length length


vector3d : Generator (Vector3d Meters coordinates)
vector3d =
    Random.map3 Vector3d.xyz length length length


vectorBoundingBox2d : Generator (VectorBoundingBox2d Meters coordinates)
vectorBoundingBox2d =
    Random.map2 VectorBoundingBox2d.hull2 vector2d vector2d


vectorBoundingBox3d : Generator (VectorBoundingBox3d Meters coordinates)
vectorBoundingBox3d =
    Random.map2 VectorBoundingBox3d.hull2 vector3d vector3d


point2d : Generator (Point2d Meters coordinates)
point2d =
    Random.map2 Point2d.xy length length


point3d : Generator (Point3d Meters coordinates)
point3d =
    Random.map3 Point3d.xyz length length length


axis2d : Generator (Axis2d Meters coordinates)
axis2d =
    Random.map2 Axis2d.through point2d direction2d


axis3d : Generator (Axis3d Meters coordinates)
axis3d =
    Random.map2 Axis3d.through point3d direction3d


bool : Generator Bool
bool =
    Random.uniform True [ False ]


frame2d : Generator (Frame2d Meters coordinates { defines : localCoordinates })
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
    Random.map3 frame point2d direction2d bool


frame3d : Generator (Frame3d Meters coordinates { defines : localCoordinates })
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
    Random.map4 frame point3d direction3d bool bool


lineSegment2d : Generator (LineSegment2d Meters coordinates)
lineSegment2d =
    Random.map2 LineSegment2d.from point2d point2d


lineSegment3d : Generator (LineSegment3d Meters coordinates)
lineSegment3d =
    Random.map2 LineSegment3d.from point3d point3d


triangle2d : Generator (Triangle2d Meters coordinates)
triangle2d =
    Random.map3 Triangle2d.from point2d point2d point2d


triangle3d : Generator (Triangle3d Meters coordinates)
triangle3d =
    Random.map3 Triangle3d.from point3d point3d point3d


quadraticSpline2d : Generator (QuadraticSpline2d Meters coordinates)
quadraticSpline2d =
    Random.map3 QuadraticSpline2d.fromControlPoints point2d point2d point2d


quadraticSpline3d : Generator (QuadraticSpline3d Meters coordinates)
quadraticSpline3d =
    Random.map3 QuadraticSpline3d.fromControlPoints point3d point3d point3d


cubicSpline2d : Generator (CubicSpline2d Meters coordinates)
cubicSpline2d =
    Random.map4 CubicSpline2d.fromControlPoints point2d point2d point2d point2d


weightedControlPoint2d : Generator ( Point2d Meters coordinates, Float )
weightedControlPoint2d =
    Random.map2 Tuple.pair point2d (Random.float 1 10)


rationalQuadraticSpline2d : Generator (RationalQuadraticSpline2d Meters coordinates)
rationalQuadraticSpline2d =
    Random.map3 RationalQuadraticSpline2d.fromControlPoints
        weightedControlPoint2d
        weightedControlPoint2d
        weightedControlPoint2d


rationalCubicSpline2d : Generator (RationalCubicSpline2d Meters coordinates)
rationalCubicSpline2d =
    Random.map4 RationalCubicSpline2d.fromControlPoints
        weightedControlPoint2d
        weightedControlPoint2d
        weightedControlPoint2d
        weightedControlPoint2d


weightedControlPoint3d : Generator ( Point3d Meters coordinates, Float )
weightedControlPoint3d =
    Random.map2 Tuple.pair point3d (Random.float 1 10)


rationalQuadraticSpline3d : Generator (RationalQuadraticSpline3d Meters coordinates)
rationalQuadraticSpline3d =
    Random.map3 RationalQuadraticSpline3d.fromControlPoints
        weightedControlPoint3d
        weightedControlPoint3d
        weightedControlPoint3d


rationalCubicSpline3d : Generator (RationalCubicSpline3d Meters coordinates)
rationalCubicSpline3d =
    Random.map4 RationalCubicSpline3d.fromControlPoints
        weightedControlPoint3d
        weightedControlPoint3d
        weightedControlPoint3d
        weightedControlPoint3d


cubicSpline3d : Generator (CubicSpline3d Meters coordinates)
cubicSpline3d =
    Random.map4 CubicSpline3d.fromControlPoints point3d point3d point3d point3d


spline3d : Generator (Spline3d Meters coordinates)
spline3d =
    Random.int 0 12
        |> Random.andThen
            (\degree ->
                Random.map2 Spline3d.fromControlPoints
                    point3d
                    (Random.list degree point3d)
            )


spline2d : Generator (Spline2d Meters coordinates)
spline2d =
    Random.int 0 12
        |> Random.andThen
            (\degree ->
                Random.map2 Spline2d.fromControlPoints
                    point2d
                    (Random.list degree point2d)
            )


plane3d : Generator (Plane3d Meters coordinates)
plane3d =
    Random.map2 Plane3d.through point3d direction3d


sketchPlane3d : Generator (SketchPlane3d Meters coordinates { defines : sketchCoordinates })
sketchPlane3d =
    let
        sketchPlane plane rotationAngle =
            SketchPlane3d.fromPlane plane
                |> SketchPlane3d.rotateAroundOwn SketchPlane3d.normalAxis rotationAngle
    in
    Random.map2 sketchPlane plane3d angle


arc2d : Generator (Arc2d Meters coordinates)
arc2d =
    Random.map3 Arc2d.from
        point2d
        point2d
        (Random.uniform
            (Random.float -350 350)
            [ Random.float 370 710
            , Random.float -710 -370
            ]
            |> Random.andThen (Random.map Angle.degrees)
        )


arc3d : Generator (Arc3d Meters coordinates)
arc3d =
    Random.map2 Arc3d.on sketchPlane3d arc2d


ellipse2d : Generator (Ellipse2d Meters coordinates)
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
    Random.map4 ellipse point2d direction2d positiveLength positiveLength


ellipticalArc2d : Generator (EllipticalArc2d Meters coordinates)
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
    Random.map3 ellipticalArc
        (Random.map2 Tuple.pair point2d direction2d)
        (Random.map2 Tuple.pair positiveLength positiveLength)
        (Random.map2 Tuple.pair angle angle)


ellipticalArc3d : Generator (EllipticalArc3d Meters coordinates)
ellipticalArc3d =
    Random.map2 EllipticalArc3d.on sketchPlane3d ellipticalArc2d
