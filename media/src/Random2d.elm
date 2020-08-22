module Random2d exposing
    ( arbitraryFrame
    , axis
    , boundingBox
    , circle
    , circularArc
    , cubicSpline
    , ellipse
    , ellipticalArc
    , leftHandedFrame
    , lineSegment
    , polyline
    , quadraticSpline
    , rectangle
    , rightHandedFrame
    , triangle
    )

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity)
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import Rectangle2d exposing (Rectangle2d)
import Triangle2d exposing (Triangle2d)


axis : BoundingBox2d units coordinates -> Generator (Axis2d units coordinates)
axis bounds =
    Random.map2 Axis2d.withDirection Direction2d.random (BoundingBox2d.randomPoint bounds)


rightHandedFrame : BoundingBox2d units coordinates -> Generator (Frame2d units coordinates defines)
rightHandedFrame bounds =
    Random.map2 Frame2d.withXDirection Direction2d.random (BoundingBox2d.randomPoint bounds)


leftHandedFrame : BoundingBox2d units coordinates -> Generator (Frame2d units coordinates defines)
leftHandedFrame bounds =
    Random.map Frame2d.reverseY (rightHandedFrame bounds)


arbitraryFrame : BoundingBox2d units coordinates -> Generator (Frame2d units coordinates defines)
arbitraryFrame bounds =
    Random.uniform (rightHandedFrame bounds) [ leftHandedFrame bounds ]
        |> Random.andThen identity


boundingBox : BoundingBox2d units coordinates -> Generator (BoundingBox2d units coordinates)
boundingBox bounds =
    let
        randomPoint =
            BoundingBox2d.randomPoint bounds
    in
    Random.map2 BoundingBox2d.from randomPoint randomPoint


lineSegment : BoundingBox2d units coordinates -> Generator (LineSegment2d units coordinates)
lineSegment bounds =
    let
        randomPoint =
            BoundingBox2d.randomPoint bounds
    in
    Random.map2 LineSegment2d.from randomPoint randomPoint


triangle : BoundingBox2d units coordinates -> Generator (Triangle2d units coordinates)
triangle bounds =
    let
        randomPoint =
            BoundingBox2d.randomPoint bounds
    in
    Random.map3 Triangle2d.from randomPoint randomPoint randomPoint


polyline : BoundingBox2d units coordinates -> Generator (Polyline2d units coordinates)
polyline bounds =
    Random.int 2 12
        |> Random.andThen
            (\numVertices ->
                Random.list numVertices (BoundingBox2d.randomPoint bounds)
                    |> Random.map Polyline2d.fromVertices
            )


circle : BoundingBox2d units coordinates -> Generator (Circle2d units coordinates)
circle bounds =
    BoundingBox2d.randomPoint bounds
        |> Random.andThen
            (\centerPoint ->
                let
                    { minX, maxX, minY, maxY } =
                        BoundingBox2d.extrema bounds

                    ( x0, y0 ) =
                        Point2d.coordinates centerPoint

                    maxRadiusX =
                        Quantity.min
                            (x0 |> Quantity.minus minX)
                            (maxX |> Quantity.minus x0)

                    maxRadiusY =
                        Quantity.min
                            (y0 |> Quantity.minus minY)
                            (maxY |> Quantity.minus y0)

                    maxRadius =
                        Quantity.min maxRadiusX maxRadiusY
                in
                Random.map (Circle2d.atPoint centerPoint)
                    (Interval.randomValue (Interval.from Quantity.zero maxRadius))
            )


circularArc : BoundingBox2d units coordinates -> Generator (Arc2d units coordinates)
circularArc bounds =
    let
        largeAngle =
            Random.map Angle.turns (Random.float -1.5 1.5)
    in
    Random.map3
        (\generatedCircle startAngle sweptAngle ->
            Arc2d.with
                { centerPoint = Circle2d.centerPoint generatedCircle
                , radius = Circle2d.radius generatedCircle
                , startAngle = startAngle
                , sweptAngle = sweptAngle
                }
        )
        (circle bounds)
        largeAngle
        largeAngle


quadraticSpline : BoundingBox2d units coordinates -> Generator (QuadraticSpline2d units coordinates)
quadraticSpline bounds =
    let
        randomPoint =
            BoundingBox2d.randomPoint bounds
    in
    Random.map3 QuadraticSpline2d.fromControlPoints randomPoint randomPoint randomPoint


cubicSpline : BoundingBox2d units coordinates -> Generator (CubicSpline2d units coordinates)
cubicSpline bounds =
    let
        randomPoint =
            BoundingBox2d.randomPoint bounds
    in
    Random.map4 CubicSpline2d.fromControlPoints randomPoint randomPoint randomPoint randomPoint


rectangle : BoundingBox2d units coordinates -> Generator (Rectangle2d units coordinates)
rectangle bounds =
    Random.map3
        (\boundingCircle orientation vertexAngle ->
            let
                axes =
                    Frame2d.withAngle orientation (Circle2d.centerPoint boundingCircle)

                radius =
                    Circle2d.radius boundingCircle

                width =
                    Quantity.twice (radius |> Quantity.multiplyBy (Angle.cos vertexAngle))

                height =
                    Quantity.twice (radius |> Quantity.multiplyBy (Angle.sin vertexAngle))
            in
            Rectangle2d.centeredOn axes ( width, height )
        )
        (circle bounds)
        (Random.map Angle.degrees (Random.float -180 180))
        (Random.map Angle.degrees (Random.float 0 90))


ellipse : BoundingBox2d units coordinates -> Generator (Ellipse2d units coordinates)
ellipse bounds =
    Random.map3
        (\boundingCircle orientation vertexAngle ->
            let
                circleRadius =
                    Circle2d.radius boundingCircle

                xRadius =
                    circleRadius |> Quantity.multiplyBy (Angle.cos vertexAngle)

                yRadius =
                    circleRadius |> Quantity.multiplyBy (Angle.sin vertexAngle)

                radiusScale =
                    min
                        (Quantity.ratio circleRadius xRadius)
                        (Quantity.ratio circleRadius yRadius)
            in
            Ellipse2d.with
                { centerPoint = Circle2d.centerPoint boundingCircle
                , xDirection = Direction2d.fromAngle orientation
                , xRadius = Quantity.multiplyBy radiusScale xRadius
                , yRadius = Quantity.multiplyBy radiusScale yRadius
                }
        )
        (circle bounds)
        (Random.map Angle.degrees (Random.float -180 180))
        (Random.map Angle.degrees (Random.float 0 90))


ellipticalArc : BoundingBox2d units coordinates -> Generator (EllipticalArc2d units coordinates)
ellipticalArc bounds =
    let
        largeAngle =
            Random.map Angle.turns (Random.float -1.5 1.5)
    in
    Random.map3
        (\generatedEllipse startAngle sweptAngle ->
            EllipticalArc2d.with
                { centerPoint = Ellipse2d.centerPoint generatedEllipse
                , xDirection = Ellipse2d.xDirection generatedEllipse
                , xRadius = Ellipse2d.xRadius generatedEllipse
                , yRadius = Ellipse2d.yRadius generatedEllipse
                , startAngle = startAngle
                , sweptAngle = sweptAngle
                }
        )
        (ellipse bounds)
        largeAngle
        largeAngle
