module BSplines exposing (main)

import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Drawing2d
import Frame2d
import Html exposing (Html)
import Interval
import Length
import LineSegment2d
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity
import RationalQuadraticSpline2d exposing (RationalQuadraticSpline2d)
import Rectangle2d exposing (Rectangle2d)
import Triangle2d
import Vector2d


main : Html Never
main =
    let
        knots =
            [ 0, 0, 2, 3, 4, 5, 6, 8, 12, 12 ]

        weightedControlPoints =
            [ ( Point2d.meters 1 8, 2 )
            , ( Point2d.meters 4 5, 3 )
            , ( Point2d.meters 2 4, 1 )
            , ( Point2d.meters 4 1, 3 )
            , ( Point2d.meters 8 2, 1 )
            , ( Point2d.meters 5 6, 5 )
            , ( Point2d.meters 8 9, 1 )
            , ( Point2d.meters 9 7, 2 )
            , ( Point2d.meters 9 4, 1 )
            ]
                |> List.map (Tuple.mapFirst (Point2d.at (Pixels.float 80 |> Quantity.per Length.meter)))
                |> List.map (Tuple.mapSecond identity)

        dot point =
            Drawing2d.circle [] (Circle2d.withRadius (Pixels.float 3) point)

        points =
            List.map Tuple.first weightedControlPoints

        segments =
            RationalQuadraticSpline2d.bSplineSegments knots weightedControlPoints

        knotIntervals =
            RationalQuadraticSpline2d.bSplineIntervals knots

        arrow attributes point vector =
            let
                endPoint =
                    point |> Point2d.translateBy vector
            in
            case Vector2d.direction vector of
                Just direction ->
                    let
                        tipFrame =
                            Frame2d.withXDirection direction endPoint
                    in
                    Drawing2d.group attributes
                        [ Drawing2d.lineSegment [] (LineSegment2d.from point endPoint)
                        , Drawing2d.placeIn tipFrame <|
                            Drawing2d.triangle [] <|
                                Triangle2d.from
                                    (Point2d.pixels -5 -3)
                                    Point2d.origin
                                    (Point2d.pixels -5 3)
                        ]

                Nothing ->
                    Drawing2d.empty

        drawSegment segment knotInterval =
            let
                startPoint =
                    RationalQuadraticSpline2d.startPoint segment

                endPoint =
                    RationalQuadraticSpline2d.endPoint segment

                startDerivative =
                    RationalQuadraticSpline2d.startDerivative segment
                        |> Vector2d.scaleBy (1 / Interval.width knotInterval)
                        |> Vector2d.scaleBy 0.5

                endDerivative =
                    RationalQuadraticSpline2d.endDerivative segment
                        |> Vector2d.scaleBy (1 / Interval.width knotInterval)
                        |> Vector2d.scaleBy 0.5
            in
            Drawing2d.group [ Drawing2d.blackFill ]
                [ Drawing2d.polyline [] (RationalQuadraticSpline2d.segments 100 segment)
                , dot (RationalQuadraticSpline2d.startPoint segment)
                , dot (RationalQuadraticSpline2d.endPoint segment)
                , arrow [ Drawing2d.strokeColor Color.blue, Drawing2d.fillColor Color.blue ] startPoint startDerivative
                , arrow [ Drawing2d.strokeColor Color.orange, Drawing2d.fillColor Color.orange ] endPoint endDerivative
                ]
    in
    Drawing2d.toHtml
        { viewBox = Rectangle2d.from (Point2d.pixels -100 0) (Point2d.pixels 800 800)
        , size = Drawing2d.fixed
        }
        []
        [ Drawing2d.polyline [ Drawing2d.strokeColor Color.grey ] (Polyline2d.fromVertices points)
        , Drawing2d.group [] (List.map2 drawSegment segments knotIntervals)
        , Drawing2d.group [] (List.map dot points)
        ]
