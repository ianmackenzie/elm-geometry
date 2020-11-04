module BSplines exposing (main)

import Circle2d exposing (Circle2d)
import Color exposing (Color)
import CubicSpline2d exposing (CubicSpline2d)
import Drawing2d
import Html exposing (Html)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Rectangle2d exposing (Rectangle2d)


main : Html Never
main =
    let
        knots =
            [ 2, 2, 2, 3, 4, 5, 5, 5 ]

        points =
            [ Point2d.pixels 100 100
            , Point2d.pixels 100 700
            , Point2d.pixels 300 400
            , Point2d.pixels 500 400
            , Point2d.pixels 700 700
            , Point2d.pixels 700 100
            ]

        dot point =
            Drawing2d.circle [] (Circle2d.withRadius (Pixels.float 3) point)

        segments =
            CubicSpline2d.bSpline knots points

        drawSegment segment =
            Drawing2d.group [ Drawing2d.blackFill ]
                [ Drawing2d.cubicSpline [] segment
                , dot (CubicSpline2d.startPoint segment)
                , dot (CubicSpline2d.endPoint segment)
                ]
    in
    Drawing2d.toHtml
        { viewBox = Rectangle2d.from Point2d.origin (Point2d.pixels 800 800)
        , size = Drawing2d.fixed
        }
        []
        [ Drawing2d.group [] (List.map drawSegment segments)
        , Drawing2d.polyline [ Drawing2d.strokeColor Color.lightGrey ] (Polyline2d.fromVertices points)
        , Drawing2d.group [] (List.map dot points)
        ]
