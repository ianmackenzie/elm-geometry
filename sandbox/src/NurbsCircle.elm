module NurbsCircle exposing (main)

import Circle2d
import Color
import Drawing2d
import Html exposing (Html)
import Pixels
import Point2d
import Polyline2d
import Quantity
import RationalQuadraticSpline2d
import Rectangle2d


main : Html Never
main =
    let
        circle =
            Circle2d.withRadius (Pixels.float 300) Point2d.origin

        w =
            1 / sqrt 2

        splineSegments =
            RationalQuadraticSpline2d.bSplineSegments
                [ 0, 0, 1, 1, 2, 2, 3, 3, 4, 4 ]
                [ ( Point2d.pixels 300 0, 1 )
                , ( Point2d.pixels 300 300, w )
                , ( Point2d.pixels 0 300, 1 )
                , ( Point2d.pixels -300 300, w )
                , ( Point2d.pixels -300 0, 1 )
                , ( Point2d.pixels -300 -300, w )
                , ( Point2d.pixels 0 -300, 1 )
                , ( Point2d.pixels 300 -300, w )
                , ( Point2d.pixels 300 0, 1 )
                ]

        polylines =
            List.map (RationalQuadraticSpline2d.segments 100) splineSegments
    in
    Drawing2d.toHtml
        { viewBox =
            Rectangle2d.from
                (Point2d.pixels -400 -400)
                (Point2d.pixels 400 400)
        , size = Drawing2d.fixed
        }
        []
        [ Drawing2d.group
            [ Drawing2d.strokeColor Color.lightBlue
            , Drawing2d.strokeWidth (Pixels.float 6)
            ]
            (List.map (Drawing2d.polyline []) polylines)
        , Drawing2d.circle
            [ Drawing2d.strokeColor Color.orange
            , Drawing2d.strokeWidth (Pixels.float 2)
            , Drawing2d.noFill
            ]
            circle
        ]
