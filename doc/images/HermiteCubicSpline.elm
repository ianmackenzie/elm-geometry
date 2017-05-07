module HermiteCubicSpline2d exposing (..)

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.Svg as Svg
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Html exposing (Html)


drawPoint : Point2d -> Svg Never
drawPoint =
    Svg.point2d
        { radius = 3
        , attributes = [ Attributes.fill "white", Attributes.stroke "black" ]
        }


drawVector : Point2d -> Vector2d -> Svg Never
drawVector =
    Svg.vector2d
        { tipAttributes = [ Attributes.fill "grey" ]
        , stemAttributes = [ Attributes.stroke "grey" ]
        , groupAttributes = []
        , tipLength = 10
        , tipWidth = 8
        }


drawSpline : CubicSpline2d -> Svg Never
drawSpline =
    Svg.cubicSpline2d
        [ Attributes.stroke "black"
        , Attributes.fill "none"
        ]


main : Html Never
main =
    let
        bounds =
            BoundingBox2d
                { minX = 30
                , maxX = 220
                , minY = 30
                , maxY = 190
                }

        p1 =
            Point2d ( 50, 100 )

        p2 =
            Point2d ( 150, 100 )

        v1 =
            Vector2d ( 75, 75 )

        v2 =
            Vector2d ( 50, -50 )

        spline =
            CubicSpline2d.hermite ( p1, v1 ) ( p2, v2 )

        svg =
            Svg.g []
                [ drawSpline spline
                , drawVector p1 v1
                , drawVector p2 v2
                , drawPoint p1
                , drawPoint p2
                ]
    in
        Svg.render2d bounds svg
