module HermiteCubicSpline2d exposing (..)

import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


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
            BoundingBox2d.with
                { minX = 30
                , maxX = 220
                , minY = 30
                , maxY = 190
                }

        p1 =
            Point2d.withCoordinates ( 50, 100 )

        p2 =
            Point2d.withCoordinates ( 150, 100 )

        v1 =
            Vector2d.withComponents ( 75, 75 )

        v2 =
            Vector2d.withComponents ( 50, -50 )

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
