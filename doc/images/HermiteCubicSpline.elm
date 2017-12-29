module HermiteCubicSpline2d exposing (..)

import FillsAndStrokes exposing (..)
import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Svg)


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
            Point2d.fromCoordinates ( 50, 100 )

        p2 =
            Point2d.fromCoordinates ( 150, 100 )

        v1 =
            Vector2d.fromComponents ( 75, 75 )

        v2 =
            Vector2d.fromComponents ( 50, -50 )

        spline =
            CubicSpline2d.hermite ( p1, v1 ) ( p2, v2 )

        svg =
            Svg.g []
                [ Svg.cubicSpline2d [ blackStroke, noFill ] spline
                , Svg.vector2d [ greyStroke, greyFill ] p1 v1
                , Svg.vector2d [ greyStroke, greyFill ] p2 v2
                , Svg.point2d [ whiteFill, blackStroke ] p1
                , Svg.point2d [ whiteFill, blackStroke ] p2
                ]
    in
    Svg.render2d bounds svg
