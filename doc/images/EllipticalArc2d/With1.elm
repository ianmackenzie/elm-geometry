module EllipticalArc2d.With1 exposing (..)

import FillsAndStrokes exposing (..)
import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.EllipticalArc2d as EllipticalArc2d exposing (EllipticalArc2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import Svg exposing (Svg)


main : Html Never
main =
    let
        centerPoint =
            Point2d.origin

        xDirection =
            Direction2d.x

        arc =
            EllipticalArc2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = 200
                , yRadius = 100
                , startAngle = 0
                , sweptAngle = degrees 90
                }

        svg =
            Svg.g [ blackStroke, whiteFill ]
                [ Svg.direction2d [] centerPoint xDirection
                , Svg.point2d [] centerPoint
                , Svg.ellipticalArc2d [ blackStroke, noFill ] arc
                , Svg.point2d [] (EllipticalArc2d.startPoint arc)
                , Svg.point2d [] (EllipticalArc2d.endPoint arc)
                ]

        bounds =
            BoundingBox2d.with
                { minX = -10
                , maxX = 210
                , minY = -10
                , maxY = 110
                }
    in
    Svg.render2d bounds svg
