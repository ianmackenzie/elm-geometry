module ReleaseNotes.DefaultParameterization exposing (..)

import Html exposing (Html)
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.Svg as Svg
import ReleaseNotes.Common exposing (..)
import Svg


main : Html Never
main =
    let
        parameterValues =
            List.range 0 numSegments
                |> List.map (\n -> toFloat n / toFloat numSegments)

        points =
            parameterValues
                |> List.map (CubicSpline2d.pointOn spline)

        pointElements =
            points |> List.map (Svg.point2d [ whiteFill, blackStroke ])
    in
    Svg.render2d renderBounds <|
        Svg.g []
            [ Svg.cubicSpline2d [ blackStroke, noFill ] spline
            , Svg.g [] pointElements
            ]
