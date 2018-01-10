module ReleaseNotes.ArcLengthParameterization exposing (..)

import Html exposing (Html)
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.Svg as Svg
import ReleaseNotes.Common exposing (..)
import Svg


main : Html Never
main =
    let
        parameterization =
            CubicSpline2d.arcLengthParameterized 0.5 spline

        overallArcLength =
            CubicSpline2d.arcLength parameterization

        arcLengths =
            List.range 0 numSegments
                |> List.map
                    (\n -> toFloat n * overallArcLength / toFloat numSegments)

        points =
            arcLengths
                |> List.filterMap (CubicSpline2d.pointAlong parameterization)

        pointElements =
            points |> List.map (Svg.point2d [ whiteFill, blackStroke ])
    in
    Svg.render2d renderBounds <|
        Svg.g []
            [ Svg.cubicSpline2d [ blackStroke, noFill ] spline
            , Svg.g [] pointElements
            ]
