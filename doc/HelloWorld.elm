module HelloWorld exposing (..)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Frame2d as Frame2d
import OpenSolid.BoundingBox.Types exposing (..)
import OpenSolid.Svg as Svg
import Svg.Attributes as Attributes


angles =
    [0..24] |> List.map (\index -> index * degrees 15)


firstPoint =
    Point2d ( 100, 0 )


pointAt angle =
    Point2d.rotateAround Point2d.origin angle firstPoint


points =
    List.map pointAt angles


svgPoint : Point2d -> Svg Never
svgPoint point =
    Svg.point2d [ Attributes.r "2", Attributes.fill "blue" ] point


pointsGroup : Svg Never
pointsGroup =
    Svg.g [] (List.map svgPoint points)


topLeftFrame =
    Frame2d.at (Point2d ( -120, 120 )) |> Frame2d.flipY


main : Html Never
main =
    Svg.svg [ Attributes.width "240", Attributes.height "240" ]
        [ Svg.relativeTo topLeftFrame pointsGroup ]
