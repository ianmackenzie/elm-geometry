module HelloWorld exposing (..)

import Html exposing (Html)
import Svg exposing (Svg)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
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


boundingBox : BoundingBox2d
boundingBox =
    BoundingBox2d { minX = -120, minY = -120, maxX = 120, maxY = 120 }


main : Html Never
main =
    Svg.scene2d boundingBox [ pointsGroup ]
