module Readme exposing (..)

import Svg exposing (Svg)
import Html exposing (Html)
import Html.Attributes as Attributes
import Common exposing (..)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Frame2d as Frame2d
import OpenSolid.BoundingBox.Types exposing (..)


main : Html Never
main =
    let
        points =
            [ Point2d ( 2, 0 )
            , Point2d ( 3, 0.2 )
            , Point2d ( 4, 0.1 )
            ]

        rotatePoint =
            Point2d.rotateAround Point2d.origin (degrees 30)

        rotatedPoints =
            List.map rotatePoint points

        viewBox =
            BoundingBox2d
                { minX = -1
                , maxX = 5
                , minY = -1
                , maxY = 5
                }

        elements =
            List.concat
                [ [ frame2d black Frame2d.xy ]
                , List.map (point2d blue) points
                , List.map (point2d orange) rotatedPoints
                ]
    in
        scene2d viewBox elements
