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
        orangePoints =
            [ Point2d ( 2, 0 )
            , Point2d ( 2.5, 0.1 )
            , Point2d ( 3, 0.2 )
            , Point2d ( 4, 0.1 )
            ]

        firstRotation =
            Point2d.rotateAround Point2d.origin (degrees 30)

        bluePoints =
            List.map firstRotation orangePoints

        centerPoint =
            Point2d ( 3, 2 )

        secondRotation =
            Point2d.rotateAround centerPoint (degrees -60)

        tealPoints =
            List.map secondRotation bluePoints

        viewBox =
            BoundingBox2d
                { minX = -0.4
                , maxX = 4.4
                , minY = -0.4
                , maxY = 3.0
                }

        elements =
            [ frame2d black Frame2d.xy
            , centerPoint2d black centerPoint
            ]
                ++ List.map (point2d orange) orangePoints
                ++ List.map (point2d blue) bluePoints
                ++ List.map (point2d teal) tealPoints
    in
        scene2d largeScale viewBox elements
