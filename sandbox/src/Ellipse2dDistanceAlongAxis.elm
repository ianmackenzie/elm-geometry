module Ellipse2dDistanceAlongAxis exposing (main)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import DistanceAlongAxis2d
import Drawing2d
import Ellipse2d
import Html exposing (Html)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Random2d
import Rectangle2d exposing (Rectangle2d)
import VisualTest


main : Program () Int VisualTest.Msg
main =
    Browser.element
        { init = always ( 0, Cmd.none )
        , update = \msg index -> ( VisualTest.update msg index, Cmd.none )
        , subscriptions = always VisualTest.onKeyDown
        , view = view
        }


viewBounds : BoundingBox2d Pixels coordinates
viewBounds =
    BoundingBox2d.from Point2d.origin (Point2d.pixels 800 800)


view : Int -> Html msg
view index =
    let
        ellipse =
            VisualTest.randomValue (Random2d.ellipse viewBounds) index

        axis =
            VisualTest.randomValue (Random2d.axis viewBounds) index

        distanceInterval =
            Ellipse2d.signedDistanceAlong axis ellipse
    in
    Drawing2d.toHtml
        { viewBox = Rectangle2d.fromBoundingBox viewBounds
        , size = Drawing2d.fixed
        }
        []
        [ DistanceAlongAxis2d.drawProjection axis distanceInterval
        , Drawing2d.ellipse [] ellipse
        ]
