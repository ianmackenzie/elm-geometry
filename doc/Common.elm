module Common
    exposing
        ( scene2d
        , black
        , blue
        , orange
        , teal
        , point2d
        , direction2d
        , frame2d
        )

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Frame2d as Frame2d
import OpenSolid.LineSegment.Types exposing (..)
import OpenSolid.Triangle.Types exposing (..)
import OpenSolid.BoundingBox.Types exposing (..)
import OpenSolid.BoundingBox.BoundingBox2d as BoundingBox2d
import OpenSolid.Svg as Svg


globalAttributes : List (Svg.Attribute msg)
globalAttributes =
    [ Attributes.strokeWidth "0.02" ]


scene2d : BoundingBox2d -> List (Svg msg) -> Html msg
scene2d boundingBox elements =
    let
        scale =
            50

        minX =
            BoundingBox2d.minX boundingBox

        maxX =
            BoundingBox2d.maxX boundingBox

        minY =
            BoundingBox2d.minY boundingBox

        maxY =
            BoundingBox2d.maxY boundingBox

        width =
            toString (scale * (maxX - minX))

        height =
            toString (scale * (maxY - minY))

        topLeftFrame =
            Frame2d
                { originPoint = Point2d ( minX, maxY )
                , xDirection = Direction2d.x
                , yDirection = Direction2d.negate Direction2d.y
                }
    in
        Svg.svg [ Attributes.width width, Attributes.height height ]
            [ Svg.g globalAttributes elements
                |> Svg.relativeTo topLeftFrame
                |> Svg.scaleAbout Point2d.origin scale
            ]


black : String
black =
    "black"


blue : String
blue =
    "rgb(0, 109, 219)"


orange : String
orange =
    "rgb(219, 109, 0)"


teal : String
teal =
    "rgb(0, 146, 146)"


point2d : String -> Point2d -> Svg msg
point2d color point =
    Svg.point2d [ Attributes.r "0.05", Attributes.fill color ] point


direction2d : String -> Point2d -> Direction2d -> Svg msg
direction2d color basePoint direction =
    let
        length =
            1

        tipLength =
            0.15

        tipWidth =
            0.1

        frame =
            Frame2d
                { originPoint = basePoint
                , xDirection = direction
                , yDirection = Direction2d.perpendicularTo direction
                }

        tipPoint =
            Point2d ( length, 0 ) |> Point2d.placeIn frame

        stemPoint =
            Point2d ( length - tipLength, 0 ) |> Point2d.placeIn frame

        leftPoint =
            Point2d ( length - tipLength, tipWidth / 2 )
                |> Point2d.placeIn frame

        rightPoint =
            Point2d ( length - tipLength, -tipWidth / 2 )
                |> Point2d.placeIn frame

        stem =
            LineSegment2d ( basePoint, stemPoint )

        tip =
            Triangle2d ( tipPoint, leftPoint, rightPoint )
    in
        Svg.g [ Attributes.stroke color ]
            [ Svg.lineSegment2d [] stem, Svg.triangle2d [] tip ]


frame2d : String -> Frame2d -> Svg msg
frame2d color frame =
    let
        originPoint =
            Frame2d.originPoint frame

        xDirection =
            Frame2d.xDirection frame

        yDirection =
            Frame2d.yDirection frame
    in
        Svg.g []
            [ point2d color originPoint
            , direction2d color originPoint xDirection
            , direction2d color originPoint yDirection
            ]
