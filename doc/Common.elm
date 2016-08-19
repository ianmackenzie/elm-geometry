module Common
    exposing
        ( largeScale
        , smallScale
        , thin
        , black
        , blue
        , orange
        , teal
        , scene2d
        , point2d
        , centerPoint2d
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


largeScale : Float
largeScale =
    75


smallScale : Float
smallScale =
    40


thin : String
thin =
    "0.01"


pointRadius : String
pointRadius =
    "0.05"


centerPointRadius : String
centerPointRadius =
    "0.03"


originPointRadius : String
originPointRadius =
    "0.05"


centerPointCrossRadius : Float
centerPointCrossRadius =
    0.1


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


globalAttributes : List (Svg.Attribute msg)
globalAttributes =
    [ Attributes.strokeWidth "0.02" ]


scene2d : Float -> BoundingBox2d -> List (Svg msg) -> Html msg
scene2d scale boundingBox elements =
    let
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


point2d : String -> Point2d -> Svg msg
point2d color point =
    Svg.point2d [ Attributes.r pointRadius, Attributes.fill color ] point


centerPoint2d : String -> Point2d -> Svg msg
centerPoint2d color point =
    let
        frame =
            Frame2d.at point

        origin =
            Point2d.origin

        offset =
            centerPointCrossRadius

        verticalLine =
            LineSegment2d ( Point2d ( 0, -offset ), Point2d ( 0, offset ) )

        horizontalLine =
            LineSegment2d ( Point2d ( -offset, 0 ), Point2d ( offset, 0 ) )

        pointAttributes =
            [ Attributes.r centerPointRadius, Attributes.fill color ]
    in
        Svg.g [ Attributes.stroke color, Attributes.strokeWidth thin ]
            [ Svg.point2d pointAttributes origin
            , Svg.lineSegment2d [] verticalLine
            , Svg.lineSegment2d [] horizontalLine
            ]
            |> Svg.placeIn frame


originPoint2d : String -> Point2d -> Svg msg
originPoint2d color point =
    Svg.point2d
        [ Attributes.r originPointRadius
        , Attributes.fill "white"
        , Attributes.stroke color
        , Attributes.strokeWidth thin
        ]
        point


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
            Point2d ( length, 0 )

        stemPoint =
            Point2d ( length - tipLength, 0 )

        leftPoint =
            Point2d ( length - tipLength, tipWidth / 2 )

        rightPoint =
            Point2d ( length - tipLength, -tipWidth / 2 )

        stem =
            LineSegment2d ( basePoint, stemPoint )

        tip =
            Triangle2d ( tipPoint, leftPoint, rightPoint )
    in
        Svg.g
            [ Attributes.stroke color
            , Attributes.fill "white"
            , Attributes.strokeWidth thin
            ]
            [ Svg.lineSegment2d [] stem, Svg.triangle2d [] tip ]
            |> Svg.placeIn frame


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
            [ direction2d color originPoint xDirection
            , direction2d color originPoint yDirection
            , originPoint2d color originPoint
            ]
