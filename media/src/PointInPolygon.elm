--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module PointInPolygon exposing (main)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Circle2d
import Element exposing (Element)
import Element.Background as Background
import Element.Events
import Element.Input as Input
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polygon2d.Random as Random
import Random exposing (Generator)
import Svg
import Svg.Attributes


type alias Model =
    { polygon : Polygon2d
    , points : List Point2d
    , angleInDegrees : Float
    }


type Msg
    = Click
    | NewPolygon Polygon2d
    | SetAngleInDegrees Float


renderBounds : BoundingBox2d
renderBounds =
    BoundingBox2d.fromExtrema
        { minX = 0
        , maxX = 600
        , minY = 0
        , maxY = 600
        }


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon (Random.polygon2d renderBounds)


pointGenerator : Generator Point2d
pointGenerator =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema renderBounds

        xCoordinate =
            Random.float minX maxX

        yCoordinate =
            Random.float minY maxY
    in
    Random.map2 Tuple.pair xCoordinate yCoordinate
        |> Random.map Point2d.fromCoordinates


init : () -> ( Model, Cmd Msg )
init () =
    ( { polygon = Polygon2d.singleLoop []
      , angleInDegrees = 0
      , points = Tuple.first (Random.step (Random.list 500 pointGenerator) (Random.initialSeed 1234))
      }
    , generateNewPolygon
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewPolygon )

        NewPolygon polygon ->
            ( { model | polygon = polygon }, Cmd.none )

        SetAngleInDegrees angleInDegrees ->
            ( { model | angleInDegrees = angleInDegrees }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( width, height ) =
            BoundingBox2d.dimensions renderBounds

        rotatedPolygon =
            Polygon2d.rotateAround (BoundingBox2d.centerPoint renderBounds)
                (degrees model.angleInDegrees)
                model.polygon

        polygonElement =
            Svg.polygon2d
                [ Svg.Attributes.fill "none", Svg.Attributes.stroke "black" ]
                rotatedPolygon

        drawPoint point =
            let
                color =
                    if Polygon2d.contains point rotatedPolygon then
                        "black"

                    else
                        "lightgrey"
            in
            Svg.circle2d [ Svg.Attributes.stroke "none", Svg.Attributes.fill color ]
                (Circle2d.withRadius 2 point)
    in
    Element.layout [] <|
        Element.column []
            [ Element.el [ Element.Events.onClick Click ] <|
                Element.html <|
                    Svg.svg
                        [ Svg.Attributes.width (String.fromFloat width)
                        , Svg.Attributes.height (String.fromFloat height)
                        ]
                        [ polygonElement
                        , Svg.g [] (List.map drawPoint model.points)
                        ]
            , Input.slider
                [ Element.width (Element.px (round width))
                , Element.height (Element.px 8)
                , Background.color (Element.rgb 0.75 0.75 0.75)
                ]
                { onChange = SetAngleInDegrees
                , min = -180
                , max = 180
                , step = Just 1
                , label = Input.labelHidden "Angle"
                , value = model.angleInDegrees
                , thumb = Input.defaultThumb
                }
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
