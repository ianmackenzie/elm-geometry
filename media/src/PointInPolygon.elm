--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module PointInPolygon exposing (main)

import Angle exposing (Angle)
import Browser
import Circle2d
import Color
import Drawing2d
import Element exposing (Element)
import Element.Background as Background
import Element.Events
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polygon2d.Random as Random
import Random exposing (Generator)
import Rectangle2d exposing (Rectangle2d)


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { polygon : Polygon2d Pixels ScreenCoordinates
    , points : List (Point2d Pixels ScreenCoordinates)
    , angle : Angle
    }


type Msg
    = Click
    | NewPolygon (Polygon2d Pixels ScreenCoordinates)
    | SetAngle Angle


renderBounds : Rectangle2d Pixels ScreenCoordinates
renderBounds =
    Rectangle2d.from Point2d.origin (Point2d.pixels 600 600)


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon (Random.polygon2d (Rectangle2d.boundingBox renderBounds))


pointGenerator : Generator (Point2d Pixels ScreenCoordinates)
pointGenerator =
    Random.map2 (Rectangle2d.interpolate renderBounds)
        (Random.float 0 1)
        (Random.float 0 1)


init : () -> ( Model, Cmd Msg )
init () =
    ( { polygon = Polygon2d.singleLoop []
      , angle = Angle.degrees 0
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

        SetAngle angle ->
            ( { model | angle = angle }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( width, height ) =
            Rectangle2d.dimensions renderBounds

        rotatedPolygon =
            Polygon2d.rotateAround (Rectangle2d.centerPoint renderBounds)
                model.angle
                model.polygon

        polygonElement =
            Drawing2d.polygon
                [ Drawing2d.noFill, Drawing2d.blackStroke ]
                rotatedPolygon

        drawPoint point =
            let
                color =
                    if Polygon2d.contains point rotatedPolygon then
                        Color.black

                    else
                        Color.lightGrey
            in
            Drawing2d.circle [ Drawing2d.noBorder, Drawing2d.fillColor color ]
                (Circle2d.withRadius (Pixels.float 2) point)
    in
    Element.layout [] <|
        Element.column []
            [ Element.el [ Element.Events.onClick Click ] <|
                Element.html <|
                    Drawing2d.toHtml
                        { size = Drawing2d.fixed
                        , viewBox = renderBounds
                        }
                        []
                        [ Drawing2d.group [] (List.map drawPoint model.points)
                        , polygonElement
                        ]
            , Input.slider
                [ Element.width (Element.px (round (Pixels.toFloat width)))
                , Element.height (Element.px 8)
                , Background.color (Element.rgb 0.75 0.75 0.75)
                ]
                { onChange = Angle.degrees >> SetAngle
                , min = -180
                , max = 180
                , step = Just 1
                , label = Input.labelHidden "Angle"
                , value = Angle.inDegrees model.angle
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
