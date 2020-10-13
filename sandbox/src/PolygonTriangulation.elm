--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module PolygonTriangulation exposing (main)

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Color
import Drawing2d
import Drawing2d.Attributes as Attributes
import Html exposing (Html)
import Html.Attributes
import Html.Events
import InputWidget as InputWidget
import Pixels exposing (Pixels)
import Point2d
import Polygon2d exposing (Polygon2d)
import Polygon2d.Random as Random
import Quantity exposing (Quantity)
import Random exposing (Generator)
import Rectangle2d
import Triangle2d exposing (Triangle2d)
import TriangularMesh


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { polygon : Polygon2d Pixels ScreenCoordinates
    , angle : Angle
    }


type Msg
    = Click
    | NewPolygon (Polygon2d Pixels ScreenCoordinates)
    | SetAngle Angle


renderBounds : BoundingBox2d Pixels ScreenCoordinates
renderBounds =
    BoundingBox2d.from Point2d.origin (Point2d.pixels 500 500)


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon (Random.polygon2d renderBounds)


square : Polygon2d Pixels ScreenCoordinates
square =
    Polygon2d.singleLoop
        [ Point2d.pixels 100 100
        , Point2d.pixels 400 100
        , Point2d.pixels 400 400
        , Point2d.pixels 100 400
        ]


init : () -> ( Model, Cmd Msg )
init () =
    ( { polygon = square, angle = Angle.degrees 0 }
    , Cmd.none
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
            BoundingBox2d.dimensions renderBounds

        rotatedPolygon =
            Polygon2d.rotateAround (BoundingBox2d.centerPoint renderBounds)
                model.angle
                model.polygon

        triangulationRule =
            Polygon2d.edgeSubdivisions
                (\p1 p2 ->
                    let
                        deltaY =
                            Quantity.abs (Point2d.yCoordinate p2 |> Quantity.minus (Point2d.yCoordinate p1))
                    in
                    ceiling (Quantity.ratio deltaY (Pixels.float 20))
                )

        mesh =
            Polygon2d.triangulateWith triangulationRule rotatedPolygon

        triangles =
            TriangularMesh.faceVertices mesh
                |> List.map Triangle2d.fromVertices

        drawTriangle =
            Drawing2d.triangle
                [ Drawing2d.fillColor (Color.rgba 0.5 0.5 0.5 0.25)
                , Drawing2d.strokeColor (Color.rgb 0.5 0.5 0.5)
                ]

        polygonElement =
            Drawing2d.polygon
                [ Drawing2d.noFill, Drawing2d.blackStroke ]
                rotatedPolygon
    in
    Html.div []
        [ Html.div [ Html.Events.onClick Click ]
            [ Drawing2d.toHtml
                { size = Drawing2d.fixed
                , viewBox = Rectangle2d.fromBoundingBox renderBounds
                }
                []
                [ polygonElement
                ]
            ]
        , Html.div [ Html.Events.onClick Click ]
            [ Drawing2d.toHtml
                { size = Drawing2d.fixed
                , viewBox = Rectangle2d.fromBoundingBox renderBounds
                }
                []
                [ Drawing2d.group [] (List.map drawTriangle triangles)
                , polygonElement
                ]
            ]
        , InputWidget.slider
            [ Html.Attributes.style "width" (String.fromFloat (Pixels.toFloat width) ++ "px") ]
            { min = -180, max = 180, step = 1 }
            (Angle.inDegrees model.angle)
            |> Html.map (Angle.degrees >> SetAngle)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
