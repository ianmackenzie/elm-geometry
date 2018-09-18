--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module PolygonTriangulation exposing (main)

import BoundingBox2d exposing (BoundingBox2d)
import Color
import Drawing2d
import Drawing2d.Attributes as Attributes
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Kintail.InputWidget as InputWidget
import Polygon2d exposing (Polygon2d)
import Polygon2d.Random as Random
import Random exposing (Generator)
import Triangle2d exposing (Triangle2d)
import TriangularMesh


type alias Model =
    { polygon : Polygon2d
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
        , maxX = 300
        , minY = 0
        , maxY = 300
        }


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon (Random.polygon2d renderBounds)


init : ( Model, Cmd Msg )
init =
    ( { polygon = Polygon2d.singleLoop [], angleInDegrees = 0 }
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

        mesh =
            Polygon2d.triangulate rotatedPolygon

        triangles =
            TriangularMesh.faceVertices mesh
                |> List.map Triangle2d.fromVertices

        drawTriangle =
            Drawing2d.triangleWith
                [ Attributes.fillColor (Color.rgba 127 127 127 0.25)
                , Attributes.strokeColor (Color.rgb 127 127 127)
                ]

        polygonElement =
            Drawing2d.polygonWith
                [ Attributes.noFill, Attributes.blackStroke ]
                rotatedPolygon
    in
    Html.div []
        [ Html.div [ Html.Events.onClick Click ]
            [ Drawing2d.toHtml renderBounds [] <|
                [ polygonElement
                ]
            ]
        , Html.div [ Html.Events.onClick Click ]
            [ Drawing2d.toHtml renderBounds [] <|
                [ Drawing2d.group (List.map drawTriangle triangles)
                , polygonElement
                ]
            ]
        , InputWidget.slider
            [ Html.Attributes.style [ ( "width", toString width ++ "px" ) ] ]
            { min = -180, max = 180, step = 1 }
            model.angleInDegrees
            |> Html.map SetAngleInDegrees
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
