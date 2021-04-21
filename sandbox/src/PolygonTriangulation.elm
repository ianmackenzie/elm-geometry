--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module PolygonTriangulation exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Circle2d
import Color exposing (Color)
import Drawing2d
import Drawing2d.Attributes as Attributes
import Generic.Polygon2d as Polygon2d exposing (Polygon2d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import InputWidget as InputWidget
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d.Random as Random
import Quantity exposing (Quantity)
import Random exposing (Generator)
import Rectangle2d
import Triangle2d exposing (Triangle2d)
import TriangularMesh


type ScreenCoordinates
    = ScreenCoordinates


type Vertex
    = ExteriorVertex (Point2d Pixels ScreenCoordinates)
    | InteriorVertex (Point2d Pixels ScreenCoordinates)


type alias Model =
    { polygon : Polygon2d Vertex
    , angle : Angle
    }


type Msg
    = Click
    | NewPolygon (Polygon2d Vertex)
    | SetAngle Angle


vertexPosition : Vertex -> Point2d Pixels ScreenCoordinates
vertexPosition vertex =
    case vertex of
        ExteriorVertex position ->
            position

        InteriorVertex position ->
            position


vertexColor : Vertex -> Color
vertexColor vertex =
    case vertex of
        ExteriorVertex _ ->
            Color.blue

        InteriorVertex _ ->
            Color.orange


mapVertex : (Point2d Pixels ScreenCoordinates -> Point2d Pixels ScreenCoordinates) -> Vertex -> Vertex
mapVertex pointFunction vertex =
    case vertex of
        ExteriorVertex point ->
            ExteriorVertex (pointFunction point)

        InteriorVertex point ->
            InteriorVertex (pointFunction point)


vertexMidpoint : Vertex -> Vertex -> Vertex
vertexMidpoint firstVertex secondVertex =
    InteriorVertex (Point2d.midpoint (vertexPosition firstVertex) (vertexPosition secondVertex))


renderBounds : BoundingBox2d Pixels ScreenCoordinates
renderBounds =
    BoundingBox2d.from Point2d.origin (Point2d.pixels 500 500)


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon (Random.polygon2d renderBounds |> Random.map (Polygon2d.mapVertices ExteriorVertex vertexPosition))


square : Polygon2d Vertex
square =
    Polygon2d.singleLoopBy vertexPosition
        [ ExteriorVertex (Point2d.pixels 100 100)
        , ExteriorVertex (Point2d.pixels 400 100)
        , ExteriorVertex (Point2d.pixels 400 400)
        , ExteriorVertex (Point2d.pixels 100 400)
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
            Polygon2d.mapVertices
                (mapVertex (Point2d.rotateAround (BoundingBox2d.centerPoint renderBounds) model.angle))
                vertexPosition
                model.polygon

        triangulationRule =
            Polygon2d.maxTriangleDimensions Quantity.positiveInfinity (Pixels.float 50)

        mesh =
            Polygon2d.triangulateWith
                { triangulationRule = triangulationRule
                , vertexPosition = vertexPosition
                , midpoint = vertexMidpoint
                }
                rotatedPolygon

        triangles =
            TriangularMesh.faceVertices mesh
                |> List.map
                    (\( v1, v2, v3 ) ->
                        Triangle2d.from
                            (vertexPosition v1)
                            (vertexPosition v2)
                            (vertexPosition v3)
                    )

        drawTriangle =
            Drawing2d.triangle
                [ Drawing2d.fillColor (Color.rgba 0.5 0.5 0.5 0.25)
                , Drawing2d.strokeColor (Color.rgb 0.5 0.5 0.5)
                ]

        polygonEntity =
            Drawing2d.polygon
                [ Drawing2d.noFill, Drawing2d.blackStroke ]
                (Polygon2d.mapVertices vertexPosition identity rotatedPolygon)

        drawVertex vertex =
            Drawing2d.circle [ Drawing2d.fillColor (vertexColor vertex) ]
                (Circle2d.withRadius (Pixels.float 4) (vertexPosition vertex))
    in
    Html.div []
        [ Html.div [ Html.Events.onClick Click ]
            [ Drawing2d.draw
                { viewBox = Rectangle2d.fromBoundingBox renderBounds
                , entities = [ polygonEntity ]
                }
            ]
        , Html.div [ Html.Events.onClick Click ]
            [ Drawing2d.draw
                { viewBox = Rectangle2d.fromBoundingBox renderBounds
                , entities =
                    [ Drawing2d.group [] (List.map drawTriangle triangles)
                    , polygonEntity
                    , Drawing2d.group [] (List.map drawVertex (Array.toList (TriangularMesh.vertices mesh)))
                    ]
                }
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
