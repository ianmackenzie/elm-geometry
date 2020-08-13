--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module VoronoiRegions exposing (main)

import Array
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Circle2d
import Color exposing (Color)
import Colorbrewer.Qualitative
import Drawing2d
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import LineSegment2d
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Random exposing (Generator)
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)
import VoronoiDiagram2d exposing (Error(..), VoronoiDiagram2d)


type ScreenCoordinates
    = ScreenCoordinates


type alias Point =
    Point2d Pixels ScreenCoordinates


type alias Vertex =
    { position : Point
    , color : Color
    }


type alias Model =
    { baseDiagram : Result (Error Vertex) (VoronoiDiagram2d Vertex Pixels ScreenCoordinates)
    , mousePosition : Maybe Point
    }


type Msg
    = Click
    | NewRandomVertices (List Vertex)
    | MouseMove Mouse.Event


drawingDimensions : ( Float, Float )
drawingDimensions =
    ( 700, 700 )


renderBounds : Rectangle2d Pixels ScreenCoordinates
renderBounds =
    Rectangle2d.from Point2d.origin
        (Point2d.fromTuple Pixels.float drawingDimensions)


assignColors : List Point -> List Vertex
assignColors points =
    assignColorsHelp points [] []


assignColorsHelp : List Point -> List Color -> List Vertex -> List Vertex
assignColorsHelp points colors accumulated =
    case points of
        [] ->
            List.reverse accumulated

        firstPoint :: remainingPoints ->
            case colors of
                [] ->
                    assignColorsHelp points Colorbrewer.Qualitative.set312 accumulated

                firstColor :: remainingColors ->
                    let
                        newVertex =
                            { position = firstPoint
                            , color = firstColor
                            }
                    in
                    assignColorsHelp
                        remainingPoints
                        remainingColors
                        (newVertex :: accumulated)


verticesGenerator : Generator (List Vertex)
verticesGenerator =
    let
        pointGenerator =
            Random.map2 (Rectangle2d.interpolate renderBounds)
                (Random.float 0.05 0.95)
                (Random.float 0.05 0.95)
    in
    Random.int 32 256
        |> Random.andThen (\listSize -> Random.list listSize pointGenerator)
        |> Random.map assignColors


collinearVerticesGenerator : Generator (List Vertex)
collinearVerticesGenerator =
    let
        pointGenerator =
            Random.map (\t -> Rectangle2d.interpolate renderBounds t t)
                (Random.float 0.05 0.95)
    in
    Random.int 1 5
        |> Random.andThen (\listSize -> Random.list listSize pointGenerator)
        |> Random.map assignColors


generateNewVertices : Cmd Msg
generateNewVertices =
    Random.generate NewRandomVertices verticesGenerator


init : () -> ( Model, Cmd Msg )
init () =
    ( { baseDiagram = Ok VoronoiDiagram2d.empty
      , mousePosition = Nothing
      }
    , generateNewVertices
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewVertices )

        NewRandomVertices vertices ->
            let
                diagram =
                    VoronoiDiagram2d.fromVerticesBy .position (Array.fromList vertices)
            in
            ( { model | baseDiagram = diagram }, Cmd.none )

        MouseMove event ->
            let
                topLeftFrame =
                    Frame2d.atPoint (Rectangle2d.interpolate renderBounds 0 1)
                        |> Frame2d.reverseY

                point =
                    Point2d.fromTuple Pixels.float event.clientPos
                        |> Point2d.relativeTo topLeftFrame
            in
            ( { model | mousePosition = Just point }, Cmd.none )


darken : Color -> Color
darken color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color

        scale =
            0.8
    in
    Color.fromRgba
        { red = scale * red
        , green = scale * green
        , blue = scale * blue
        , alpha = alpha
        }


drawPolygon : ( Vertex, Polygon2d Pixels ScreenCoordinates ) -> Drawing2d.Element Pixels ScreenCoordinates event
drawPolygon ( vertex, polygon ) =
    Drawing2d.polygon
        [ Drawing2d.strokeColor (darken vertex.color)
        , Drawing2d.fillColor vertex.color
        ]
        polygon


drawVertex : Vertex -> Drawing2d.Element Pixels ScreenCoordinates event
drawVertex vertex =
    Drawing2d.circle [ Drawing2d.fillColor (darken vertex.color) ]
        (Circle2d.withRadius (Pixels.float 2.5) vertex.position)


view : Model -> Html Msg
view model =
    let
        voronoiDiagram =
            case model.mousePosition of
                Just point ->
                    let
                        vertex =
                            { position = point
                            , color = Color.white
                            }
                    in
                    model.baseDiagram
                        |> Result.andThen
                            (VoronoiDiagram2d.insertVertexBy .position vertex)

                Nothing ->
                    model.baseDiagram

        vertices =
            voronoiDiagram
                |> Result.map (VoronoiDiagram2d.vertices >> Array.toList)
                |> Result.withDefault []

        trimBox =
            BoundingBox2d.fromExtrema
                { minX = Pixels.float 150
                , maxX = Pixels.float 550
                , minY = Pixels.float 150
                , maxY = Pixels.float 550
                }

        polygons =
            voronoiDiagram
                |> Result.map (VoronoiDiagram2d.polygons trimBox)
                |> Result.withDefault []

        mousePointElement =
            case model.mousePosition of
                Just point ->
                    Drawing2d.circle [ Drawing2d.fillColor Color.blue ] (Circle2d.withRadius (Pixels.float 2.5) point)

                Nothing ->
                    Drawing2d.empty

        ( width, height ) =
            drawingDimensions

        overlayPolygon =
            Polygon2d.singleLoop
                [ Point2d.pixels 0 0
                , Point2d.pixels width 0
                , Point2d.pixels width height
                , Point2d.pixels 0 height
                ]

        isInBounds vertex =
            BoundingBox2d.contains vertex.position trimBox
    in
    Html.div
        [ Html.Events.onClick Click
        , Mouse.onMove MouseMove
        ]
        [ Drawing2d.toHtml
            { size = Drawing2d.fixed
            , viewBox = renderBounds
            }
            []
            [ Drawing2d.group [] (List.map drawPolygon polygons)
            , Drawing2d.group [] (List.map drawVertex (List.filter isInBounds vertices))
            , mousePointElement
            , Drawing2d.polygon [ Drawing2d.noFill ] overlayPolygon
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
