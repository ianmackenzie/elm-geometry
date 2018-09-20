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
import Colorbrewer.Qualitative
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import LineSegment2d
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)
import VoronoiDiagram2d exposing (Error(..), VoronoiDiagram2d)


type alias Vertex =
    { position : Point2d
    , color : ( Float, Float, Float )
    }


type alias Model =
    { baseDiagram : Result (Error Vertex) (VoronoiDiagram2d Vertex)
    , mousePosition : Maybe Point2d
    }


type Msg
    = Click
    | NewRandomVertices (List Vertex)
    | MouseMove Mouse.Event


svgDimensions : ( Float, Float )
svgDimensions =
    ( 700, 700 )


renderBounds : BoundingBox2d
renderBounds =
    BoundingBox2d.fromExtrema
        { minX = 0
        , maxX = 700
        , minY = 0
        , maxY = 700
        }


assignColors : List Point2d -> List Vertex
assignColors points =
    assignColorsHelp points [] []


assignColorsHelp : List Point2d -> List ( Float, Float, Float ) -> List Vertex -> List Vertex
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
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema renderBounds

        pointGenerator =
            Random.map2 (\x y -> Point2d.fromCoordinates ( x, y ))
                (Random.float (minX + 30) (maxX - 30))
                (Random.float (minY + 30) (maxY - 30))
    in
    Random.int 32 256
        |> Random.andThen (\listSize -> Random.list listSize pointGenerator)
        |> Random.map assignColors


collinearVerticesGenerator : Generator (List Vertex)
collinearVerticesGenerator =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema renderBounds

        pointGenerator =
            Random.map (\x -> Point2d.fromCoordinates ( x, x ))
                (Random.float (minX + 30) (maxX - 30))
    in
    Random.int 1 5
        |> Random.andThen (\listSize -> Random.list listSize pointGenerator)
        |> Random.map assignColors


generateNewVertices : Cmd Msg
generateNewVertices =
    Random.generate NewRandomVertices verticesGenerator


init : ( Model, Cmd Msg )
init =
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
                point =
                    Point2d.fromCoordinates event.offsetPos
            in
            ( { model | mousePosition = Just point }, Cmd.none )


fillColor : Vertex -> String
fillColor vertex =
    let
        ( r, g, b ) =
            vertex.color
    in
    String.concat
        [ "rgb("
        , String.fromInt (round (255 * r))
        , ","
        , String.fromInt (round (255 * g))
        , ","
        , String.fromInt (round (255 * b))
        , ")"
        ]


strokeColor : Vertex -> String
strokeColor vertex =
    let
        ( r, g, b ) =
            vertex.color

        darkenScale =
            0.8
    in
    String.concat
        [ "rgb("
        , String.fromInt (round (255 * r * darkenScale))
        , ","
        , String.fromInt (round (255 * g * darkenScale))
        , ","
        , String.fromInt (round (255 * b * darkenScale))
        , ")"
        ]


drawPolygon : ( Vertex, Polygon2d ) -> Svg msg
drawPolygon ( vertex, polygon ) =
    Svg.polygon2d
        [ Svg.Attributes.stroke (strokeColor vertex)
        , Svg.Attributes.fill (fillColor vertex)
        ]
        polygon


drawVertex : Vertex -> Svg msg
drawVertex vertex =
    Svg.circle2d [ Svg.Attributes.fill (strokeColor vertex) ]
        (Circle2d.withRadius 2.5 vertex.position)


view : Model -> Browser.Document Msg
view model =
    let
        voronoiDiagram =
            case model.mousePosition of
                Just point ->
                    let
                        vertex =
                            { position = point
                            , color = ( 1, 1, 1 )
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
                { minX = 150
                , maxX = 550
                , minY = 150
                , maxY = 550
                }

        polygons =
            voronoiDiagram
                |> Result.map (VoronoiDiagram2d.polygons trimBox)
                |> Result.withDefault []

        mousePointElement =
            case model.mousePosition of
                Just point ->
                    Svg.circle2d [ Svg.Attributes.fill "blue" ] (Circle2d.withRadius 2.5 point)

                Nothing ->
                    Svg.text ""

        ( width, height ) =
            svgDimensions

        overlayPolygon =
            Polygon2d.singleLoop
                [ Point2d.fromCoordinates ( 0, 0 )
                , Point2d.fromCoordinates ( width, 0 )
                , Point2d.fromCoordinates ( width, height )
                , Point2d.fromCoordinates ( 0, height )
                ]

        isInBounds vertex =
            BoundingBox2d.contains vertex.position trimBox
    in
    { title = "Voronoi Regions"
    , body =
        [ Html.div [ Html.Events.onClick Click ]
            [ Svg.svg
                [ Svg.Attributes.width (String.fromFloat width)
                , Svg.Attributes.height (String.fromFloat height)
                , Svg.Attributes.fill "white"
                , Svg.Attributes.stroke "black"
                , Html.Attributes.style "border" "1px solid black"
                , Mouse.onMove MouseMove
                ]
                [ Svg.g [] (List.map drawPolygon polygons)
                , Svg.g [] (List.map drawVertex (List.filter isInBounds vertices))
                , mousePointElement
                , Svg.polygon2d [ Svg.Attributes.fill "transparent" ] overlayPolygon
                ]
            ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
