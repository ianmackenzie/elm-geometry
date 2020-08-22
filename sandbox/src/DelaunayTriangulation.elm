--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module DelaunayTriangulation exposing (main)

import Array
import Axis2d
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Circle2d
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d, Error(..))
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
import Quantity.Interval as Interval
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)


type ScreenCoordinates
    = ScreenCoordinates


type alias Point =
    Point2d Pixels ScreenCoordinates


type alias Model =
    { baseTriangulation : Result (Error Point) (DelaunayTriangulation2d Point Pixels ScreenCoordinates)
    , mousePosition : Maybe Point
    }


type Msg
    = Click
    | NewRandomPoints (List Point)
    | MouseMove Mouse.Event


svgDimensions : ( Float, Float )
svgDimensions =
    ( 700, 700 )


renderBounds : BoundingBox2d Pixels ScreenCoordinates
renderBounds =
    BoundingBox2d.from Point2d.origin (Point2d.pixels 700 700)


pointsGenerator : Generator (List (Point2d Pixels ScreenCoordinates))
pointsGenerator =
    let
        ( xInterval, yInterval ) =
            BoundingBox2d.intervals renderBounds

        parameterGenerator =
            Random.float 0.05 0.95

        pointGenerator =
            Random.map2 Point2d.xy
                (Random.map (Interval.interpolate xInterval) parameterGenerator)
                (Random.map (Interval.interpolate yInterval) parameterGenerator)
    in
    Random.int 50 500
        |> Random.andThen
            (\listSize -> Random.list listSize pointGenerator)


generateNewPoints : Cmd Msg
generateNewPoints =
    Random.generate NewRandomPoints pointsGenerator


init : ( Model, Cmd Msg )
init =
    ( { baseTriangulation = Ok DelaunayTriangulation2d.empty
      , mousePosition = Nothing
      }
    , generateNewPoints
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewPoints )

        NewRandomPoints points ->
            let
                triangulation =
                    DelaunayTriangulation2d.fromPoints (Array.fromList points)
            in
            ( { model | baseTriangulation = triangulation }, Cmd.none )

        MouseMove event ->
            let
                point =
                    Point2d.fromTuple Pixels.float event.offsetPos
            in
            ( { model | mousePosition = Just point }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        triangulation =
            case model.mousePosition of
                Just point ->
                    model.baseTriangulation
                        |> Result.andThen
                            (DelaunayTriangulation2d.insertPoint point)

                Nothing ->
                    model.baseTriangulation

        triangles =
            triangulation
                |> Result.map DelaunayTriangulation2d.triangles
                |> Result.withDefault []

        points =
            triangulation
                |> Result.map (DelaunayTriangulation2d.vertices >> Array.toList)
                |> Result.withDefault []

        mousePointElement =
            case model.mousePosition of
                Just point ->
                    Svg.circle2d [ Svg.Attributes.fill "blue" ] (Circle2d.withRadius (Pixels.float 2.5) point)

                Nothing ->
                    Svg.text ""

        ( width, height ) =
            svgDimensions

        overlayPolygon =
            Polygon2d.singleLoop
                [ Point2d.pixels 0 0
                , Point2d.pixels width 0
                , Point2d.pixels width height
                , Point2d.pixels 0 height
                ]
    in
    { title = "Delaunay Triangulation"
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
                [ Svg.g [] (triangles |> List.map (Svg.triangle2d []))
                , Svg.g [] (points |> List.map (Circle2d.withRadius (Pixels.float 2.5) >> Svg.circle2d []))
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
