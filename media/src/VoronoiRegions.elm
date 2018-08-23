module VoronoiRegions exposing (..)

import Array
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Circle2d
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
import VoronoiDiagram2d exposing (VoronoiDiagram2d)


type alias Model =
    { baseDiagram : VoronoiDiagram2d Point2d
    , mousePosition : Maybe Point2d
    }


type Msg
    = Click
    | NewRandomPoints (List Point2d)
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


pointsGenerator : Generator (List Point2d)
pointsGenerator =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema renderBounds

        pointGenerator =
            Random.map2 (\x y -> Point2d.fromCoordinates ( x, y ))
                (Random.float (minX + 30) (maxX - 30))
                (Random.float (minY + 30) (maxY - 30))
    in
    Random.int 3 500
        |> Random.andThen
            (\listSize -> Random.list listSize pointGenerator)


generateNewPoints : Cmd Msg
generateNewPoints =
    Random.generate NewRandomPoints pointsGenerator


init : ( Model, Cmd Msg )
init =
    ( { baseDiagram = VoronoiDiagram2d.empty
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
                diagram =
                    VoronoiDiagram2d.fromPoints (Array.fromList points)
            in
            ( { model | baseDiagram = diagram }, Cmd.none )

        MouseMove event ->
            let
                point =
                    Point2d.fromCoordinates event.offsetPos
            in
            ( { model | mousePosition = Just point }, Cmd.none )


drawPolygon : Polygon2d -> Svg msg
drawPolygon polygon =
    Svg.polygon2d
        [ Svg.Attributes.stroke "black"
        , Svg.Attributes.fill "rgb(246,246,250)"
        ]
        polygon


view : Model -> Browser.Document Msg
view model =
    let
        voronoiDiagram =
            case model.mousePosition of
                Just point ->
                    model.baseDiagram
                        |> VoronoiDiagram2d.insertPoint point

                Nothing ->
                    model.baseDiagram

        points =
            Array.toList (VoronoiDiagram2d.vertices voronoiDiagram)

        trimBox =
            BoundingBox2d.fromExtrema
                { minX = 150
                , maxX = 550
                , minY = 150
                , maxY = 550
                }

        polygons =
            VoronoiDiagram2d.polygons trimBox voronoiDiagram

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
                [ Svg.g [] (List.map (Tuple.second >> drawPolygon) polygons)
                , Svg.g [] (points |> List.map (Circle2d.withRadius 2.5 >> Svg.circle2d []))
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
