module VoronoiRegions exposing (..)

import Array
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Circle2d
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d)
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


type alias Model =
    { baseTriangulation : DelaunayTriangulation2d Point2d
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
    ( { baseTriangulation = DelaunayTriangulation2d.empty
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
                    Point2d.fromCoordinates event.offsetPos
            in
            ( { model | mousePosition = Just point }, Cmd.none )


drawFiniteRegion : { vertex : Point2d, region : Polygon2d } -> Svg msg
drawFiniteRegion region =
    Svg.polygon2d
        [ Svg.Attributes.stroke "black"
        , Svg.Attributes.fill "rgb(246,246,250)"
        ]
        region.region


drawInfiniteRegion : { vertex : Point2d, leftAxis : Axis2d, rightAxis : Axis2d, finitePortion : Polyline2d } -> Svg msg
drawInfiniteRegion region =
    let
        leftSegment =
            LineSegment2d.from
                (Axis2d.originPoint region.leftAxis)
                (Point2d.along region.leftAxis 1000)

        rightSegment =
            LineSegment2d.from
                (Axis2d.originPoint region.rightAxis)
                (Point2d.along region.rightAxis 1000)
    in
    Svg.g [ Svg.Attributes.stroke "blue" ]
        [ Svg.polyline2d [] region.finitePortion
        , Svg.lineSegment2d [ Svg.Attributes.strokeDasharray "3 3" ]
            leftSegment
        , Svg.lineSegment2d [ Svg.Attributes.strokeDasharray "3 3" ]
            rightSegment
        ]


view : Model -> Browser.Document Msg
view model =
    let
        triangulation =
            case model.mousePosition of
                Just point ->
                    model.baseTriangulation
                        |> DelaunayTriangulation2d.insertPoint point

                Nothing ->
                    model.baseTriangulation

        points =
            Array.toList (DelaunayTriangulation2d.vertices triangulation)

        voronoiRegions =
            DelaunayTriangulation2d.voronoiRegions triangulation

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
                [ Svg.g [] (List.map drawFiniteRegion voronoiRegions.finite)
                , Svg.g [] (List.map drawInfiniteRegion voronoiRegions.infinite)
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
