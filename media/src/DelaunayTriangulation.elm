module DelaunayTriangulation exposing (..)

import Array
import Axis2d
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
    Random.int 50 500
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



--drawFiniteRegion : Delaunay.VoronoiRegion -> Maybe (Svg msg)
--drawFiniteRegion region =
--    case region of
--        Delaunay.FiniteRegion polygon ->
--            Just <| Svg.polygon2d [ Svg.Attributes.stroke "black", Svg.Attributes.fill "rgb(246,246,250)" ] polygon
--        _ ->
--            Nothing
--drawInfiniteRegion : Delaunay.VoronoiRegion -> Maybe (Svg msg)
--drawInfiniteRegion region =
--    case region of
--        Delaunay.InfiniteRegion polyline startDirection endDirection ->
--            case Polyline2d.vertices polyline of
--                startPoint :: rest ->
--                    let
--                        endPoint =
--                            List.foldl always startPoint rest
--                        startAxis =
--                            Axis2d.through startPoint startDirection
--                        endAxis =
--                            Axis2d.through endPoint endDirection
--                    in
--                    Just <|
--                        Svg.g [ Svg.Attributes.stroke "blue" ]
--                            [ Svg.polyline2d [] polyline
--                            , Svg.lineSegment2d [ Svg.Attributes.strokeDasharray "3 3" ]
--                                (LineSegment2d.from startPoint (Point2d.along startAxis 700))
--                            , Svg.lineSegment2d [ Svg.Attributes.strokeDasharray "3 3" ]
--                                (LineSegment2d.from endPoint (Point2d.along endAxis 700))
--                            ]
--                [] ->
--                    Nothing
--        _ ->
--            Nothing


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

        triangles =
            DelaunayTriangulation2d.triangles triangulation

        points =
            Array.toList (DelaunayTriangulation2d.vertices triangulation)

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
