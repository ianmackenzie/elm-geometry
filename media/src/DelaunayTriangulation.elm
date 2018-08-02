module DelaunayTriangulation exposing (..)

import Array
import Axis2d
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Events
import LineSegment2d
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes
import Tesselation2d.Delaunay2 as Delaunay
import Triangle2d exposing (Triangle2d)


type alias Model =
    { points : List Point2d
    }


type Msg
    = Click
    | NewRandomPoints (List Point2d)


renderBounds : BoundingBox2d
renderBounds =
    BoundingBox2d.fromExtrema
        { minX = 200
        , maxX = 500
        , minY = 200
        , maxY = 500
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
    Random.int 2 32
        |> Random.andThen
            (\listSize -> Random.list listSize pointGenerator)


generateNewPoints : Cmd Msg
generateNewPoints =
    Random.generate NewRandomPoints pointsGenerator


init : ( Model, Cmd Msg )
init =
    ( { points = [] }, generateNewPoints )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewPoints )

        NewRandomPoints points ->
            ( { model | points = points }, Cmd.none )


getTriangle : Delaunay.Face -> Maybe Triangle2d
getTriangle face =
    case face of
        Delaunay.ThreeVertexFace vertex1 vertex2 vertex3 _ ->
            Just <|
                Triangle2d.fromVertices
                    ( vertex1.position
                    , vertex2.position
                    , vertex3.position
                    )

        _ ->
            Nothing


drawFiniteRegion : Delaunay.VoronoiRegion -> Maybe (Svg msg)
drawFiniteRegion region =
    case region of
        Delaunay.FiniteRegion polygon ->
            Just <| Svg.polygon2d [ Svg.Attributes.stroke "black", Svg.Attributes.fill "rgb(246,246,250)" ] polygon

        _ ->
            Nothing


drawInfiniteRegion : Delaunay.VoronoiRegion -> Maybe (Svg msg)
drawInfiniteRegion region =
    case region of
        Delaunay.InfiniteRegion polyline startDirection endDirection ->
            case Polyline2d.vertices polyline of
                startPoint :: rest ->
                    let
                        endPoint =
                            List.foldl always startPoint rest

                        startAxis =
                            Axis2d.through startPoint startDirection

                        endAxis =
                            Axis2d.through endPoint endDirection
                    in
                    Just <|
                        Svg.g [ Svg.Attributes.stroke "blue" ]
                            [ Svg.polyline2d [] polyline
                            , Svg.lineSegment2d [ Svg.Attributes.strokeDasharray "3 3" ]
                                (LineSegment2d.from startPoint (Point2d.along startAxis 100))
                            , Svg.lineSegment2d [ Svg.Attributes.strokeDasharray "3 3" ]
                                (LineSegment2d.from endPoint (Point2d.along endAxis 100))
                            ]

                [] ->
                    Nothing

        _ ->
            Nothing


view : Model -> Browser.Document Msg
view model =
    let
        pointArray =
            Array.fromList model.points

        delaunayTriangles =
            Delaunay.triangulate pointArray
                |> Tuple.second
                |> List.filterMap getTriangle

        voronoiRegions =
            Delaunay.voronoiRegions pointArray
    in
    { title = "Delaunay Triangulation"
    , body =
        [ Html.div [ Html.Events.onClick Click ]
            [ Svg.svg
                [ Svg.Attributes.width "700"
                , Svg.Attributes.height "700"
                , Svg.Attributes.fill "white"
                , Svg.Attributes.stroke "black"
                ]
                [ Svg.g [] (List.filterMap drawFiniteRegion voronoiRegions)
                , Svg.g [] (List.filterMap drawInfiniteRegion voronoiRegions)
                , Svg.g [] (List.map (\point -> Svg.circle2d [] (Circle2d.withRadius 2.5 point)) model.points)
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
