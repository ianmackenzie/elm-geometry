module DelaunayTriangulation exposing (..)

import Array
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Events
import Point2d exposing (Point2d)
import Random exposing (Generator)
import Svg
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
        { minX = 0
        , maxX = 300
        , minY = 0
        , maxY = 300
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


view : Model -> Browser.Document Msg
view model =
    let
        faces =
            Delaunay.triangulation (Array.fromList model.points)
                |> List.filterMap getTriangle
    in
    { title = "Delaunay Triangulation"
    , body =
        [ Html.div [ Html.Events.onClick Click ]
            [ Svg.svg
                [ Svg.Attributes.width "300"
                , Svg.Attributes.height "300"
                , Svg.Attributes.fill "white"
                , Svg.Attributes.stroke "black"
                ]
                [ Svg.g [] (List.map (Svg.triangle2d []) faces)
                , Svg.g [] (List.map (\point -> Svg.circle2d [] (Circle2d.withRadius 2 point)) model.points)
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
