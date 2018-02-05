module MonotonePolygones exposing (..)

import Array.Hamt as Array
import Html exposing (Html)
import Html.Events
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d)
import OpenSolid.Polygon2d.Monotone as Monotone
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Random exposing (Generator)
import Svg
import Svg.Attributes


type alias Model =
    { polygon : Polygon2d
    }


type Msg
    = Click
    | NewPolygon Polygon2d


renderBounds : BoundingBox2d
renderBounds =
    BoundingBox2d.fromExtrema
        { minX = 0
        , maxX = 800
        , minY = 0
        , maxY = 600
        }


polygonGenerator : Generator Polygon2d
polygonGenerator =
    let
        centerPoint =
            BoundingBox2d.centroid renderBounds

        ( width, height ) =
            BoundingBox2d.dimensions renderBounds

        minRadius =
            10

        maxRadius =
            0.5 * min width height - 10

        radiusGenerator =
            Random.float minRadius maxRadius
    in
    Random.int 3 32
        |> Random.andThen
            (\numPoints ->
                Random.list numPoints radiusGenerator
                    |> Random.map
                        (List.indexedMap
                            (\index radius ->
                                let
                                    angle =
                                        turns 1
                                            * toFloat index
                                            / toFloat numPoints

                                    radialVector =
                                        Vector2d.fromPolarComponents
                                            ( radius
                                            , angle
                                            )
                                in
                                centerPoint |> Point2d.translateBy radialVector
                            )
                        )
                    |> Random.map Polygon2d.singleLoop
            )


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon polygonGenerator


init : ( Model, Cmd Msg )
init =
    ( { polygon = Polygon2d.singleLoop [] }, generateNewPolygon )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewPolygon )

        NewPolygon polygon ->
            ( { model | polygon = polygon }, Cmd.none )



--( Array Point2d, List (Array Int) )


view : Model -> Html Msg
view model =
    let
        ( vertices, loops ) =
            Monotone.polygons model.polygon

        drawLoop vertexIndices =
            vertexIndices
                |> Array.toList
                |> List.filterMap (\index -> Array.get index vertices)
                |> Polygon2d.singleLoop
                |> Svg.polygon2d
                    [ Svg.Attributes.fill "rgba(0, 127, 0, 0.25)"
                    , Svg.Attributes.stroke "rgb(0, 127, 0)"
                    ]
    in
    Html.div [ Html.Events.onClick Click ]
        [ Svg.render2d renderBounds <|
            Svg.g []
                [ Svg.g [] (List.map drawLoop loops)
                , Svg.polygon2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "black"
                    ]
                    model.polygon
                ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
