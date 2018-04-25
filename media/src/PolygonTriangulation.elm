module PolygonTriangulation exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Color
import Drawing2d
import Drawing2d.Attributes as Attributes
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Kintail.InputWidget as InputWidget
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Random exposing (Generator)
import Triangle2d exposing (Triangle2d)
import TriangularMesh
import Vector2d exposing (Vector2d)


type alias Model =
    { polygon : Polygon2d
    , angleInDegrees : Float
    }


type Msg
    = Click
    | NewPolygon Polygon2d
    | SetAngleInDegrees Float


renderBounds : BoundingBox2d
renderBounds =
    BoundingBox2d.fromExtrema
        { minX = 0
        , maxX = 300
        , minY = 0
        , maxY = 300
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

        midRadius =
            (minRadius + maxRadius) / 2

        innerRadiusGenerator =
            Random.float minRadius (midRadius - 5)

        outerRadiusGenerator =
            Random.float (midRadius + 5) maxRadius
    in
    Random.int 5 10
        |> Random.andThen
            (\numPoints ->
                Random.list numPoints
                    (Random.pair innerRadiusGenerator outerRadiusGenerator)
                    |> Random.map
                        (List.indexedMap
                            (\index ( innerRadius, outerRadius ) ->
                                let
                                    angle =
                                        turns 1
                                            * toFloat index
                                            / toFloat numPoints

                                    innerRadialVector =
                                        Vector2d.fromPolarComponents
                                            ( innerRadius
                                            , angle
                                            )

                                    outerRadialVector =
                                        Vector2d.fromPolarComponents
                                            ( outerRadius
                                            , angle
                                            )

                                    innerPoint =
                                        centerPoint
                                            |> Point2d.translateBy
                                                innerRadialVector

                                    outerPoint =
                                        centerPoint
                                            |> Point2d.translateBy
                                                outerRadialVector
                                in
                                ( innerPoint, outerPoint )
                            )
                        )
                    |> Random.map List.unzip
                    |> Random.map
                        (\( innerLoop, outerLoop ) ->
                            Polygon2d.with
                                { outerLoop = outerLoop
                                , innerLoops = [ List.reverse innerLoop ]
                                }
                        )
            )


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon polygonGenerator


init : ( Model, Cmd Msg )
init =
    ( { polygon = Polygon2d.singleLoop [], angleInDegrees = 0 }
    , generateNewPolygon
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewPolygon )

        NewPolygon polygon ->
            ( { model | polygon = polygon }, Cmd.none )

        SetAngleInDegrees angleInDegrees ->
            ( { model | angleInDegrees = angleInDegrees }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( width, height ) =
            BoundingBox2d.dimensions renderBounds

        rotatedPolygon =
            Polygon2d.rotateAround (BoundingBox2d.centroid renderBounds)
                (degrees model.angleInDegrees)
                model.polygon

        mesh =
            Polygon2d.triangulate rotatedPolygon

        triangles =
            TriangularMesh.faceVertices mesh
                |> List.map Triangle2d.fromVertices

        drawTriangle =
            Drawing2d.triangleWith
                [ Attributes.fillColor (Color.rgba 127 127 127 0.25)
                , Attributes.strokeColor (Color.rgb 127 127 127)
                ]

        polygonElement =
            Drawing2d.polygonWith
                [ Attributes.noFill, Attributes.blackStroke ]
                rotatedPolygon
    in
    Html.div []
        [ Html.div [ Html.Events.onClick Click ]
            [ Drawing2d.toHtml renderBounds [] <|
                [ polygonElement
                ]
            ]
        , Html.div [ Html.Events.onClick Click ]
            [ Drawing2d.toHtml renderBounds [] <|
                [ Drawing2d.group (List.map drawTriangle triangles)
                , polygonElement
                ]
            ]
        , InputWidget.slider
            [ Html.Attributes.style [ ( "width", toString width ++ "px" ) ] ]
            { min = -180, max = 180, step = 1 }
            model.angleInDegrees
            |> Html.map SetAngleInDegrees
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
