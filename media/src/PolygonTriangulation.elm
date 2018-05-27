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
import Random.Pcg as Random exposing (Generator)
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
        , maxX = 800
        , minY = 0
        , maxY = 800
        }


radialPolygonWithHole : BoundingBox2d -> Generator Polygon2d
radialPolygonWithHole boundingBox =
    let
        centerPoint =
            BoundingBox2d.centroid boundingBox

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox

        minRadius =
            0.05 * min width height

        maxRadius =
            0.5 * min width height - minRadius

        midRadius =
            (minRadius + maxRadius) / 2

        innerRadiusGenerator =
            Random.float minRadius (midRadius - 5)

        outerRadiusGenerator =
            Random.float (midRadius + 5) maxRadius
    in
    Random.int 3 32
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


type alias GridPolygon =
    { outerLoop : List ( Int, Int )
    , innerLoops : List (List ( Int, Int ))
    }


localCoordinates : Generator ( Float, Float )
localCoordinates =
    Random.map2 (,) (Random.float 0.1 0.9) (Random.float 0.1 0.9)


loopPoints : BoundingBox2d -> List ( Int, Int ) -> Generator (List Point2d)
loopPoints boundingBox gridCoordinates =
    let
        { minX, minY } =
            BoundingBox2d.extrema boundingBox

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox

        xStart =
            minX + width * 0.15

        yStart =
            minY + width * 0.15

        xStep =
            width * 0.7 / 8

        yStep =
            height * 0.7 / 8
    in
    Random.list (List.length gridCoordinates) localCoordinates
        |> Random.map
            (\localCoordinatesList ->
                List.map2
                    (\( i, j ) ( u, v ) ->
                        Point2d.fromCoordinates
                            ( xStart + toFloat i * xStep + u * xStep
                            , yStart + toFloat j * yStep + v * yStep
                            )
                    )
                    gridCoordinates
                    localCoordinatesList
            )


squareOuterLoop : List ( Int, Int )
squareOuterLoop =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    , ( 3, 0 )
    , ( 4, 0 )
    , ( 5, 0 )
    , ( 6, 0 )
    , ( 7, 0 )
    , ( 7, 1 )
    , ( 7, 2 )
    , ( 7, 3 )
    , ( 7, 4 )
    , ( 7, 5 )
    , ( 7, 6 )
    , ( 7, 7 )
    , ( 6, 7 )
    , ( 5, 7 )
    , ( 4, 7 )
    , ( 3, 7 )
    , ( 2, 7 )
    , ( 1, 7 )
    , ( 0, 7 )
    , ( 0, 6 )
    , ( 0, 5 )
    , ( 0, 4 )
    , ( 0, 3 )
    , ( 0, 2 )
    , ( 0, 1 )
    ]


squarish : GridPolygon
squarish =
    { outerLoop = squareOuterLoop
    , innerLoops = []
    }


lShaped : GridPolygon
lShaped =
    { outerLoop =
        [ ( 0, 0 )
        , ( 1, 0 )
        , ( 2, 0 )
        , ( 3, 0 )
        , ( 4, 0 )
        , ( 5, 0 )
        , ( 6, 0 )
        , ( 7, 0 )
        , ( 7, 1 )
        , ( 7, 2 )
        , ( 7, 3 )
        , ( 7, 4 )
        , ( 7, 5 )
        , ( 7, 6 )
        , ( 7, 7 )
        , ( 6, 7 )
        , ( 5, 7 )
        , ( 4, 7 )
        , ( 4, 6 )
        , ( 4, 5 )
        , ( 4, 4 )
        , ( 4, 3 )
        , ( 3, 3 )
        , ( 2, 3 )
        , ( 1, 3 )
        , ( 0, 3 )
        , ( 0, 2 )
        , ( 0, 1 )
        ]
    , innerLoops = []
    }


squareWithHole : GridPolygon
squareWithHole =
    { outerLoop = squareOuterLoop
    , innerLoops =
        [ [ ( 2, 2 )
          , ( 3, 2 )
          , ( 4, 2 )
          , ( 5, 2 )
          , ( 5, 3 )
          , ( 5, 4 )
          , ( 5, 5 )
          , ( 4, 5 )
          , ( 3, 5 )
          , ( 2, 5 )
          , ( 2, 4 )
          , ( 2, 3 )
          ]
        ]
    }


squareWithTwoHoles : GridPolygon
squareWithTwoHoles =
    { outerLoop = squareOuterLoop
    , innerLoops =
        [ [ ( 1, 1 )
          , ( 1, 2 )
          , ( 2, 2 )
          , ( 3, 2 )
          , ( 4, 2 )
          , ( 5, 2 )
          , ( 5, 3 )
          , ( 5, 4 )
          , ( 5, 5 )
          , ( 5, 6 )
          , ( 6, 6 )
          , ( 6, 5 )
          , ( 6, 4 )
          , ( 6, 3 )
          , ( 6, 2 )
          , ( 6, 1 )
          , ( 5, 1 )
          , ( 4, 1 )
          , ( 3, 1 )
          , ( 2, 1 )
          ]
        , [ ( 1, 4 )
          , ( 1, 5 )
          , ( 1, 6 )
          , ( 2, 6 )
          , ( 3, 6 )
          , ( 3, 5 )
          , ( 3, 4 )
          , ( 2, 4 )
          ]
        ]
    }


interlocking : GridPolygon
interlocking =
    { outerLoop = squareOuterLoop
    , innerLoops =
        [ [ ( 1, 1 )
          , ( 1, 2 )
          , ( 2, 2 )
          , ( 3, 2 )
          , ( 3, 3 )
          , ( 3, 4 )
          , ( 4, 4 )
          , ( 4, 3 )
          , ( 4, 2 )
          , ( 5, 2 )
          , ( 6, 2 )
          , ( 6, 1 )
          , ( 5, 1 )
          , ( 4, 1 )
          , ( 3, 1 )
          , ( 2, 1 )
          ]
        , [ ( 1, 3 )
          , ( 1, 4 )
          , ( 1, 5 )
          , ( 1, 6 )
          , ( 2, 6 )
          , ( 3, 6 )
          , ( 4, 6 )
          , ( 5, 6 )
          , ( 6, 6 )
          , ( 6, 5 )
          , ( 6, 4 )
          , ( 6, 3 )
          , ( 5, 3 )
          , ( 5, 4 )
          , ( 5, 5 )
          , ( 4, 5 )
          , ( 3, 5 )
          , ( 2, 5 )
          , ( 2, 4 )
          , ( 2, 3 )
          ]
        ]
    }


join : List (Generator a) -> Generator (List a)
join generators =
    case generators of
        [] ->
            Random.constant []

        first :: rest ->
            Random.map2 (::) first (join rest)


gridPolygon : BoundingBox2d -> GridPolygon -> Generator Polygon2d
gridPolygon boundingBox { outerLoop, innerLoops } =
    let
        outerLoopGenerator =
            loopPoints boundingBox outerLoop

        innerLoopGenerators =
            List.map (loopPoints boundingBox) innerLoops
    in
    Random.map2
        (\outerLoopPoints innerLoopPoints ->
            Polygon2d.with
                { outerLoop = outerLoopPoints
                , innerLoops = innerLoopPoints
                }
        )
        outerLoopGenerator
        (join innerLoopGenerators)


polygonGenerator : BoundingBox2d -> Generator Polygon2d
polygonGenerator boundingBox =
    Random.map2
        (\polygon angle ->
            polygon
                |> Polygon2d.rotateAround (BoundingBox2d.centroid boundingBox)
                    angle
        )
        (Random.choices
            [ radialPolygonWithHole boundingBox
            , gridPolygon boundingBox squarish
            , gridPolygon boundingBox lShaped
            , gridPolygon boundingBox squareWithHole
            , gridPolygon boundingBox squareWithTwoHoles
            , gridPolygon boundingBox interlocking
            ]
        )
        (Random.float -pi pi)


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon (polygonGenerator renderBounds)


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
                [ Attributes.fillColor (Color.rgba 192 192 192 0.25)
                , Attributes.strokeColor (Color.rgb 192 192 192)
                ]

        polygonElement =
            Drawing2d.polygonWith
                [ Attributes.noFill, Attributes.blackStroke ]
                rotatedPolygon
    in
    Html.div []
        [ Html.div [ Html.Events.onClick Click ]
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
