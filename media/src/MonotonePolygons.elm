--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module MonotonePolygones exposing (main)

import Array.Hamt as Array
import BoundingBox2d exposing (BoundingBox2d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Kintail.InputWidget as InputWidget
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polygon2d.Monotone as Monotone
import Random exposing (Generator)
import Svg
import Svg.Attributes
import Triangle2d exposing (Triangle2d)
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
        , maxY = 600
        }


polygonGenerator : Generator Polygon2d
polygonGenerator =
    let
        centerPoint =
            BoundingBox2d.centerPoint renderBounds

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
            Polygon2d.rotateAround (BoundingBox2d.centerPoint renderBounds)
                (degrees model.angleInDegrees)
                model.polygon

        ( points, loops ) =
            Monotone.monotonePolygons rotatedPolygon

        numLoops =
            List.length loops

        drawLoop loopIndex vertices =
            let
                hueString =
                    toString (360 * toFloat loopIndex / toFloat numLoops)

                fillColor =
                    "hsla(" ++ hueString ++ ",50%, 50%, 0.5)"

                strokeColor =
                    "hsla(" ++ hueString ++ ",50%, 40%, 0.5)"

                faceIndices =
                    Monotone.faces vertices

                triangles =
                    faceIndices
                        |> List.filterMap
                            (\( i, j, k ) ->
                                Maybe.map3
                                    (\p1 p2 p3 ->
                                        Triangle2d.fromVertices ( p1, p2, p3 )
                                    )
                                    (Array.get i points)
                                    (Array.get j points)
                                    (Array.get k points)
                            )
            in
            triangles
                |> List.map
                    (Svg.triangle2d
                        [ Svg.Attributes.fill fillColor
                        , Svg.Attributes.stroke strokeColor
                        ]
                    )
                |> Svg.g []
    in
    Html.div []
        [ Html.div [ Html.Events.onClick Click ]
            [ Svg.render2d renderBounds <|
                Svg.g []
                    [ Svg.g [] (List.indexedMap drawLoop loops)
                    , Svg.polygon2d
                        [ Svg.Attributes.fill "none"
                        , Svg.Attributes.stroke "black"
                        ]
                        rotatedPolygon
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
