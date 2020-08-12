--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module MonotonePolygons exposing (main)

import Angle exposing (Angle)
import Array
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Color
import Drawing2d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import InputWidget as InputWidget
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polygon2d.Monotone as Monotone
import Quantity
import Random exposing (Generator)
import Rectangle2d
import Triangle2d exposing (Triangle2d)
import Vector2d exposing (Vector2d)


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { polygon : Polygon2d Pixels ScreenCoordinates
    , angle : Angle
    }


type Msg
    = Click
    | NewPolygon (Polygon2d Pixels ScreenCoordinates)
    | SetAngle Angle


renderBounds : BoundingBox2d Pixels ScreenCoordinates
renderBounds =
    BoundingBox2d.from Point2d.origin (Point2d.pixels 800 800)


polygonGenerator : Generator (Polygon2d Pixels ScreenCoordinates)
polygonGenerator =
    let
        centerPoint =
            BoundingBox2d.centerPoint renderBounds

        ( width, height ) =
            BoundingBox2d.dimensions renderBounds

        minRadius =
            Pixels.float 10

        maxRadius =
            Quantity.half (Quantity.min width height) |> Quantity.minus (Pixels.float 10)

        midRadius =
            Quantity.midpoint minRadius maxRadius

        parameterGenerator =
            Random.float 0 1

        innerRadiusGenerator =
            Random.map (Quantity.interpolateFrom minRadius (midRadius |> Quantity.minus (Pixels.float 5))) parameterGenerator

        outerRadiusGenerator =
            Random.map (Quantity.interpolateFrom (midRadius |> Quantity.plus (Pixels.float 5)) maxRadius) parameterGenerator
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
                                        Angle.turns 1
                                            |> Quantity.multiplyBy (toFloat index / toFloat numPoints)

                                    innerRadialVector =
                                        Vector2d.rTheta innerRadius angle

                                    outerRadialVector =
                                        Vector2d.rTheta outerRadius angle

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
                            Polygon2d.withHoles [ List.reverse innerLoop ] outerLoop
                        )
            )


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon polygonGenerator


init : () -> ( Model, Cmd Msg )
init () =
    ( { polygon = Polygon2d.singleLoop [], angle = Angle.degrees 0 }
    , generateNewPolygon
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewPolygon )

        NewPolygon polygon ->
            ( { model | polygon = polygon }, Cmd.none )

        SetAngle angle ->
            ( { model | angle = angle }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( width, height ) =
            BoundingBox2d.dimensions renderBounds

        rotatedPolygon =
            Polygon2d.rotateAround (BoundingBox2d.centerPoint renderBounds)
                model.angle
                model.polygon

        ( points, loops ) =
            Monotone.monotonePolygons rotatedPolygon

        numLoops =
            List.length loops

        drawLoop loopIndex vertices =
            let
                hue =
                    toFloat loopIndex / toFloat numLoops

                fillColor =
                    Color.hsla hue 0.5 0.5 0.5

                strokeColor =
                    Color.hsla hue 0.5 0.4 0.5

                faceIndices =
                    Monotone.faces vertices

                triangles =
                    faceIndices
                        |> List.filterMap
                            (\( i, j, k ) ->
                                Maybe.map3 Triangle2d.from
                                    (Array.get i points)
                                    (Array.get j points)
                                    (Array.get k points)
                            )
            in
            triangles
                |> List.map
                    (Drawing2d.triangle
                        [ Drawing2d.fillColor fillColor
                        , Drawing2d.strokeColor strokeColor
                        ]
                    )
                |> Drawing2d.group []
    in
    Html.div []
        [ Html.div [ Html.Events.onClick Click ]
            [ Drawing2d.toHtml
                { viewBox = Rectangle2d.fromBoundingBox renderBounds
                , size = Drawing2d.fixed
                }
                []
                [ Drawing2d.group []
                    [ Drawing2d.group [] (List.indexedMap drawLoop loops)
                    , Drawing2d.polygon
                        [ Drawing2d.noFill
                        , Drawing2d.blackStroke
                        ]
                        rotatedPolygon
                    ]
                ]
            ]
        , InputWidget.slider
            [ Html.Attributes.style "width" (String.fromFloat (Pixels.toFloat width) ++ "px") ]
            { min = -180, max = 180, step = 1 }
            (Angle.inDegrees model.angle)
            |> Html.map (Angle.degrees >> SetAngle)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
