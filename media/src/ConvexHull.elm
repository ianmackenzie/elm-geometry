--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module ConvexHull exposing (main)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Circle2d
import Drawing2d
import Html exposing (Html)
import Html.Events
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d
import Quantity.Interval as Interval
import Random exposing (Generator)
import Rectangle2d


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { points : List (Point2d Pixels ScreenCoordinates)
    }


type Msg
    = Click
    | NewRandomPoints (List (Point2d Pixels ScreenCoordinates))


renderBounds : BoundingBox2d Pixels ScreenCoordinates
renderBounds =
    BoundingBox2d.from Point2d.origin (Point2d.pixels 300 300)


pointsGenerator : Generator (List (Point2d Pixels ScreenCoordinates))
pointsGenerator =
    let
        ( xInterval, yInterval ) =
            BoundingBox2d.intervals renderBounds

        parameterGenerator =
            Random.float 0.05 0.95

        pointGenerator =
            Random.map2 Point2d.xy
                (Random.map (Interval.interpolate xInterval) parameterGenerator)
                (Random.map (Interval.interpolate yInterval) parameterGenerator)
    in
    Random.int 2 32
        |> Random.andThen
            (\listSize -> Random.list listSize pointGenerator)


generateNewPoints : Cmd Msg
generateNewPoints =
    Random.generate NewRandomPoints pointsGenerator


init : () -> ( Model, Cmd Msg )
init () =
    ( { points = [] }, generateNewPoints )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewPoints )

        NewRandomPoints points ->
            ( { model | points = points }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        convexHull =
            Polygon2d.convexHull model.points
    in
    Html.div [ Html.Events.onClick Click ]
        [ Drawing2d.toHtml
            { size = Drawing2d.fixed
            , viewBox = Rectangle2d.fromBoundingBox renderBounds
            }
            []
            [ Drawing2d.polygon [] convexHull
            , Drawing2d.group [] <|
                (model.points
                    |> List.map
                        (\point -> Drawing2d.circle [] (Circle2d.withRadius (Pixels.float 3) point))
                )
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
