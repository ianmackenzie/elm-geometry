module BoundingBox2dTest exposing (Program, program)

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Events
import Color
import Drawing2d
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Random exposing (Generator)
import Random2d
import Rectangle2d exposing (Rectangle2d)


type DrawingCoordinates
    = DrawingCoordinates


type alias DrawingEvent =
    Drawing2d.Event DrawingCoordinates Msg


type alias Model =
    { drawValue : Int -> Drawing2d.Element Pixels DrawingCoordinates DrawingEvent
    , drawBounds : Int -> Drawing2d.Element Pixels DrawingCoordinates DrawingEvent
    , debugText : Int -> String
    , index : Int
    }


type Msg
    = Next
    | Previous


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Next ->
            ( { model | index = model.index + 1 }, Cmd.none )

        Previous ->
            ( { model | index = max (model.index - 1) 0 }, Cmd.none )


viewBounds : BoundingBox2d Pixels DrawingCoordinates
viewBounds =
    BoundingBox2d.from Point2d.origin (Point2d.pixels 400 400)


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text ("Test case " ++ String.fromInt model.index)
        , Drawing2d.toHtml
            { viewBox = Rectangle2d.fromBoundingBox viewBounds
            , size = Drawing2d.fixed
            }
            []
            [ model.drawBounds model.index
            , model.drawValue model.index
            ]
        , Html.text (model.debugText model.index)
        ]


subscriptions : Sub Msg
subscriptions =
    Browser.Events.onKeyDown
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    case key of
                        "ArrowLeft" ->
                            Decode.succeed Previous

                        "ArrowRight" ->
                            Decode.succeed Next

                        "ArrowUp" ->
                            Decode.succeed Previous

                        "ArrowDown" ->
                            Decode.succeed Next

                        _ ->
                            Decode.fail "Unrecognized key"
                )
        )


randomValue : Generator a -> Int -> a
randomValue generator current =
    Tuple.first (Random.step generator (Random.initialSeed current))


type alias GeneratorFunction a =
    BoundingBox2d Pixels DrawingCoordinates -> Generator a


type alias BoundsFunction a =
    a -> BoundingBox2d Pixels DrawingCoordinates


type alias DrawFunction a =
    a -> Drawing2d.Element Pixels DrawingCoordinates (Drawing2d.Event DrawingCoordinates Msg)


type alias Program =
    Platform.Program () Model Msg


program : GeneratorFunction a -> BoundsFunction a -> DrawFunction a -> Program
program generatorFunction boundsFunction drawFunction =
    let
        generator =
            generatorFunction viewBounds
    in
    Browser.element
        { init =
            \() ->
                ( { drawValue = randomValue generator >> drawFunction
                  , drawBounds = randomValue generator >> boundsFunction >> Drawing2d.boundingBox [ Drawing2d.strokeColor Color.green ]
                  , debugText = randomValue generator >> Debug.toString
                  , index = 0
                  }
                , Cmd.none
                )
        , update = update
        , view = view
        , subscriptions = always subscriptions
        }
