module DistanceAlongAxis2dTest exposing (Program, program)

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Events
import Circle2d
import Color exposing (Color)
import Direction2d exposing (perpendicularTo)
import Drawing2d
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import Random2d
import Rectangle2d exposing (Rectangle2d)


type DrawingCoordinates
    = DrawingCoordinates


type alias DrawingEvent =
    Drawing2d.Event DrawingCoordinates Msg


type alias Model =
    { drawScene : Int -> Drawing2d.Element Pixels DrawingCoordinates DrawingEvent
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
            [ model.drawScene model.index
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


type alias DistanceFunction a =
    Axis2d Pixels DrawingCoordinates -> a -> Interval Float Pixels


type alias DrawFunction a =
    a -> Drawing2d.Element Pixels DrawingCoordinates (Drawing2d.Event DrawingCoordinates Msg)


type alias Program =
    Platform.Program () Model Msg


drawAxis : Color -> Axis2d Pixels DrawingCoordinates -> Drawing2d.Element Pixels DrawingCoordinates event
drawAxis color axis =
    let
        originPoint =
            Axis2d.originPoint axis
    in
    Drawing2d.group []
        [ Drawing2d.lineSegment [ Drawing2d.strokeColor color ]
            (LineSegment2d.along axis (Pixels.float -1000) (Pixels.float 1000))
        ]


drawScene : Generator a -> DistanceFunction a -> DrawFunction a -> Int -> Drawing2d.Element Pixels DrawingCoordinates (Drawing2d.Event DrawingCoordinates Msg)
drawScene generator distanceFunction drawFunction index =
    let
        value =
            randomValue generator index

        axis =
            randomValue (Random2d.axis viewBounds) index

        signedDistance =
            distanceFunction axis value

        perpendicularDirection =
            Direction2d.perpendicularTo (Axis2d.direction axis)

        minAxis =
            Axis2d.through (Point2d.along axis (Interval.minValue signedDistance))
                perpendicularDirection

        maxAxis =
            Axis2d.through (Point2d.along axis (Interval.maxValue signedDistance))
                perpendicularDirection
    in
    Drawing2d.group []
        [ drawAxis Color.green axis
        , drawFunction value
        , drawAxis Color.blue minAxis
        , drawAxis Color.blue maxAxis
        ]


program : GeneratorFunction a -> DistanceFunction a -> DrawFunction a -> Program
program generatorFunction distanceFunction drawFunction =
    let
        generator =
            generatorFunction viewBounds
    in
    Browser.element
        { init =
            \() ->
                ( { drawScene = drawScene generator distanceFunction drawFunction
                  , debugText = randomValue generator >> Debug.toString
                  , index = 0
                  }
                , Cmd.none
                )
        , update = update
        , view = view
        , subscriptions = always subscriptions
        }
