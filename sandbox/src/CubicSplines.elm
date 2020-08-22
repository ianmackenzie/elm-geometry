module CubicSplines exposing (main)

import Browser
import Circle2d
import Color
import CubicSpline2d exposing (CubicSpline2d)
import Drawing2d
import Drawing2d.MouseInteraction as MouseInteraction exposing (MouseInteraction)
import Html exposing (Html)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Rectangle2d exposing (Rectangle2d)
import Vector2d exposing (Vector2d)


type DrawingCoordinates
    = DrawingCoordinates


type WhichPoint
    = P1
    | P2
    | P3
    | P4


type alias Drag =
    { whichPoint : WhichPoint
    , mouseInteraction : MouseInteraction DrawingCoordinates
    , offset : Vector2d Pixels DrawingCoordinates
    }


type alias Model =
    { p1 : Point2d Pixels DrawingCoordinates
    , p2 : Point2d Pixels DrawingCoordinates
    , p3 : Point2d Pixels DrawingCoordinates
    , p4 : Point2d Pixels DrawingCoordinates
    , currentDrag : Maybe Drag
    }


type Msg
    = MouseDown WhichPoint (Point2d Pixels DrawingCoordinates) (MouseInteraction DrawingCoordinates)
    | MouseUp
    | MouseMove (Point2d Pixels DrawingCoordinates)


init : () -> ( Model, Cmd Msg )
init () =
    ( { p1 = Point2d.pixels 100 100
      , p2 = Point2d.pixels 200 700
      , p3 = Point2d.pixels 600 100
      , p4 = Point2d.pixels 700 700
      , currentDrag = Nothing
      }
    , Cmd.none
    )


startDrag : Model -> WhichPoint -> Point2d Pixels DrawingCoordinates -> MouseInteraction DrawingCoordinates -> Drag
startDrag model whichPoint startPosition mouseInteraction =
    let
        originalPosition =
            case whichPoint of
                P1 ->
                    model.p1

                P2 ->
                    model.p2

                P3 ->
                    model.p3

                P4 ->
                    model.p4
    in
    { whichPoint = whichPoint
    , mouseInteraction = mouseInteraction
    , offset = Vector2d.from startPosition originalPosition
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MouseDown whichPoint startPosition mouseInteraction ->
            ( { model | currentDrag = Just (startDrag model whichPoint startPosition mouseInteraction) }
            , Cmd.none
            )

        MouseUp ->
            ( { model | currentDrag = Nothing }, Cmd.none )

        MouseMove mousePosition ->
            case model.currentDrag of
                Nothing ->
                    ( model, Cmd.none )

                Just drag ->
                    let
                        updatedPosition =
                            mousePosition |> Point2d.translateBy drag.offset
                    in
                    case drag.whichPoint of
                        P1 ->
                            ( { model | p1 = updatedPosition }, Cmd.none )

                        P2 ->
                            ( { model | p2 = updatedPosition }, Cmd.none )

                        P3 ->
                            ( { model | p3 = updatedPosition }, Cmd.none )

                        P4 ->
                            ( { model | p4 = updatedPosition }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentDrag of
        Just drag ->
            Sub.batch
                [ MouseInteraction.onMove MouseMove drag.mouseInteraction
                , MouseInteraction.onEnd MouseUp drag.mouseInteraction
                ]

        Nothing ->
            Sub.none


type alias DrawingElement =
    Drawing2d.Element Pixels DrawingCoordinates (Drawing2d.Event DrawingCoordinates Msg)


type alias DrawingAttribute =
    Drawing2d.Attribute Pixels DrawingCoordinates (Drawing2d.Event DrawingCoordinates Msg)


drawControlPoint :
    WhichPoint
    -> Point2d Pixels DrawingCoordinates
    -> DrawingElement
drawControlPoint whichPoint position =
    Drawing2d.circle
        [ Drawing2d.onLeftMouseDown (MouseDown whichPoint) ]
        (Circle2d.withRadius (Pixels.float 5) position)


drawVector : List DrawingAttribute -> Vector2d Pixels DrawingCoordinates -> DrawingElement
drawVector attributes vector =
    Drawing2d.lineSegment attributes
        (LineSegment2d.from Point2d.origin (Point2d.origin |> Point2d.translateBy vector))


view : Model -> Html Msg
view { p1, p2, p3, p4 } =
    let
        f1 =
            Vector2d.from p1 p2

        f2 =
            Vector2d.from p2 p3

        f3 =
            Vector2d.from p3 p4

        s1 =
            f2 |> Vector2d.minus f1

        s2 =
            f3 |> Vector2d.minus f2
    in
    Html.div []
        [ Drawing2d.toHtml
            { viewBox = Rectangle2d.from Point2d.origin (Point2d.pixels 800 800)
            , size = Drawing2d.fixed
            }
            []
            [ Drawing2d.cubicSpline [] <|
                CubicSpline2d.fromControlPoints p1 p2 p3 p4
            , Drawing2d.group []
                [ Drawing2d.lineSegment [ Drawing2d.strokeColor Color.green ] (LineSegment2d.from p1 p2)
                , Drawing2d.lineSegment [ Drawing2d.strokeColor Color.grey ] (LineSegment2d.from p2 p3)
                , Drawing2d.lineSegment [ Drawing2d.strokeColor Color.red ] (LineSegment2d.from p3 p4)
                ]
            , Drawing2d.group [ Drawing2d.whiteFill ]
                [ drawControlPoint P1 p1
                , drawControlPoint P2 p2
                , drawControlPoint P3 p3
                , drawControlPoint P4 p4
                ]
            ]
        , Drawing2d.toHtml
            { viewBox = Rectangle2d.from (Point2d.pixels -400 -400) (Point2d.pixels 400 400)
            , size = Drawing2d.fixed
            }
            []
            [ Drawing2d.quadraticSpline [] <|
                QuadraticSpline2d.fromControlPoints
                    (Point2d.origin |> Point2d.translateBy f1)
                    (Point2d.origin |> Point2d.translateBy f2)
                    (Point2d.origin |> Point2d.translateBy f3)
            , drawVector [ Drawing2d.strokeColor Color.green ] f1
            , drawVector [ Drawing2d.strokeColor Color.grey ] f2
            , drawVector [ Drawing2d.strokeColor Color.red ] f3
            ]

        -- , Drawing2d.toHtml
        --     { viewBox = Rectangle2d.from (Point2d.pixels -400 -400) (Point2d.pixels 400 400)
        --     , size = Drawing2d.fixed
        --     }
        --     []
        --     [ drawVector [ Drawing2d.strokeColor Color.green ] s1
        --     , drawVector [ Drawing2d.strokeColor Color.red ] s2
        --     ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
