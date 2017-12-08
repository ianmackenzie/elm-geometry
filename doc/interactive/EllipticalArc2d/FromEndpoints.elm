module EllipticalArc2d.FromEndpoints exposing (..)

import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.EllipticalArc2d as EllipticalArc2d exposing (EllipticalArc2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Svg.Interaction as Interaction
import OpenSolid.Svg.Interaction.ScrollAmount as ScrollAmount exposing (ScrollAmount)
import OpenSolid.Vector2d as Vector2d
import Svg exposing (Svg)
import Svg.Attributes


boundingBox : BoundingBox2d
boundingBox =
    BoundingBox2d.with
        { minX = 0
        , minY = 0
        , maxX = 500
        , maxY = 400
        }


type Target
    = CenterPoint
    | StartPoint
    | EndPoint
    | XDirection
    | Section EllipticalArc2d.SweptAngle
    | XRadius
    | YRadius
    | Elsewhere


type alias Model =
    { startPoint : Point2d
    , endPoint : Point2d
    , xDirection : Direction2d
    , xRadius : Float
    , yRadius : Float
    , sweptAngle : EllipticalArc2d.SweptAngle
    , interactionModel : Interaction.Model Target
    , dragRotationCenter : Maybe Point2d
    , selected : Maybe Target
    }


type Msg
    = InteractionMsg (Interaction.Msg Target)


init : ( Model, Cmd Msg )
init =
    ( { startPoint = Point2d.fromCoordinates ( 400, 150 )
      , endPoint = Point2d.fromCoordinates ( 250, 300 )
      , xDirection = Direction2d.x
      , xRadius = 200
      , yRadius = 100
      , sweptAngle = EllipticalArc2d.smallPositive
      , interactionModel = Interaction.model
      , dragRotationCenter = Nothing
      , selected = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update (InteractionMsg interactionMsg) model =
    let
        ( updatedInteractionModel, notification ) =
            Interaction.update interactionMsg model.interactionModel

        updatedModel =
            case notification of
                Just (Interaction.Drag target modifiers { previousPoint, currentPoint }) ->
                    let
                        displacement =
                            Vector2d.from previousPoint currentPoint
                    in
                    case target of
                        StartPoint ->
                            { model
                                | startPoint =
                                    Point2d.translateBy displacement
                                        model.startPoint
                            }

                        EndPoint ->
                            { model
                                | endPoint =
                                    Point2d.translateBy displacement
                                        model.endPoint
                            }

                        CenterPoint ->
                            { model
                                | startPoint =
                                    Point2d.translateBy displacement
                                        model.startPoint
                                , endPoint =
                                    Point2d.translateBy displacement
                                        model.endPoint
                            }

                        XDirection ->
                            let
                                currentCenter =
                                    case model.dragRotationCenter of
                                        Just existingCenter ->
                                            Just existingCenter

                                        Nothing ->
                                            constructArc model
                                                |> Maybe.map
                                                    EllipticalArc2d.centerPoint

                                rotatedDirection =
                                    case currentCenter of
                                        Just centerPoint ->
                                            let
                                                rotationAngle =
                                                    Interaction.rotationAround
                                                        centerPoint
                                                        previousPoint
                                                        currentPoint
                                            in
                                            Direction2d.rotateBy rotationAngle
                                                model.xDirection

                                        Nothing ->
                                            model.xDirection
                            in
                            { model
                                | dragRotationCenter =
                                    currentCenter
                                , xDirection = rotatedDirection
                            }

                        _ ->
                            model

                Just (Interaction.Release XDirection _ _) ->
                    { model | dragRotationCenter = Nothing }

                Just (Interaction.Click target modifiers) ->
                    case target of
                        Section sweptAngle ->
                            { model
                                | sweptAngle = sweptAngle
                                , selected = Nothing
                            }

                        XRadius ->
                            { model | selected = Just XRadius }

                        YRadius ->
                            { model | selected = Just YRadius }

                        _ ->
                            { model | selected = Nothing }

                Just (Interaction.Tap [ target ]) ->
                    case target of
                        Section sweptAngle ->
                            { model | sweptAngle = sweptAngle }

                        _ ->
                            model

                Just (Interaction.Scroll target modifiers scrollAmount) ->
                    let
                        scale =
                            if ScrollAmount.isPositive scrollAmount then
                                1 / 1.1
                            else
                                1.1
                    in
                    case model.selected of
                        Just XRadius ->
                            { model | xRadius = scale * model.xRadius }

                        Just YRadius ->
                            { model | yRadius = scale * model.yRadius }

                        _ ->
                            model

                _ ->
                    model
    in
    ( { updatedModel | interactionModel = updatedInteractionModel }, Cmd.none )


noFill : Svg.Attribute msg
noFill =
    Svg.Attributes.fill "none"


whiteFill : Svg.Attribute msg
whiteFill =
    Svg.Attributes.fill "white"


blackStroke : Svg.Attribute msg
blackStroke =
    Svg.Attributes.stroke "black"


lightStroke : Svg.Attribute msg
lightStroke =
    Svg.Attributes.stroke "rgba(0, 0, 0, 0.25)"


blueStroke : Svg.Attribute msg
blueStroke =
    Svg.Attributes.stroke "blue"


lightGreyStroke : Svg.Attribute msg
lightGreyStroke =
    Svg.Attributes.stroke "lightgrey"


transparentStroke : Svg.Attribute msg
transparentStroke =
    Svg.Attributes.stroke "transparent"


thickStroke : Svg.Attribute msg
thickStroke =
    Svg.Attributes.strokeWidth "10"


roundCap : Svg.Attribute msg
roundCap =
    Svg.Attributes.strokeLinecap "round"


dashed : Svg.Attribute msg
dashed =
    Svg.Attributes.strokeDasharray "5 5"


drawPoint : Point2d -> Svg msg
drawPoint =
    Svg.point2d { radius = 3, attributes = [ whiteFill, blackStroke ] }


drawDirection : Point2d -> Direction2d -> Svg msg
drawDirection =
    Svg.direction2d
        { length = 50
        , tipLength = 10
        , tipWidth = 10
        , tipAttributes = [ whiteFill ]
        , stemAttributes = []
        , groupAttributes = [ blackStroke ]
        }


pointHandle : Target -> Point2d -> Svg Msg
pointHandle target point =
    Interaction.pointHandle point
        { target = target
        , radius = 5
        , renderBounds = boundingBox
        }
        |> Svg.map InteractionMsg


sectionHandle : EllipticalArc2d.SweptAngle -> EllipticalArc2d -> Svg Msg
sectionHandle sweptAngle arc =
    let
        shape =
            Svg.ellipticalArc2d
                [ noFill, transparentStroke, thickStroke, roundCap ]
                arc
    in
    Interaction.customHandle shape
        { target = Section sweptAngle
        , renderBounds = boundingBox
        }
        |> Svg.map InteractionMsg


lineSegmentHandle : Target -> LineSegment2d -> Svg Msg
lineSegmentHandle target lineSegment =
    Interaction.lineSegmentHandle lineSegment
        { target = target
        , padding = 5
        , renderBounds = boundingBox
        }
        |> Svg.map InteractionMsg


constructArc : Model -> Maybe EllipticalArc2d
constructArc model =
    EllipticalArc2d.fromEndpoints
        { startPoint = model.startPoint
        , endPoint = model.endPoint
        , xDirection = model.xDirection
        , xRadius = model.xRadius
        , yRadius = model.yRadius
        , sweptAngle = model.sweptAngle
        }


isActive : Target -> Model -> Bool
isActive target model =
    Interaction.isHovering target model.interactionModel
        || (model.selected == Just target)


view : Model -> Html Msg
view model =
    let
        endpointProperties =
            { startPoint = model.startPoint
            , endPoint = model.endPoint
            , xDirection = model.xDirection
            , xRadius = model.xRadius
            , yRadius = model.yRadius
            , sweptAngle = model.sweptAngle
            }

        computedArc =
            EllipticalArc2d.fromEndpoints endpointProperties

        sweptAngleTypes =
            [ EllipticalArc2d.smallPositive
            , EllipticalArc2d.smallNegative
            , EllipticalArc2d.largePositive
            , EllipticalArc2d.largeNegative
            ]

        dashedEllipses =
            sweptAngleTypes
                |> List.map
                    (\sweptAngle ->
                        { endpointProperties | sweptAngle = sweptAngle }
                    )
                |> List.map EllipticalArc2d.fromEndpoints
                |> List.map2 (,) sweptAngleTypes
                |> List.filterMap
                    (\( sweptAngle, computedArc ) ->
                        computedArc |> Maybe.map (\arc -> ( arc, sweptAngle ))
                    )
                |> List.map
                    (\( arc, sweptAngle ) ->
                        Svg.g []
                            [ Svg.ellipticalArc2d
                                [ noFill, dashed, lightGreyStroke ]
                                arc
                            , sectionHandle sweptAngle arc
                            ]
                    )
                |> Svg.g []

        ellipseSvg =
            case computedArc of
                Just arc ->
                    let
                        centerPoint =
                            EllipticalArc2d.centerPoint arc

                        xPoint =
                            centerPoint
                                |> Point2d.translateBy
                                    (Vector2d.with
                                        { length = model.xRadius
                                        , direction = model.xDirection
                                        }
                                    )

                        yPoint =
                            centerPoint
                                |> Point2d.translateBy
                                    (Vector2d.with
                                        { length = model.yRadius
                                        , direction =
                                            Direction2d.perpendicularTo
                                                model.xDirection
                                        }
                                    )

                        xRadialLine =
                            LineSegment2d.from centerPoint xPoint

                        yRadialLine =
                            LineSegment2d.from centerPoint yPoint

                        xStroke =
                            if isActive XRadius model then
                                blueStroke
                            else
                                lightStroke

                        yStroke =
                            if isActive YRadius model then
                                blueStroke
                            else
                                lightStroke
                    in
                    Svg.g []
                        [ dashedEllipses
                        , Svg.g [ noFill ]
                            [ Svg.lineSegment2d [ xStroke ] xRadialLine
                            , Svg.lineSegment2d [ yStroke ] yRadialLine
                            , lineSegmentHandle XRadius xRadialLine
                            , lineSegmentHandle YRadius yRadialLine
                            ]
                        , Svg.ellipticalArc2d [ noFill, blackStroke ] arc
                        , drawDirection centerPoint model.xDirection
                        , Interaction.directionTipHandle
                            centerPoint
                            model.xDirection
                            { length = 50
                            , tipLength = 10
                            , tipWidth = 10
                            , target = XDirection
                            , padding = 5
                            , renderBounds = boundingBox
                            }
                            |> Svg.map InteractionMsg
                        , drawPoint centerPoint
                        , pointHandle CenterPoint centerPoint
                        , directionHandle
                        ]

                Nothing ->
                    Svg.text ""

        directionHandle =
            case model.dragRotationCenter of
                Just centerPoint ->
                    Svg.g [ lightGreyStroke, whiteFill ]
                        [ Svg.direction2d
                            { length = 50
                            , tipLength = 10
                            , tipWidth = 10
                            , groupAttributes = []
                            , tipAttributes = []
                            , stemAttributes = []
                            }
                            centerPoint
                            model.xDirection
                        ]

                Nothing ->
                    Svg.text ""
    in
    Html.div []
        [ Svg.render2d boundingBox <|
            Interaction.container InteractionMsg
                { target = Elsewhere
                , renderBounds = boundingBox
                }
                [ ellipseSvg
                , directionHandle
                , drawPoint model.startPoint
                , pointHandle StartPoint model.startPoint
                , drawPoint model.endPoint
                , pointHandle EndPoint model.endPoint
                , Svg.boundingBox2d
                    [ Svg.Attributes.stroke "rgb(192, 192, 192)"
                    , Svg.Attributes.rx "3"
                    , Svg.Attributes.ry "3"
                    , noFill
                    ]
                    (BoundingBox2d.with
                        { minX = BoundingBox2d.minX boundingBox + 0.5
                        , minY = BoundingBox2d.minY boundingBox + 0.5
                        , maxX = BoundingBox2d.maxX boundingBox - 0.5
                        , maxY = BoundingBox2d.maxY boundingBox - 0.5
                        }
                    )
                ]
        , Html.div []
            [ Html.h2 [] [ Html.text "Instructions" ]
            , Html.ul []
                [ Html.li [] [ Html.text "Drag start and end point" ]
                , Html.li [] [ Html.text "Drag tip of X direction" ]
                , Html.li [] [ Html.text "Click dashed segments to switch to them" ]
                , Html.li [] [ Html.text "Drag center point to move the whole ellipse" ]
                , Html.li [] [ Html.text "Click either radial line to select it and then scroll to change that radius" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Interaction.subscriptions model.interactionModel |> Sub.map InteractionMsg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
