module OpenSolid.EllipticalArc2d
    exposing
        ( EllipticalArc2d
        , SweptAngle
        , axes
        , centerPoint
        , endPoint
        , fromEndpoints
        , largeNegative
        , largePositive
        , pointOn
        , smallNegative
        , smallPositive
        , startAngle
        , startPoint
        , sweptAngle
        , with
        , xAxis
        , xDirection
        , xRadius
        , yAxis
        , yDirection
        , yRadius
        )

import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


type alias EllipticalArc2d =
    Internal.EllipticalArc2d


with : { centerPoint : Point2d, xDirection : Direction2d, xRadius : Float, yRadius : Float, startAngle : Float, sweptAngle : Float } -> EllipticalArc2d
with { centerPoint, xDirection, xRadius, yRadius, startAngle, sweptAngle } =
    Internal.EllipticalArc2d
        { axes =
            Frame2d.with { originPoint = centerPoint, xDirection = xDirection }
        , xRadius = abs xRadius
        , yRadius = abs yRadius
        , startAngle = startAngle
        , sweptAngle = sweptAngle
        }


fromEndpoints : { startPoint : Point2d, endPoint : Point2d, xDirection : Direction2d, xRadius : Float, yRadius : Float, sweptAngle : SweptAngle } -> Maybe EllipticalArc2d
fromEndpoints { startPoint, endPoint, xDirection, xRadius, yRadius, sweptAngle } =
    if xRadius > 0 && yRadius > 0 then
        let
            temporaryFrame =
                Frame2d.with
                    { originPoint =
                        startPoint
                            |> Point2d.translateBy
                                (Vector2d.with
                                    { direction = xDirection
                                    , length = -xRadius
                                    }
                                )
                    , xDirection = xDirection
                    }

            ( x2Ellipse, y2Ellipse ) =
                endPoint
                    |> Point2d.relativeTo temporaryFrame
                    |> Point2d.coordinates

            x2 =
                x2Ellipse / xRadius

            y2 =
                y2Ellipse / yRadius

            cx2 =
                x2 - 1

            cy2 =
                y2

            d =
                sqrt (cx2 * cx2 + cy2 * cy2) / 2
        in
        if d > 0 && d < 1 then
            let
                midAngle =
                    atan2 -cy2 -cx2

                offsetAngle =
                    acos d

                ( startAngle, sweptAngleInRadians ) =
                    case sweptAngle of
                        SmallPositive ->
                            ( midAngle + offsetAngle
                            , pi - 2 * offsetAngle
                            )

                        SmallNegative ->
                            ( midAngle - offsetAngle
                            , -pi + 2 * offsetAngle
                            )

                        LargePositive ->
                            ( midAngle - offsetAngle
                            , pi + 2 * offsetAngle
                            )

                        LargeNegative ->
                            ( midAngle + offsetAngle
                            , -pi - 2 * offsetAngle
                            )

                yDirection =
                    Direction2d.perpendicularTo xDirection

                centerPoint =
                    Point2d.placeIn temporaryFrame <|
                        Point2d.fromCoordinates
                            ( xRadius - xRadius * cos startAngle
                            , -yRadius * sin startAngle
                            )
            in
            Just <|
                with
                    { centerPoint = centerPoint
                    , xDirection = xDirection
                    , xRadius = xRadius
                    , yRadius = yRadius
                    , startAngle =
                        if startAngle > pi then
                            startAngle - 2 * pi
                        else if startAngle < -pi then
                            startAngle + 2 * pi
                        else
                            startAngle
                    , sweptAngle = sweptAngleInRadians
                    }
        else
            Nothing
    else
        Nothing


{-| Argument type used in [`fromEndpoints`](#fromEndpoints).
-}
type SweptAngle
    = SmallPositive
    | SmallNegative
    | LargePositive
    | LargeNegative


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
smallPositive : SweptAngle
smallPositive =
    SmallPositive


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
smallNegative : SweptAngle
smallNegative =
    SmallNegative


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
largePositive : SweptAngle
largePositive =
    LargePositive


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
largeNegative : SweptAngle
largeNegative =
    LargeNegative


centerPoint : EllipticalArc2d -> Point2d
centerPoint arc =
    Frame2d.originPoint (axes arc)


axes : EllipticalArc2d -> Frame2d
axes (Internal.EllipticalArc2d { axes }) =
    axes


xAxis : EllipticalArc2d -> Axis2d
xAxis arc =
    Frame2d.xAxis (axes arc)


yAxis : EllipticalArc2d -> Axis2d
yAxis arc =
    Frame2d.yAxis (axes arc)


xRadius : EllipticalArc2d -> Float
xRadius (Internal.EllipticalArc2d { xRadius }) =
    xRadius


yRadius : EllipticalArc2d -> Float
yRadius (Internal.EllipticalArc2d { yRadius }) =
    yRadius


startAngle : EllipticalArc2d -> Float
startAngle (Internal.EllipticalArc2d { startAngle }) =
    startAngle


sweptAngle : EllipticalArc2d -> Float
sweptAngle (Internal.EllipticalArc2d { sweptAngle }) =
    sweptAngle


pointOn : EllipticalArc2d -> Float -> Point2d
pointOn arc t =
    let
        theta =
            startAngle arc + t * sweptAngle arc
    in
    Point2d.placeIn (axes arc) <|
        Point2d.fromCoordinates
            ( xRadius arc * cos theta
            , yRadius arc * sin theta
            )


startPoint : EllipticalArc2d -> Point2d
startPoint arc =
    pointOn arc 0


endPoint : EllipticalArc2d -> Point2d
endPoint arc =
    pointOn arc 1


xDirection : EllipticalArc2d -> Direction2d
xDirection arc =
    Frame2d.xDirection (axes arc)


yDirection : EllipticalArc2d -> Direction2d
yDirection arc =
    Frame2d.yDirection (axes arc)
