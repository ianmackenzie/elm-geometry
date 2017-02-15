module OpenSolid.Arc2d
    exposing
        ( Length
        , WindingDirection
        , short
        , long
        , clockwise
        , counterclockwise
        , throughPoints
        , fromEndpoints
        , centerPoint
        , radius
        , startPoint
        , endPoint
        , sweptAngle
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        , placeOnto
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.SketchPlane3d as SketchPlane3d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Circle2d as Circle2d


type Length
    = Short
    | Long


type WindingDirection
    = Clockwise
    | Counterclockwise


clockwise : WindingDirection
clockwise =
    Clockwise


counterclockwise : WindingDirection
counterclockwise =
    Counterclockwise


short : Length
short =
    Short


long : Length
long =
    Long


throughPoints : Point2d -> Point2d -> Point2d -> Maybe Arc2d
throughPoints firstPoint secondPoint thirdPoint =
    Circle2d.throughPoints firstPoint secondPoint thirdPoint
        |> Maybe.andThen
            (\circle ->
                let
                    centerPoint =
                        Circle2d.centerPoint circle

                    firstVector =
                        Point2d.vectorFrom centerPoint firstPoint

                    secondVector =
                        Point2d.vectorFrom centerPoint secondPoint

                    thirdVector =
                        Point2d.vectorFrom centerPoint thirdPoint
                in
                    Maybe.map3
                        (\firstDirection secondDirection thirdDirection ->
                            let
                                partial =
                                    Direction2d.angleFrom firstDirection
                                        secondDirection

                                full =
                                    Direction2d.angleFrom firstDirection
                                        thirdDirection

                                sweptAngle =
                                    if partial >= 0 && full >= partial then
                                        full
                                    else if partial <= 0 && full <= partial then
                                        full
                                    else if full >= 0 then
                                        full - 2 * pi
                                    else
                                        full + 2 * pi
                            in
                                Arc2d
                                    { centerPoint = centerPoint
                                    , startPoint = firstPoint
                                    , sweptAngle = sweptAngle
                                    }
                        )
                        (Vector2d.direction firstVector)
                        (Vector2d.direction secondVector)
                        (Vector2d.direction thirdVector)
            )


fromEndpoints : Point2d -> Point2d -> Float -> Length -> WindingDirection -> Maybe Arc2d
fromEndpoints startPoint endPoint radius lengthType windingDirection =
    let
        chord =
            LineSegment2d ( startPoint, endPoint )

        squaredRadius =
            radius * radius

        squaredHalfLength =
            LineSegment2d.squaredLength chord / 4
    in
        if squaredRadius >= squaredHalfLength then
            LineSegment2d.normalDirection chord
                |> Maybe.map
                    (\offsetDirection ->
                        let
                            offsetMagnitude =
                                sqrt (squaredRadius - squaredHalfLength)

                            offsetDistance =
                                case ( windingDirection, lengthType ) of
                                    ( Counterclockwise, Short ) ->
                                        offsetMagnitude

                                    ( Clockwise, Long ) ->
                                        offsetMagnitude

                                    ( Clockwise, Short ) ->
                                        -offsetMagnitude

                                    ( Counterclockwise, Long ) ->
                                        -offsetMagnitude

                            offset =
                                Direction2d.scaleBy offsetDistance
                                    offsetDirection

                            midpoint =
                                LineSegment2d.midpoint chord

                            centerPoint =
                                Point2d.translateBy offset midpoint

                            halfLength =
                                sqrt squaredHalfLength

                            shortAngle =
                                2 * asin (halfLength / radius)

                            sweptAngle =
                                case ( windingDirection, lengthType ) of
                                    ( Counterclockwise, Short ) ->
                                        shortAngle

                                    ( Clockwise, Short ) ->
                                        -shortAngle

                                    ( Counterclockwise, Long ) ->
                                        2 * pi - shortAngle

                                    ( Clockwise, Long ) ->
                                        shortAngle - 2 * pi
                        in
                            Arc2d
                                { centerPoint = centerPoint
                                , startPoint = startPoint
                                , sweptAngle = sweptAngle
                                }
                    )
        else
            Nothing


centerPoint : Arc2d -> Point2d
centerPoint (Arc2d properties) =
    properties.centerPoint


radius : Arc2d -> Float
radius arc =
    Point2d.distanceFrom (centerPoint arc) (startPoint arc)


startPoint : Arc2d -> Point2d
startPoint (Arc2d properties) =
    properties.startPoint


endPoint : Arc2d -> Point2d
endPoint arc =
    Point2d.rotateAround (centerPoint arc) (sweptAngle arc) (startPoint arc)


sweptAngle : Arc2d -> Float
sweptAngle (Arc2d properties) =
    properties.sweptAngle


scaleAbout : Point2d -> Float -> Arc2d -> Arc2d
scaleAbout point scale arc =
    let
        scalePoint =
            Point2d.scaleAbout point scale
    in
        Arc2d
            { centerPoint = scalePoint (centerPoint arc)
            , startPoint = scalePoint (startPoint arc)
            , sweptAngle =
                if scale > 0 then
                    sweptAngle arc
                else
                    -(sweptAngle arc)
            }


rotateAround : Point2d -> Float -> Arc2d -> Arc2d
rotateAround point angle =
    let
        rotatePoint =
            Point2d.rotateAround point angle
    in
        \arc ->
            Arc2d
                { centerPoint = rotatePoint (centerPoint arc)
                , startPoint = rotatePoint (startPoint arc)
                , sweptAngle = sweptAngle arc
                }


translateBy : Vector2d -> Arc2d -> Arc2d
translateBy displacement arc =
    let
        translatePoint =
            Point2d.translateBy displacement
    in
        Arc2d
            { centerPoint = translatePoint (centerPoint arc)
            , startPoint = translatePoint (startPoint arc)
            , sweptAngle = sweptAngle arc
            }


mirrorAcross : Axis2d -> Arc2d -> Arc2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis
    in
        \arc ->
            Arc2d
                { centerPoint = mirrorPoint (centerPoint arc)
                , startPoint = mirrorPoint (startPoint arc)
                , sweptAngle = -(sweptAngle arc)
                }


relativeTo : Frame2d -> Arc2d -> Arc2d
relativeTo frame arc =
    let
        relativePoint =
            Point2d.relativeTo frame
    in
        Arc2d
            { centerPoint = relativePoint (centerPoint arc)
            , startPoint = relativePoint (startPoint arc)
            , sweptAngle =
                if Frame2d.isRightHanded frame then
                    (sweptAngle arc)
                else
                    -(sweptAngle arc)
            }


placeIn : Frame2d -> Arc2d -> Arc2d
placeIn frame arc =
    let
        placePoint =
            Point2d.placeIn frame
    in
        Arc2d
            { centerPoint = placePoint (centerPoint arc)
            , startPoint = placePoint (startPoint arc)
            , sweptAngle =
                if Frame2d.isRightHanded frame then
                    (sweptAngle arc)
                else
                    -(sweptAngle arc)
            }


placeOnto : SketchPlane3d -> Arc2d -> Arc3d
placeOnto sketchPlane arc =
    let
        place =
            Point2d.placeOnto sketchPlane

        axis =
            Axis3d
                { originPoint = place (centerPoint arc)
                , direction = SketchPlane3d.normalDirection sketchPlane
                }
    in
        Arc3d
            { axis = axis
            , startPoint = place (startPoint arc)
            , sweptAngle = sweptAngle arc
            }
