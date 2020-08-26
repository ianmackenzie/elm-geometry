--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module EllipticalArc2d exposing
    ( EllipticalArc2d
    , with, fromEndpoints
    , startAngle, sweptAngle, startPoint, endPoint
    , centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius
    , boundingBox, signedDistanceAlong
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , segments, approximate
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    , ArcLengthParameterized, arcLengthParameterized, arcLength
    , pointAlong, midpoint, tangentDirectionAlong, sampleAlong
    , arcLengthParameterization, fromArcLengthParameterized
    , firstDerivative, maxSecondDerivativeMagnitude, numApproximationSegments
    )

{-| An `EllipticalArc2d` is a section of an `Ellipse2d` with a start and end
point. This module includes functionality for

  - Constructing an elliptical arc from its center or end points
  - Scaling, rotating, translating and mirroring elliptical arcs
  - Evaluating points and derivative vectors along elliptical arcs
  - Forming arc length parameterizations of elliptical arcs

The `startAngle` and `sweptAngle` values referred to below are not actually
proper angles but instead refer to values of the [ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation).
However, in simple cases you don't need to worry about the difference - if
`startAngle` and `sweptAngle` are both multiples of 90 degrees, then you can
treat them as actual angles and everything will behave as you expect.

@docs EllipticalArc2d


# Constructors

@docs with, fromEndpoints


# Properties

@docs startAngle, sweptAngle, startPoint, endPoint

All remaining properties of elliptical arcs are actually just [properties of the
underlying ellipse](Ellipse2d#properties).

@docs centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius


# Bounds

@docs boundingBox, signedDistanceAlong


# Evaluation

@docs pointOn
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, sample


# Linear approximation

@docs segments, approximate


# Transformations

These transformations generally behave just like [the ones in the `Point2d`
module](Point2d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength

For the following evaluation functions, the given arc length will be clamped to
the arc length of the spline, so the result will always be on the spline.

@docs pointAlong, midpoint, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `EllipticalArc2d`. If you need to do something fancy, you can extract
these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Advanced

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, maxSecondDerivativeMagnitude, numApproximationSegments

-}

import Angle exposing (Angle, Radians)
import Angle.Interval
import ArcLengthParameterization exposing (ArcLengthParameterization)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Curve
import Direction2d exposing (Direction2d)
import Ellipse2d exposing (Ellipse2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Parameter1d
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity(..), Rate, Squared)
import Quantity.Extra as Quantity
import Quantity.Interval as Interval exposing (Interval)
import SweptAngle exposing (SweptAngle)
import Unsafe.Direction2d as Direction2d
import Vector2d exposing (Vector2d)


{-| -}
type alias EllipticalArc2d units coordinates =
    Types.EllipticalArc2d units coordinates


{-| Construct an elliptical arc from its center point, X direction, X and Y
radii, start angle and swept angle. If you pass a negative radius, the absolute
value will be used.

For example, to construct a simple 90 degree elliptical arc, you might use

    exampleArc =
        EllipticalArc2d.with
            { centerPoint = Point2d.origin
            , xDirection = Direction2d.x
            , xRadius = Length.meters 2
            , yRadius = Length.meters 1
            , startAngle = Angle.degrees 0
            , sweptAngle = Angle.degrees 90
            }

![90 degree elliptical arc](https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/with1.svg)

To make an inclined 180 degree elliptical arc, you might use

    EllipticalArc2d.with
        { centerPoint = Point2d.origin
        , xDirection = Direction2d.degrees 30
        , xRadius = Length.meters 2
        , yRadius = Length.meters 1
        , startAngle = Angle.degrees -90
        , sweptAngle = Angle.degrees 180
        }

![180 degree inclined elliptical arc](https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/with2.svg)

-}
with :
    { centerPoint : Point2d units coordinates
    , xDirection : Direction2d coordinates
    , xRadius : Quantity Float units
    , yRadius : Quantity Float units
    , startAngle : Angle
    , sweptAngle : Angle
    }
    -> EllipticalArc2d units coordinates
with properties =
    Types.EllipticalArc2d
        { ellipse =
            Ellipse2d.with
                { centerPoint = properties.centerPoint
                , xDirection = properties.xDirection
                , xRadius = properties.xRadius
                , yRadius = properties.yRadius
                }
        , startAngle = properties.startAngle
        , sweptAngle = properties.sweptAngle
        }


{-| Attempt to construct an elliptical arc from its endpoints, X direction, and
X and Y radii. For any given valid set of these inputs, there are four possible
solutions, so you also need to specify which of the four solutions you want -
whether the swept angle of the arc should be less than or greater than 180
degrees, and whether the swept angle should be positive (counterclockwise) or
negative (clockwise).

This function will return `Nothing` if no solution can found. Typically this
means that the two endpoints are too far apart, but could also mean that one of
the specified radii was negative or zero, or the two given points were
coincident.

The behavior of this function is very close to [the SVG spec](https://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes),
but when 'out of range' parameters are given this function will simply return
`Nothing` instead of attempting to degrade gracefully (for example, by
increasing X and Y radius slightly if the given endpoints are slightly too far
apart). Note that this means this function is dangerous to use for 180 degree
arcs, since then slight numerical roundoff can mean the difference between a
solution being found and not - for 180 degree arcs it is safer to use
`EllipticalArc2d.with` instead.

-}
fromEndpoints :
    { startPoint : Point2d units coordinates
    , endPoint : Point2d units coordinates
    , xRadius : Quantity Float units
    , yRadius : Quantity Float units
    , xDirection : Direction2d coordinates
    , sweptAngle : SweptAngle
    }
    -> Maybe (EllipticalArc2d units coordinates)
fromEndpoints arguments =
    if
        (arguments.xRadius |> Quantity.greaterThan Quantity.zero)
            && (arguments.yRadius |> Quantity.greaterThan Quantity.zero)
    then
        let
            temporaryFrame =
                Frame2d.withXDirection arguments.xDirection
                    (arguments.startPoint
                        |> Point2d.translateIn arguments.xDirection
                            (Quantity.negate arguments.xRadius)
                    )

            x2Ellipse =
                Point2d.xCoordinateIn temporaryFrame arguments.endPoint

            y2Ellipse =
                Point2d.yCoordinateIn temporaryFrame arguments.endPoint

            x2 =
                Quantity.ratio x2Ellipse arguments.xRadius

            y2 =
                Quantity.ratio y2Ellipse arguments.yRadius

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
                    Angle.radians (atan2 -cy2 -cx2)

                offsetAngle =
                    Angle.acos d

                twiceOffsetAngle =
                    Quantity.multiplyBy 2 offsetAngle

                ( computedStartAngle, computedSweptAngle ) =
                    case arguments.sweptAngle of
                        Types.SmallPositive ->
                            ( midAngle |> Quantity.plus offsetAngle
                            , Angle.radians pi
                                |> Quantity.minus
                                    twiceOffsetAngle
                            )

                        Types.SmallNegative ->
                            ( midAngle |> Quantity.minus offsetAngle
                            , Angle.radians -pi
                                |> Quantity.plus
                                    twiceOffsetAngle
                            )

                        Types.LargePositive ->
                            ( midAngle |> Quantity.minus offsetAngle
                            , Angle.radians pi
                                |> Quantity.plus
                                    twiceOffsetAngle
                            )

                        Types.LargeNegative ->
                            ( midAngle |> Quantity.plus offsetAngle
                            , Angle.radians -pi
                                |> Quantity.minus
                                    twiceOffsetAngle
                            )

                computedCenterPoint =
                    Point2d.xyIn temporaryFrame
                        (Quantity.multiplyBy (1 - Angle.cos computedStartAngle) arguments.xRadius)
                        (Quantity.multiplyBy -(Angle.sin computedStartAngle) arguments.yRadius)

                adjustedStartAngle =
                    if
                        computedStartAngle
                            |> Quantity.greaterThan
                                (Angle.radians pi)
                    then
                        computedStartAngle
                            |> Quantity.minus
                                (Angle.radians (2 * pi))

                    else if
                        computedStartAngle
                            |> Quantity.lessThan
                                (Angle.radians -pi)
                    then
                        computedStartAngle
                            |> Quantity.plus
                                (Angle.radians (2 * pi))

                    else
                        computedStartAngle
            in
            Just <|
                with
                    { centerPoint = computedCenterPoint
                    , xDirection = arguments.xDirection
                    , xRadius = arguments.xRadius
                    , yRadius = arguments.yRadius
                    , startAngle = adjustedStartAngle
                    , sweptAngle = computedSweptAngle
                    }

        else
            Nothing

    else
        Nothing


{-| Convert an elliptical arc from one units type to another, by providing a
conversion factor given as a rate of change of destination units with respect to
source units.
-}
at : Quantity Float (Rate units2 units1) -> EllipticalArc2d units1 coordinates -> EllipticalArc2d units2 coordinates
at rate (Types.EllipticalArc2d arc) =
    Types.EllipticalArc2d
        { ellipse = Ellipse2d.at rate arc.ellipse
        , startAngle = arc.startAngle
        , sweptAngle = arc.sweptAngle
        }


{-| Convert an elliptical arc from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> EllipticalArc2d units1 coordinates -> EllipticalArc2d units2 coordinates
at_ rate arc =
    at (Quantity.inverse rate) arc


{-| -}
centerPoint : EllipticalArc2d units coordinates -> Point2d units coordinates
centerPoint (Types.EllipticalArc2d arc) =
    Ellipse2d.centerPoint arc.ellipse


{-| -}
axes : EllipticalArc2d units coordinates -> Frame2d units coordinates defines
axes (Types.EllipticalArc2d arc) =
    Ellipse2d.axes arc.ellipse


{-| -}
xAxis : EllipticalArc2d units coordinates -> Axis2d units coordinates
xAxis (Types.EllipticalArc2d arc) =
    Ellipse2d.xAxis arc.ellipse


{-| -}
yAxis : EllipticalArc2d units coordinates -> Axis2d units coordinates
yAxis (Types.EllipticalArc2d arc) =
    Ellipse2d.yAxis arc.ellipse


{-| -}
xRadius : EllipticalArc2d units coordinates -> Quantity Float units
xRadius (Types.EllipticalArc2d arc) =
    Ellipse2d.xRadius arc.ellipse


{-| -}
yRadius : EllipticalArc2d units coordinates -> Quantity Float units
yRadius (Types.EllipticalArc2d arc) =
    Ellipse2d.yRadius arc.ellipse


{-| The start angle of an elliptical arc is the value of the [ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation)
at the start point of the arc.

    EllipticalArc2d.startAngle exampleArc
    --> Angle.degrees 0

-}
startAngle : EllipticalArc2d units coordinates -> Angle
startAngle (Types.EllipticalArc2d arc) =
    arc.startAngle


{-| The swept angle of an elliptical arc is the difference between values of the
[ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation)
from the start point to the end point of the arc.

    EllipticalArc2d.sweptAngle exampleArc
    --> Angle.degrees 90

-}
sweptAngle : EllipticalArc2d units coordinates -> Angle
sweptAngle (Types.EllipticalArc2d arc) =
    arc.sweptAngle


{-| Get the point along an elliptical arc at a given parameter value.
-}
pointOn : EllipticalArc2d units coordinates -> Float -> Point2d units coordinates
pointOn arc parameterValue =
    let
        theta =
            startAngle arc
                |> Quantity.plus
                    (Quantity.multiplyBy parameterValue (sweptAngle arc))

        localX =
            Quantity.rCosTheta (xRadius arc) theta

        localY =
            Quantity.rSinTheta (yRadius arc) theta
    in
    Point2d.xyIn (axes arc) localX localY


{-| Get the first derivative of an elliptical arc at a given parameter value.
-}
firstDerivative : EllipticalArc2d units coordinates -> Float -> Vector2d units coordinates
firstDerivative arc parameterValue =
    let
        deltaTheta =
            sweptAngle arc

        theta =
            startAngle arc
                |> Quantity.plus
                    (Quantity.multiplyBy parameterValue deltaTheta)
    in
    Vector2d.xyIn (axes arc)
        (Quantity.rTheta (xRadius arc) deltaTheta
            |> Quantity.multiplyBy -(Angle.sin theta)
        )
        (Quantity.rTheta (yRadius arc) deltaTheta
            |> Quantity.multiplyBy (Angle.cos theta)
        )


{-| Represents a nondegenerate spline (one that has finite, non-zero length).
-}
type Nondegenerate units coordinates
    = Curved (EllipticalArc2d units coordinates)
    | Horizontal (EllipticalArc2d units coordinates)
    | Vertical (EllipticalArc2d units coordinates)


{-| Attempt to construct a nondegenerate elliptical arc from a general
`EllipticalArc2d`. If the arc is in fact degenerate (consists of a single
point), returns an `Err` with that point.
-}
nondegenerate : EllipticalArc2d units coordinates -> Result (Point2d units coordinates) (Nondegenerate units coordinates)
nondegenerate arc =
    let
        rx =
            xRadius arc

        ry =
            yRadius arc
    in
    if sweptAngle arc == Quantity.zero then
        Err (startPoint arc)

    else if rx == Quantity.zero && ry == Quantity.zero then
        Err (startPoint arc)

    else if rx == Quantity.zero then
        Ok (Vertical arc)

    else if ry == Quantity.zero then
        Ok (Horizontal arc)

    else
        Ok (Curved arc)


{-| Convert a nondegenerate elliptical arc back to a general `EllipticalArc2d`.
-}
fromNondegenerate : Nondegenerate units coordinates -> EllipticalArc2d units coordinates
fromNondegenerate nondegenerateArc =
    case nondegenerateArc of
        Curved arc ->
            arc

        Horizontal arc ->
            arc

        Vertical arc ->
            arc


{-| Get the tangent direction to a nondegenerate elliptical arc at a given
parameter value.
-}
tangentDirection : Nondegenerate units coordinates -> Float -> Direction2d coordinates
tangentDirection nondegenerateArc parameterValue =
    let
        arc =
            fromNondegenerate nondegenerateArc

        angle =
            startAngle arc
                |> Quantity.plus
                    (Quantity.multiplyBy parameterValue (sweptAngle arc))
    in
    case nondegenerateArc of
        Curved curvedArc ->
            let
                sinAngle =
                    Angle.sin angle

                cosAngle =
                    Angle.cos angle

                vx =
                    Quantity.multiplyBy -sinAngle (xRadius curvedArc)

                vy =
                    Quantity.multiplyBy cosAngle (yRadius curvedArc)

                -- Since xRadius and yRadius are both non-zero and at least one
                -- of sinAngle or cosAngle must be non-zero, one of vx or vy
                -- will always be non-zero and therefore this norm will be
                -- non-zero
                norm =
                    Quantity.sqrt
                        (Quantity.squared vx
                            |> Quantity.plus
                                (Quantity.squared vy)
                        )

                dx =
                    Quantity.ratio vx norm

                dy =
                    Quantity.ratio vy norm
            in
            Direction2d.unsafeXyIn (axes arc) dx dy

        Vertical verticalArc ->
            if Angle.cos angle >= 0 then
                yDirection verticalArc

            else
                Direction2d.reverse (yDirection verticalArc)

        Horizontal horizontalArc ->
            if Angle.sin angle >= 0 then
                Direction2d.reverse (xDirection horizontalArc)

            else
                xDirection horizontalArc


{-| Get both the point and tangent direction of a nondegenerate elliptical arc
at a given parameter value.
-}
sample : Nondegenerate units coordinates -> Float -> ( Point2d units coordinates, Direction2d coordinates )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


{-| Approximate an elliptical arc by a given number of line segments. Note that
the number of points in the polyline will be one more than the number of
segments.
-}
segments : Int -> EllipticalArc2d units coordinates -> Polyline2d units coordinates
segments numSegments arc =
    Polyline2d.fromVertices (Parameter1d.steps numSegments (pointOn arc))


{-| Approximate an elliptical arc as a polyline, within a given tolerance. Every
point on the returned polyline will be within the given tolerance of the
elliptical arc.
-}
approximate : Quantity Float units -> EllipticalArc2d units coordinates -> Polyline2d units coordinates
approximate maxError arc =
    segments (numApproximationSegments maxError arc) arc


{-| Get the start point of an elliptical arc.
-}
startPoint : EllipticalArc2d units coordinates -> Point2d units coordinates
startPoint arc =
    pointOn arc 0


{-| Get the end point of an elliptical arc.
-}
endPoint : EllipticalArc2d units coordinates -> Point2d units coordinates
endPoint arc =
    pointOn arc 1


{-| Project an elliptical arc onto an axis, returning the range of projected
distances along that axis.
-}
signedDistanceAlong : Axis2d units coordinates -> EllipticalArc2d units coordinates -> Interval Float units
signedDistanceAlong axis arc =
    let
        (Quantity dTheta) =
            sweptAngle arc

        p1 =
            startPoint arc

        p2 =
            endPoint arc

        (Quantity d1) =
            Point2d.signedDistanceAlong axis p1

        (Quantity d2) =
            Point2d.signedDistanceAlong axis p2
    in
    if dTheta == 0 then
        Interval.from (Quantity d1) (Quantity d2)

    else
        let
            (Types.Direction2d u) =
                Axis2d.direction axis

            (Types.Direction2d i) =
                xDirection arc

            (Types.Direction2d j) =
                yDirection arc

            (Quantity rX) =
                xRadius arc

            (Quantity rY) =
                yRadius arc

            (Quantity thetaStart) =
                startAngle arc

            thetaEnd =
                thetaStart + dTheta

            thetaMin =
                min thetaStart thetaEnd

            thetaMax =
                max thetaStart thetaEnd

            iDotU =
                i.x * u.x + i.y * u.y

            jDotU =
                j.x * u.x + j.y * u.y

            theta0 =
                atan2 (rY * jDotU) (rX * iDotU)

            thetaA =
                theta0 + pi * toFloat (ceiling ((thetaMin - theta0) / pi))

            thetaB =
                thetaA + pi

            tA =
                (thetaA - thetaStart) / dTheta

            tB =
                (thetaB - thetaStart) / dTheta

            (Quantity dA) =
                if 0 < tA && tA < 1 then
                    Point2d.signedDistanceAlong axis (pointOn arc tA)

                else
                    Quantity d1

            (Quantity dB) =
                if 0 < tB && tB < 1 then
                    Point2d.signedDistanceAlong axis (pointOn arc tB)

                else
                    Quantity d1
        in
        Interval.from
            (Quantity (min (min d1 d2) (min dA dB)))
            (Quantity (max (max d1 d2) (max dA dB)))


{-| Get the bounding box of an elliptical arc.
-}
boundingBox : EllipticalArc2d units coordinates -> BoundingBox2d units coordinates
boundingBox arc =
    BoundingBox2d.xy
        (signedDistanceAlong Axis2d.x arc)
        (signedDistanceAlong Axis2d.y arc)


{-| -}
xDirection : EllipticalArc2d units coordinates -> Direction2d coordinates
xDirection arc =
    Frame2d.xDirection (axes arc)


{-| -}
yDirection : EllipticalArc2d units coordinates -> Direction2d coordinates
yDirection arc =
    Frame2d.yDirection (axes arc)


{-| Reverse the direction of an elliptical arc, so that the start point becomes
the end point and vice versa. Does not change the shape of the arc or any
properties of the underlying ellipse.
-}
reverse : EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
reverse (Types.EllipticalArc2d properties) =
    Types.EllipticalArc2d
        { properties
            | startAngle =
                properties.startAngle |> Quantity.plus properties.sweptAngle
            , sweptAngle = Quantity.negate properties.sweptAngle
        }


transformBy : (Ellipse2d units1 coordinates1 -> Ellipse2d units2 coordinates2) -> EllipticalArc2d units1 coordinates1 -> EllipticalArc2d units2 coordinates2
transformBy ellipseTransformation (Types.EllipticalArc2d properties) =
    Types.EllipticalArc2d
        { ellipse = ellipseTransformation properties.ellipse
        , startAngle = properties.startAngle
        , sweptAngle = properties.sweptAngle
        }


{-| Scale an elliptical arc about a given point by a given scale.
-}
scaleAbout : Point2d units coordinates -> Float -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
scaleAbout point scale arc =
    transformBy (Ellipse2d.scaleAbout point scale) arc


{-| Rotate an elliptical arc around a given point by a given angle.
-}
rotateAround : Point2d units coordinates -> Angle -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
rotateAround point angle arc =
    transformBy (Ellipse2d.rotateAround point angle) arc


{-| Translate an elliptical arc by a given displacement.
-}
translateBy : Vector2d units coordinates -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
translateBy displacement arc =
    transformBy (Ellipse2d.translateBy displacement) arc


{-| Translate an elliptical arc in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
translateIn direction distance arc =
    translateBy (Vector2d.withLength distance direction) arc


{-| Mirror an elliptical arc across a given axis.
-}
mirrorAcross : Axis2d units coordinates -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
mirrorAcross axis arc =
    transformBy (Ellipse2d.mirrorAcross axis) arc


{-| Take an elliptical arc defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> EllipticalArc2d units globalCoordinates -> EllipticalArc2d units localCoordinates
relativeTo frame arc =
    transformBy (Ellipse2d.relativeTo frame) arc


{-| Take an elliptical arc considered to be defined in local coordinates
relative to a given reference frame, and return that arc expressed in global
coordinates.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> EllipticalArc2d units localCoordinates -> EllipticalArc2d units globalCoordinates
placeIn frame arc =
    transformBy (Ellipse2d.placeIn frame) arc


{-| Find a conservative upper bound on the magnitude of the second derivative of
an elliptical arc. This can be useful when determining error bounds for various
kinds of linear approximations.
-}
maxSecondDerivativeMagnitude : EllipticalArc2d units coordinates -> Quantity Float units
maxSecondDerivativeMagnitude arc =
    let
        theta0 =
            startAngle arc

        dTheta =
            sweptAngle arc

        theta1 =
            theta0 |> Quantity.plus dTheta

        (Quantity dThetaSquared) =
            Quantity.squared dTheta

        rx =
            xRadius arc

        ry =
            yRadius arc

        kx =
            Quantity.multiplyBy dThetaSquared rx

        ky =
            Quantity.multiplyBy dThetaSquared ry

        thetaInterval =
            Interval.from theta0 theta1

        sinThetaInterval =
            Angle.Interval.sin thetaInterval

        includeKx =
            Interval.contains Quantity.zero sinThetaInterval

        includeKy =
            (Interval.maxValue sinThetaInterval == Quantity.float 1)
                || (Interval.minValue sinThetaInterval == Quantity.float -1)
    in
    if (kx |> Quantity.greaterThanOrEqualTo ky) && includeKx then
        -- kx is the global max and is included in the arc
        kx

    else if (ky |> Quantity.greaterThanOrEqualTo kx) && includeKy then
        -- ky is the global max and is included in the arc
        ky

    else
        -- global max is not included in the arc, so max must be at an endpoint
        let
            rxSquared =
                Quantity.squared rx

            rySquared =
                Quantity.squared ry

            cosTheta0 =
                Angle.cos theta0

            sinTheta0 =
                Angle.sin theta0

            cosTheta1 =
                Angle.cos theta1

            sinTheta1 =
                Angle.sin theta1

            d0 =
                (rxSquared |> Quantity.multiplyBy (cosTheta0 * cosTheta0))
                    |> Quantity.plus
                        (rySquared |> Quantity.multiplyBy (sinTheta0 * sinTheta0))

            d1 =
                (rxSquared |> Quantity.multiplyBy (cosTheta1 * cosTheta1))
                    |> Quantity.plus
                        (rySquared |> Quantity.multiplyBy (sinTheta1 * sinTheta1))
        in
        Quantity.sqrt (Quantity.max d0 d1) |> Quantity.multiplyBy dThetaSquared


derivativeMagnitude : EllipticalArc2d units coordinates -> Float -> Quantity Float units
derivativeMagnitude arc =
    let
        rx =
            xRadius arc

        ry =
            yRadius arc

        theta0 =
            startAngle arc

        dTheta =
            sweptAngle arc

        absDTheta =
            Quantity.abs dTheta
    in
    \parameterValue ->
        let
            theta =
                theta0
                    |> Quantity.plus
                        (dTheta |> Quantity.multiplyBy parameterValue)

            dx =
                Quantity.rSinTheta rx theta

            dy =
                Quantity.rCosTheta ry theta

            r =
                Quantity.sqrt
                    (Quantity.squared dx |> Quantity.plus (Quantity.squared dy))
        in
        Quantity.rTheta r absDTheta


{-| An elliptical arc that has been parameterized by arc length.
-}
type ArcLengthParameterized units coordinates
    = ArcLengthParameterized
        { underlyingArc : EllipticalArc2d units coordinates
        , parameterization : ArcLengthParameterization units
        , nondegenerateArc : Nondegenerate units coordinates
        }


{-| Build an arc length parameterization of the given elliptical arc, with a
given accuracy.
-}
arcLengthParameterized : { maxError : Quantity Float units } -> Nondegenerate units coordinates -> ArcLengthParameterized units coordinates
arcLengthParameterized { maxError } nondegenerateArc =
    let
        arc =
            fromNondegenerate nondegenerateArc

        parameterization =
            ArcLengthParameterization.build
                { maxError = maxError
                , derivativeMagnitude = derivativeMagnitude arc
                , maxSecondDerivativeMagnitude =
                    maxSecondDerivativeMagnitude arc
                }
    in
    ArcLengthParameterized
        { underlyingArc = arc
        , parameterization = parameterization
        , nondegenerateArc = nondegenerateArc
        }


{-| Find the total arc length of an elliptical arc, to within the tolerance
given when calling `arcLengthParameterized`.
-}
arcLength : ArcLengthParameterized units coordinates -> Quantity Float units
arcLength parameterizedArc =
    arcLengthParameterization parameterizedArc
        |> ArcLengthParameterization.totalArcLength


{-| Get the point along an elliptical arc at a given arc length.
-}
pointAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Point2d units coordinates
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> pointOn parameterized.underlyingArc


{-| Get the midpoint of an elliptical arc. Note that this is the point half way along the
elliptical arc by arc length, which is not in general the same as evaluating at a
parameter value of 0.5.
-}
midpoint : ArcLengthParameterized units coordinates -> Point2d units coordinates
midpoint parameterized =
    let
        halfArcLength =
            Quantity.multiplyBy 0.5 (arcLength parameterized)
    in
    pointAlong parameterized halfArcLength


{-| Get the tangent direction along an elliptical arc at a given arc length.
-}
tangentDirectionAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Direction2d coordinates
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> tangentDirection parameterized.nondegenerateArc


{-| Get the point and tangent direction along an elliptical arc at a given arc
length.
-}
sampleAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> ( Point2d units coordinates, Direction2d coordinates )
sampleAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> sample parameterized.nondegenerateArc


{-| -}
arcLengthParameterization : ArcLengthParameterized units coordinates -> ArcLengthParameterization units
arcLengthParameterization (ArcLengthParameterized parameterized) =
    parameterized.parameterization


{-| -}
fromArcLengthParameterized : ArcLengthParameterized units coordinates -> EllipticalArc2d units coordinates
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingArc


{-| Determine the number of linear segments needed to approximate an elliptical
arc to within a given tolerance.
-}
numApproximationSegments : Quantity Float units -> EllipticalArc2d units coordinats -> Int
numApproximationSegments maxError arc =
    Curve.numApproximationSegments
        { maxError = maxError
        , maxSecondDerivativeMagnitude = maxSecondDerivativeMagnitude arc
        }
