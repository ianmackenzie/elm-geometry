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
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , relativeTo, placeIn
    , ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentDirectionAlong, sampleAlong
    , arcLengthParameterization, fromArcLengthParameterized
    , firstDerivative, maxSecondDerivativeMagnitude
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

All remaining properties of elliptical arcs are actually just properties of the
underlying ellipse; check out the <Ellipse2d> module for details.

@docs centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius


# Evaluation

@docs pointOn
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, sample


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `EllipticalArc2d`. If you need to do something fancy, you can extract
these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, maxSecondDerivativeMagnitude

-}

import Angle exposing (Angle, Radians)
import Arc.SweptAngle as SweptAngle exposing (SweptAngle)
import ArcLengthParameterization exposing (ArcLengthParameterization)
import Axis2d exposing (Axis2d)
import Direction2d exposing (Direction2d)
import Ellipse2d exposing (Ellipse2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Interval
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Squared)
import Quantity.Extra as Quantity
import Quantity.Interval
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
            , xRadius = 2
            , yRadius = 1
            , startAngle = 0
            , sweptAngle = degrees 90
            }

![90 degree elliptical arc](https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/with1.svg)

To make an inclined 180 degree elliptical arc, you might use

    EllipticalArc2d.with
        { centerPoint = Point2d.origin
        , xDirection = Direction2d.degrees 30
        , xRadius = 2
        , yRadius = 1
        , startAngle = degrees -90
        , sweptAngle = degrees 180
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

The example below is interactive; try dragging either endpoint or the tip of the
X direction (or the center point to move the whole arc), clicking on the X or Y
radial lines and then scrolling to changet that radius, or clicking/tapping on
the various dashed arcs to switch what kind of swept angle to use.

<iframe src="https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/fromEndpoints.html" style="width: 500px; height: 400px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/fromEndpoints.html`
</iframe>

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
    --> 0

-}
startAngle : EllipticalArc2d units coordinates -> Angle
startAngle (Types.EllipticalArc2d arc) =
    arc.startAngle


{-| The swept angle of an elliptical arc is the difference between values of the
[ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation)
from the start point to the end point of the arc.

    EllipticalArc2d.sweptAngle exampleArc
    --> degrees 90

-}
sweptAngle : EllipticalArc2d units coordinates -> Angle
sweptAngle (Types.EllipticalArc2d arc) =
    arc.sweptAngle


{-| Get the point along an elliptical arc at a given parameter value:

    EllipticalArc2d.pointOn exampleArc 0
    --> Point2d.meters 2 0

    EllipticalArc2d.pointOn exampleArc 0.5
    --> Point2d.meters 1.4142 0.7071

    EllipticalArc2d.pointOn exampleArc 1
    --> Point2d.meters 0 1

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


{-| Get the first derivative of an elliptical arc at a given parameter value:

    EllipticalArc2d.firstDerivative exampleArc 0
    --> Vector2d.meters 0 1.5708

    EllipticalArc2d.firstDerivative exampleArc 0.5
    --> Vector2d.meters -2.2214 1.1107

    EllipticalArc2d.firstDerivative exampleArc 1
    --> Vector2d.meters -3.1416 0

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


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents an arc that is definitely not degenerate. It
is used as input to functions such as `EllipticalArc2d.tangentDirection` and can
be constructed using `EllipticalArc2d.nondegenerate`.

-}
type Nondegenerate units coordinates
    = Curved (EllipticalArc2d units coordinates)
    | Horizontal (EllipticalArc2d units coordinates)
    | Vertical (EllipticalArc2d units coordinates)


{-| Attempt to construct a nondegenerate elliptical arc from a general
`EllipticalArc2d`. If the arc is in fact degenerate (consists of a single
point), returns an `Err` with that point.

    EllipticalArc2d.nondegenerate exampleArc
    --> Ok nondegenerateExampleArc

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

    EllipticalArc2d.fromNondegenerate
        nondegenerateExampleArc
    --> exampleArc

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
parameter value:

    EllipticalArc2d.tangentDirection
        nondegenerateExampleArc
        0
    --> Direction2d.degrees 90

    EllipticalArc2d.tangentDirection
        nondegenerateExampleArc
        0.5
    --> Direction2d.degrees 153.4

    EllipticalArc2d.tangentDirection
        nondegenerateExampleArc
        1
    --> Direction2d.degrees 180

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
at a given parameter value:

    EllipticalArc2d.sample nondegenerateExampleArc 0
    --> ( Point2d.meters 2 0
    --> , Direction2d.degrees 90
    --> )

    EllipticalArc2d.sample nondegenerateExampleArc 0.5
    --> ( Point2d.meters 1.4142 0.7071
    --> , Direction2d.degrees 153.4
    --> )

    EllipticalArc2d.sample nondegenerateExampleArc 1
    --> ( Point2d.meters 0 1
    --> , Direction2d.degrees 180
    --> )

-}
sample : Nondegenerate units coordinates -> Float -> ( Point2d units coordinates, Direction2d coordinates )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


{-| Get the start point of an elliptical arc.

    EllipticalArc2d.startPoint exampleArc
    --> Point2d.meters 2 0

-}
startPoint : EllipticalArc2d units coordinates -> Point2d units coordinates
startPoint arc =
    pointOn arc 0


{-| Get the end point of an elliptical arc.

    EllipticalArc2d.endPoint exampleArc
    --> Point2d.meters 0 1

-}
endPoint : EllipticalArc2d units coordinates -> Point2d units coordinates
endPoint arc =
    pointOn arc 1


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

    EllipticalArc2d.reverse exampleArc
    --> EllipticalArc2d.with
    -->     { centerPoint = Point2d.origin
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = degrees 90
    -->     , sweptAngle = degrees -90
    -->     }

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

    exampleArc
        |> EllipticalArc2d.scaleAbout Point2d.origin 3
    --> EllipticalArc2d.with
    -->     { centerPoint = Point2d.origin
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 6
    -->     , yRadius = 3
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
scaleAbout : Point2d units coordinates -> Float -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
scaleAbout point scale arc =
    transformBy (Ellipse2d.scaleAbout point scale) arc


{-| Rotate an elliptical arc around a given point by a given angle (in radians).

    exampleArc
        |> EllipticalArc2d.rotateAround Point2d.origin
            (Angle.degrees 180)
    --> EllipticalArc2d.with
    -->     { centerPoint = Point2d.origin
    -->     , xDirection = Direction2d.negativeX
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
rotateAround : Point2d units coordinates -> Angle -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
rotateAround point angle arc =
    transformBy (Ellipse2d.rotateAround point angle) arc


{-| Translate an elliptical arc by a given displacement.

    exampleArc
        |> EllipticalArc2d.translateBy
            (Vector2d.meters 2 3)
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.meters 2 3
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
translateBy : Vector2d units coordinates -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
translateBy displacement arc =
    transformBy (Ellipse2d.translateBy displacement) arc


{-| Translate an elliptical arc in a given direction by a given distance;

    EllipticalArc2d.translateIn direction distance

is equivalent to

    EllipticalArc2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d coordinates -> Quantity Float units -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
translateIn direction distance arc =
    translateBy (Vector2d.withLength distance direction) arc


{-| Mirror an elliptical arc across a given axis.

    mirroredArc =
        exampleArc
            |> EllipticalArc2d.mirrorAcross Axis2d.y

    EllipticalArc2d.startPoint mirroredArc
    --> Point2d.meters -2 0

    EllipticalArc2d.endPoint mirroredArc
    --> Point2d.meters 0 1

-}
mirrorAcross : Axis2d units coordinates -> EllipticalArc2d units coordinates -> EllipticalArc2d units coordinates
mirrorAcross axis arc =
    transformBy (Ellipse2d.mirrorAcross axis) arc


{-| Take an elliptical arc defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.meters 1 2)

    EllipticalArc2d.relativeTo localFrame exampleArc
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.meters -1 -2
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> EllipticalArc2d units globalCoordinates -> EllipticalArc2d units localCoordinates
relativeTo frame arc =
    transformBy (Ellipse2d.relativeTo frame) arc


{-| Take an elliptical arc considered to be defined in local coordinates
relative to a given reference frame, and return that arc expressed in global
coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.meters 1 2)

    EllipticalArc2d.relativeTo localFrame exampleArc
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.meters 1 2
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> EllipticalArc2d units localCoordinates -> EllipticalArc2d units globalCoordinates
placeIn frame arc =
    transformBy (Ellipse2d.placeIn frame) arc


{-| Find a conservative upper bound on the magnitude of the second derivative of
an elliptical arc. This can be useful when determining error bounds for various
kinds of linear approximations.

    exampleArc
        |> EllipticalArc2d.maxSecondDerivativeMagnitude
    --> 4.935

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
            Quantity.Interval.from theta0 theta1

        sinThetaInterval =
            Quantity.Interval.sin thetaInterval

        includeKx =
            Interval.contains 0 sinThetaInterval

        includeKy =
            (Interval.maxValue sinThetaInterval == 1)
                || (Interval.minValue sinThetaInterval == -1)
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
given accuracy. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the specified maximum
error.

    parameterizedArc =
        exampleArc
            |> EllipticalArc2d.arcLengthParameterized
                { maxError = 1.0e-4 }

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


{-| Find the total arc length of an elliptical arc. This will be accurate to
within the tolerance given when calling `arcLengthParameterized`.

    arcLength : Float
    arcLength =
        EllipticalArc2d.arcLength parameterizedArc

    arcLength
    --> 2.4221

-}
arcLength : ArcLengthParameterized units coordinates -> Quantity Float units
arcLength parameterizedArc =
    arcLengthParameterization parameterizedArc
        |> ArcLengthParameterization.totalArcLength


{-| Try to get the point along an elliptical arc at a given arc length. For
example, to get the true midpoint of `exampleArc`:

    EllipticalArc2d.pointAlong parameterizedArc
        (arcLength / 2)
    --> Just (Point2d.meters 1.1889 0.8041)

Note that this is not the same as evaulating at a parameter value of 0.5:

    EllipticalArc2d.pointOn exampleArc 0.5
    --> Point2d.meters 1.4142 0.7071

If the given arc length is less than zero or greater than the arc length of the
arc, returns `Nothing`.

-}
pointAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Point2d units coordinates
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> pointOn parameterized.underlyingArc


{-| Try to get the tangent direction along an elliptical arc at a given arc
length. To get the tangent direction at the midpoint of `exampleArc`:

    EllipticalArc2d.tangentDirectionAlong parameterizedArc
        (arcLength / 2)
    --> Just (Direction2d.degrees 159.7)

If the given arc length is less than zero or greater than the arc length of the
elliptical arc (or if the elliptical arc is degenerate), returns `Nothing`.

-}
tangentDirectionAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Direction2d coordinates
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> tangentDirection parameterized.nondegenerateArc


{-| Try to get the point and tangent direction along an elliptical arc at a
given arc length. To get the point and tangent direction at the midpoint of
`exampleArc`:

    EllipticalArc2d.sampleAlong parameterizedArc
        (arcLength / 2)
    --> Just
    -->     ( Point2d.meters 1.1889 0.8041
    -->     , Direction2d.degrees 159.7
    -->     )

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), returns `Nothing`.

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
