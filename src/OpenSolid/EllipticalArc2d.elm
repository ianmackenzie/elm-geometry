module OpenSolid.EllipticalArc2d
    exposing
        ( ArcLengthParameterized
        , EllipticalArc2d
        , SweptAngle
        , arcLength
        , arcLengthParameterized
        , arcLengthToParameterValue
        , axes
        , centerPoint
        , derivative
        , derivativeMagnitude
        , endPoint
        , fromEndpoints
        , largeNegative
        , largePositive
        , maxSecondDerivativeMagnitude
        , mirrorAcross
        , parameterValueToArcLength
        , placeIn
        , pointAlong
        , pointOn
        , relativeTo
        , reverse
        , rotateAround
        , scaleAbout
        , smallNegative
        , smallPositive
        , startAngle
        , startPoint
        , sweptAngle
        , tangentAlong
        , translateBy
        , with
        , xAxis
        , xDirection
        , xRadius
        , yAxis
        , yDirection
        , yRadius
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/ellipticalArc2d.svg" alt="EllipticalArc2d" width="160">

An `EllipticalArc2d` is a section of an `Ellipse2d` with a start and end point.
This module includes functionality for

  - Constructing an elliptical arc from its center or end points
  - Scaling, rotating, translating and mirroring elliptical arcs
  - Evaluating points and derivative vectors along elliptical arcs
  - Forming arc length parameterizations of elliptical arcs

The `startAngle` and `sweptAngle` values referred to below are not actually
proper angles but instead refer to values of the [ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation).
However, in simple cases you don't need to worry about the difference - if
`startAngle` and `sweptAngle` are both multiples of 90 degrees, then you can
treat them as actual angles and everything will work out.

@docs EllipticalArc2d


# Constructors

@docs with, fromEndpoints, SweptAngle, smallPositive, smallNegative, largePositive, largeNegative


# Properties

@docs startAngle, sweptAngle, startPoint, endPoint

All remaining properties of elliptical arcs are actually just properties of the
underlying ellipse; check out the [Ellipse2d](OpenSolid-Ellipse2d) module for
details.

@docs centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius


# Evaluation

@docs pointOn, derivative


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentAlong, arcLengthToParameterValue, parameterValueToArcLength


# Low level

Low level functionality that you are unlikely to need to use directly.

@docs derivativeMagnitude, maxSecondDerivativeMagnitude

-}

import OpenSolid.ArcLength as ArcLength
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Ellipse2d as Ellipse2d exposing (Ellipse2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Interval as Interval
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Scalar as Scalar
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


{-| An elliptical arc in 2D.
-}
type alias EllipticalArc2d =
    Internal.EllipticalArc2d


{-| Construct an elliptical arc from its center point, X direction, X and Y
radii, start angle and swept angle. Any negative sign on `xRadius` or `yRadius`
will be ignored (the absolute values will be used).

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

![90 degree elliptical arc](https://opensolid.github.io/geometry/2.1.0/doc/images/EllipticalArc2d/with1.svg)

To make an inclined 180 degree elliptical arc, you might use

    EllipticalArc2d.with
        { centerPoint = Point2d.origin
        , xDirection = Direction2d.fromAngle (degrees 30)
        , xRadius = 2
        , yRadius = 1
        , startAngle = degrees -90
        , sweptAngle = degrees 180
        }

![180 degree inclined elliptical arc](https://opensolid.github.io/geometry/2.1.0/doc/images/EllipticalArc2d/with2.svg)

-}
with : { centerPoint : Point2d, xDirection : Direction2d, xRadius : Float, yRadius : Float, startAngle : Float, sweptAngle : Float } -> EllipticalArc2d
with { centerPoint, xDirection, xRadius, yRadius, startAngle, sweptAngle } =
    Internal.EllipticalArc2d
        { ellipse =
            Ellipse2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = xRadius
                , yRadius = yRadius
                }
        , startAngle = startAngle
        , sweptAngle = sweptAngle
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

<iframe src="https://opensolid.github.io/geometry/2.1.0/doc/interactive/EllipticalArc2d/fromEndpoints.html" style="width: 500px; height: 400px" scrolling=no frameborder=0>
`https://opensolid.github.io/geometry/2.1.0/doc/interactive/EllipticalArc2d/fromEndpoints.html`
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


{-| -}
centerPoint : EllipticalArc2d -> Point2d
centerPoint (Internal.EllipticalArc2d { ellipse }) =
    Ellipse2d.centerPoint ellipse


{-| -}
axes : EllipticalArc2d -> Frame2d
axes (Internal.EllipticalArc2d { ellipse }) =
    Ellipse2d.axes ellipse


{-| -}
xAxis : EllipticalArc2d -> Axis2d
xAxis (Internal.EllipticalArc2d { ellipse }) =
    Ellipse2d.xAxis ellipse


{-| -}
yAxis : EllipticalArc2d -> Axis2d
yAxis (Internal.EllipticalArc2d { ellipse }) =
    Ellipse2d.yAxis ellipse


{-| -}
xRadius : EllipticalArc2d -> Float
xRadius (Internal.EllipticalArc2d { ellipse }) =
    Ellipse2d.xRadius ellipse


{-| -}
yRadius : EllipticalArc2d -> Float
yRadius (Internal.EllipticalArc2d { ellipse }) =
    Ellipse2d.yRadius ellipse


{-| The start angle of an elliptical arc is the value of the [ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation)
at the start point of the arc.

    EllipticalArc2d.startAngle exampleArc
    --> 0

-}
startAngle : EllipticalArc2d -> Float
startAngle (Internal.EllipticalArc2d { startAngle }) =
    startAngle


{-| The swept angle of an elliptical arc is the difference between values of the
[ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation)
at the start and end points of the arc.

    EllipticalArc2d.sweptAngle exampleArc
    --> degrees 90

-}
sweptAngle : EllipticalArc2d -> Float
sweptAngle (Internal.EllipticalArc2d { sweptAngle }) =
    sweptAngle


{-| Get a point along an elliptical arc, based on a parameter that ranges from 0
to 1. A parameter value of 0 corresponds to the start point of the arc and a
value of 1 corresponds to the end point.

    EllipticalArc2d.pointOn exampleArc 0
    --> Point2d.fromCoordinates ( 2, 0 )

    EllipticalArc2d.pointOn exampleArc 0.5
    --> Point2d.fromCoordinates ( 1.4142, 0.7071 )

    EllipticalArc2d.pointOn exampleArc 1
    --> Point2d.fromCoordinates ( 0, 1 )

-}
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


{-| Get the deriative value at a point along an elliptical arc, based on a
parameter that ranges from 0 to 1. A parameter value of 0 corresponds to the
derivative at the start point of the arc and a value of 1 corresponds to the
derivative at the end point.

    EllipticalArc2d.derivative exampleArc 0
    --> Vector2d.fromComponents ( 0, 1.5708 )

    EllipticalArc2d.derivative exampleArc 0.5
    --> Vector2d.fromComponents ( -2.2214, 1.1107 )

    EllipticalArc2d.derivative exampleArc 1
    --> Vector2d.fromComponents ( -3.1416, 0 )

-}
derivative : EllipticalArc2d -> Float -> Vector2d
derivative arc t =
    let
        deltaTheta =
            sweptAngle arc

        theta =
            startAngle arc + t * deltaTheta
    in
    Vector2d.placeIn (axes arc) <|
        Vector2d.fromComponents
            ( -(xRadius arc) * deltaTheta * sin theta
            , yRadius arc * deltaTheta * cos theta
            )


{-| Get the start point of an elliptical arc.

    EllipticalArc2d.startPoint exampleArc
    --> Point2d.fromCoordinates ( 2, 0 )

-}
startPoint : EllipticalArc2d -> Point2d
startPoint arc =
    pointOn arc 0


{-| Get the end point of an elliptical arc.

    EllipticalArc2d.endPoint exampleArc
    --> Point2d.fromCoordinates ( 0, 1 )

-}
endPoint : EllipticalArc2d -> Point2d
endPoint arc =
    pointOn arc 1


{-| -}
xDirection : EllipticalArc2d -> Direction2d
xDirection arc =
    Frame2d.xDirection (axes arc)


{-| -}
yDirection : EllipticalArc2d -> Direction2d
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
reverse : EllipticalArc2d -> EllipticalArc2d
reverse (Internal.EllipticalArc2d properties) =
    Internal.EllipticalArc2d
        { properties
            | startAngle = properties.startAngle + properties.sweptAngle
            , sweptAngle = -properties.sweptAngle
        }


transformBy : (Ellipse2d -> Ellipse2d) -> EllipticalArc2d -> EllipticalArc2d
transformBy ellipseTransformation (Internal.EllipticalArc2d properties) =
    Internal.EllipticalArc2d
        { properties | ellipse = ellipseTransformation properties.ellipse }


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
scaleAbout : Point2d -> Float -> EllipticalArc2d -> EllipticalArc2d
scaleAbout point scale =
    transformBy (Ellipse2d.scaleAbout point scale)


{-| Rotate an elliptical arc around a given point by a given angle (in radians).

    exampleArc
        |> EllipticalArc2d.rotateAround Point2d.origin
            (degrees 180)
    --> EllipticalArc2d.with
    -->     { centerPoint = Point2d.origin
    -->     , xDirection = Direction2d.negativeX
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
rotateAround : Point2d -> Float -> EllipticalArc2d -> EllipticalArc2d
rotateAround point angle =
    transformBy (Ellipse2d.rotateAround point angle)


{-| Translate an elliptical arc by a given displacement.

    exampleArc
        |> EllipticalArc2d.translateBy
            (Vector2d.fromComponents ( 2, 3 ))
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 2, 3 )
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
translateBy : Vector2d -> EllipticalArc2d -> EllipticalArc2d
translateBy displacement =
    transformBy (Ellipse2d.translateBy displacement)


{-| Mirror an elliptical arc across a given axis.

    mirroredArc =
        exampleArc
            |> EllipticalArc2d.mirrorAcross Axis2d.y

    EllipticalArc2d.startPoint mirroredArc
    --> Point2d.fromCoordinates ( -2, 0 )

    EllipticalArc2d.endPoint mirroredArc
    --> Point2d.fromCoordinates ( 0, 1 )

-}
mirrorAcross : Axis2d -> EllipticalArc2d -> EllipticalArc2d
mirrorAcross axis =
    transformBy (Ellipse2d.mirrorAcross axis)


{-| Take an elliptical arc defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    EllipticalArc2d.relativeTo localFrame exampleArc
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( -1, -2 )
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
relativeTo : Frame2d -> EllipticalArc2d -> EllipticalArc2d
relativeTo frame =
    transformBy (Ellipse2d.relativeTo frame)


{-| Take an elliptical arc considered to be defined in local coordinates
relative to a given reference frame, and return that arc expressed in global
coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    EllipticalArc2d.relativeTo localFrame exampleArc
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 1, 2 )
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
placeIn : Frame2d -> EllipticalArc2d -> EllipticalArc2d
placeIn frame =
    transformBy (Ellipse2d.placeIn frame)


{-| -}
maxSecondDerivativeMagnitude : EllipticalArc2d -> Float
maxSecondDerivativeMagnitude arc =
    let
        theta0 =
            startAngle arc

        dTheta =
            sweptAngle arc

        theta1 =
            theta0 + dTheta

        dThetaSquared =
            dTheta * dTheta

        rx =
            xRadius arc

        ry =
            yRadius arc

        kx =
            dThetaSquared * rx

        ky =
            dThetaSquared * ry

        thetaInterval =
            Scalar.hull theta0 theta1

        sinThetaInterval =
            Interval.sin thetaInterval

        includeKx =
            Interval.contains 0 sinThetaInterval

        includeKy =
            (Interval.maxValue sinThetaInterval == 1)
                || (Interval.minValue sinThetaInterval == -1)
    in
    if (kx >= ky) && includeKx then
        -- kx is the global max and is included in the arc
        kx
    else if (ky >= kx) && includeKy then
        -- ky is the global max and is included in the arc
        ky
    else
        -- global max is not included in the arc, so max must be at an endpoint
        let
            rxSquared =
                rx * rx

            rySquared =
                ry * ry

            cosTheta0 =
                cos theta0

            sinTheta0 =
                sin theta0

            cosTheta1 =
                cos theta1

            sinTheta1 =
                sin theta1

            d0 =
                (rxSquared * cosTheta0 * cosTheta0)
                    + (rySquared * sinTheta0 * sinTheta0)

            d1 =
                (rxSquared * cosTheta1 * cosTheta1)
                    + (rySquared * sinTheta1 * sinTheta1)
        in
        dThetaSquared * sqrt (max d0 d1)


{-| -}
derivativeMagnitude : EllipticalArc2d -> Float -> Float
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
            abs dTheta
    in
    \t ->
        let
            theta =
                theta0 + t * dTheta

            dx =
                rx * sin theta

            dy =
                ry * cos theta
        in
        absDTheta * sqrt (dx * dx + dy * dy)


{-| An elliptical arc that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized EllipticalArc2d ArcLength.Parameterization


{-| Build an arc length parameterization of the given elliptical arc, within
a given tolerance. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the given arc length
tolerance.

    tolerance =
        1.0e-4

    parameterizedArc =
        EllipticalArc2d.arcLengthParameterized
            tolerance
            exampleArc

-}
arcLengthParameterized : Float -> EllipticalArc2d -> ArcLengthParameterized
arcLengthParameterized tolerance arc =
    let
        parameterization =
            ArcLength.parameterization
                { tolerance = tolerance
                , derivativeMagnitude = derivativeMagnitude arc
                , maxSecondDerivativeMagnitude =
                    maxSecondDerivativeMagnitude arc
                }
    in
    ArcLengthParameterized arc parameterization


{-| Find the total arc length of an elliptical arc. This will be accurate to
within the tolerance given when calling `arcLengthParameterized`.

    arcLength : Float
    arcLength =
        EllipticalArc2d.arcLength parameterizedArc

    arcLength
    --> 2.4221

-}
arcLength : ArcLengthParameterized -> Float
arcLength (ArcLengthParameterized _ parameterization) =
    ArcLength.fromParameterization parameterization


{-| Try to get the point along an elliptical arc at a given arc length. For
example, to get the true midpoint of `exampleArc`:

    EllipticalArc2d.pointAlong parameterizedArc
        (arcLength / 2)
    --> Just (Point2d.fromCoordinates ( 1.1889, 0.8041 ))

Note that this is not the same as evaulating at a parameter value of 0.5:

    EllipticalArc2d.pointOn exampleArc 0.5
    --> Point2d.fromCoordinates ( 1.4142, 0.7071 )

If the given arc length is less than zero or greater than the arc length of the
arc, `Nothing` is returned.

-}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point2d
pointAlong (ArcLengthParameterized arc parameterization) s =
    ArcLength.toParameterValue parameterization s |> Maybe.map (pointOn arc)


{-| Try to get the tangent direction along an elliptical arc at a given arc
length. To get the tangent direction at the midpoint of `exampleArc`:

    EllipticalArc2d.tangentAlong parameterizedArc
        (arcLength / 2)
    --> Just (Direction2d.fromAngle (degrees 159.71))

If the given arc length is less than zero or greater than the arc length of the
arc (or if the derivative of the arc happens to be exactly zero at the given arc
length), `Nothing` is returned.

-}
tangentAlong : ArcLengthParameterized -> Float -> Maybe Direction2d
tangentAlong (ArcLengthParameterized arc parameterization) s =
    ArcLength.toParameterValue parameterization s
        |> Maybe.map (derivative arc)
        |> Maybe.andThen Vector2d.direction


{-| Try to get the parameter value along an elliptical arc at a given arc
length. If the given arc length is less than zero or greater than the arc length
of the arc, returns `Nothing`.

    EllipticalArc2d.arcLengthToParameterValue
        parameterizedArc
        (arcLength / 2)
    --> Just 0.5947

-}
arcLengthToParameterValue : ArcLengthParameterized -> Float -> Maybe Float
arcLengthToParameterValue (ArcLengthParameterized _ parameterization) s =
    ArcLength.toParameterValue parameterization s


{-| Get the arc length along an elliptical arc at a given parameter value. If
the given parameter value is less than zero or greater than one, returns
`Nothing`.

    EllipticalArc2d.parameterValueToArcLength
        parameterizedArc
        0.5
    --> Just 0.9657

-}
parameterValueToArcLength : ArcLengthParameterized -> Float -> Maybe Float
parameterValueToArcLength (ArcLengthParameterized _ parameterization) t =
    ArcLength.fromParameterValue parameterization t
