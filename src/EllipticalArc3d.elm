--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module EllipticalArc3d exposing
    ( EllipticalArc3d
    , on
    , startAngle, sweptAngle, startPoint, endPoint
    , centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius
    , boundingBox, signedDistanceAlong
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , segments, approximate
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectInto
    , at, at_
    , relativeTo, placeIn
    , ArcLengthParameterized, arcLengthParameterized, arcLength
    , pointAlong, midpoint, tangentDirectionAlong, sampleAlong
    , arcLengthParameterization, fromArcLengthParameterized
    , firstDerivative, maxSecondDerivativeMagnitude, numApproximationSegments
    )

{-| An `EllipticalArc3d` is a section of an `Ellipse3d` with a start and end
point, or equivalently a 3D version of an `EllipticalArc2d`. This module
includes functionality for

  - Constructing an elliptical arc from its center or end points
  - Scaling, rotating, translating and mirroring elliptical arcs
  - Evaluating points and derivative vectors along elliptical arcs
  - Forming arc length parameterizations of elliptical arcs

@docs EllipticalArc3d


# Constructors

@docs on


# Properties

@docs startAngle, sweptAngle, startPoint, endPoint

All remaining properties of elliptical arcs are actually just [properties of the
underlying ellipse](Ellipse3d#properties).

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

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectInto


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
underlying `EllipticalArc3d`. If you need to do something fancy, you can extract
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
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Curve
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import Ellipse3d exposing (Ellipse3d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Parameter1d
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity(..), Rate, Squared)
import Quantity.Extra as Quantity
import Quantity.Interval as Interval exposing (Interval)
import SketchPlane3d exposing (SketchPlane3d)
import SweptAngle exposing (SweptAngle)
import Unsafe.Direction3d as Direction3d
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias EllipticalArc3d units coordinates =
    Types.EllipticalArc3d units coordinates


{-| Construct a 3D elliptical arc by placing a 2D elliptical arc on a sketch
plane.
-}
on :
    SketchPlane3d units coordinates { defines : coordinates2d }
    -> EllipticalArc2d units coordinates2d
    -> EllipticalArc3d units coordinates
on sketchPlane (Types.EllipticalArc2d ellipticalArc2d) =
    Types.EllipticalArc3d
        { ellipse = Ellipse3d.on sketchPlane ellipticalArc2d.ellipse
        , startAngle = ellipticalArc2d.startAngle
        , sweptAngle = ellipticalArc2d.sweptAngle
        }


{-| Convert an elliptical arc from one units type to another, by providing a
conversion factor given as a rate of change of destination units with respect to
source units.
-}
at :
    Quantity Float (Rate units2 units1)
    -> EllipticalArc3d units1 coordinates
    -> EllipticalArc3d units2 coordinates
at rate (Types.EllipticalArc3d arc) =
    Types.EllipticalArc3d
        { ellipse = Ellipse3d.at rate arc.ellipse
        , startAngle = arc.startAngle
        , sweptAngle = arc.sweptAngle
        }


{-| Convert an elliptical arc from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ :
    Quantity Float (Rate units1 units2)
    -> EllipticalArc3d units1 coordinates
    -> EllipticalArc3d units2 coordinates
at_ rate arc =
    at (Quantity.inverse rate) arc


{-| -}
centerPoint : EllipticalArc3d units coordinates -> Point3d units coordinates
centerPoint (Types.EllipticalArc3d arc) =
    Ellipse3d.centerPoint arc.ellipse


{-| -}
axes : EllipticalArc3d units coordinates -> SketchPlane3d units coordinates defines
axes (Types.EllipticalArc3d arc) =
    Ellipse3d.axes arc.ellipse


{-| -}
xAxis : EllipticalArc3d units coordinates -> Axis3d units coordinates
xAxis (Types.EllipticalArc3d arc) =
    Ellipse3d.xAxis arc.ellipse


{-| -}
yAxis : EllipticalArc3d units coordinates -> Axis3d units coordinates
yAxis (Types.EllipticalArc3d arc) =
    Ellipse3d.yAxis arc.ellipse


{-| -}
xRadius : EllipticalArc3d units coordinates -> Quantity Float units
xRadius (Types.EllipticalArc3d arc) =
    Ellipse3d.xRadius arc.ellipse


{-| -}
yRadius : EllipticalArc3d units coordinates -> Quantity Float units
yRadius (Types.EllipticalArc3d arc) =
    Ellipse3d.yRadius arc.ellipse


{-| The start angle of an elliptical arc is the value of the [ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation)
at the start point of the arc.
-}
startAngle : EllipticalArc3d units coordinates -> Angle
startAngle (Types.EllipticalArc3d arc) =
    arc.startAngle


{-| The swept angle of an elliptical arc is the difference between values of the
[ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation)
from the start point to the end point of the arc.
-}
sweptAngle : EllipticalArc3d units coordinates -> Angle
sweptAngle (Types.EllipticalArc3d arc) =
    arc.sweptAngle


{-| Get the point along an elliptical arc at a given parameter value.
-}
pointOn : EllipticalArc3d units coordinates -> Float -> Point3d units coordinates
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
    Point3d.xyOn (axes arc) localX localY


{-| Get the first derivative of an elliptical arc at a given parameter value.
-}
firstDerivative : EllipticalArc3d units coordinates -> Float -> Vector3d units coordinates
firstDerivative arc parameterValue =
    let
        deltaTheta =
            sweptAngle arc

        theta =
            startAngle arc
                |> Quantity.plus
                    (Quantity.multiplyBy parameterValue deltaTheta)
    in
    Vector3d.xyOn (axes arc)
        (Quantity.rTheta (xRadius arc) deltaTheta
            |> Quantity.multiplyBy -(Angle.sin theta)
        )
        (Quantity.rTheta (yRadius arc) deltaTheta
            |> Quantity.multiplyBy (Angle.cos theta)
        )


{-| Represents a nondegenerate spline (one that has finite, non-zero length).
-}
type Nondegenerate units coordinates
    = Curved (EllipticalArc3d units coordinates)
    | Horizontal (EllipticalArc3d units coordinates)
    | Vertical (EllipticalArc3d units coordinates)


{-| Attempt to construct a nondegenerate elliptical arc from a general
`EllipticalArc3d`. If the arc is in fact degenerate (consists of a single
point), returns an `Err` with that point.
-}
nondegenerate : EllipticalArc3d units coordinates -> Result (Point3d units coordinates) (Nondegenerate units coordinates)
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


{-| Convert a nondegenerate elliptical arc back to a general `EllipticalArc3d`.
-}
fromNondegenerate : Nondegenerate units coordinates -> EllipticalArc3d units coordinates
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
tangentDirection : Nondegenerate units coordinates -> Float -> Direction3d coordinates
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
            Direction3d.unsafeXyOn (axes arc) dx dy

        Vertical verticalArc ->
            if Angle.cos angle >= 0 then
                yDirection verticalArc

            else
                Direction3d.reverse (yDirection verticalArc)

        Horizontal horizontalArc ->
            if Angle.sin angle >= 0 then
                Direction3d.reverse (xDirection horizontalArc)

            else
                xDirection horizontalArc


{-| Get both the point and tangent direction of a nondegenerate elliptical arc
at a given parameter value.
-}
sample : Nondegenerate units coordinates -> Float -> ( Point3d units coordinates, Direction3d coordinates )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


{-| Approximate an elliptical arc by a given number of line segments. Note that
the number of points in the polyline will be one more than the number of
segments.
-}
segments : Int -> EllipticalArc3d units coordinates -> Polyline3d units coordinates
segments numSegments arc =
    Polyline3d.fromVertices (Parameter1d.steps numSegments (pointOn arc))


{-| Approximate an elliptical arc as a polyline, within a given tolerance. Every
point on the returned polyline will be within the given tolerance of the
elliptical arc.
-}
approximate :
    Quantity Float units
    -> EllipticalArc3d units coordinates
    -> Polyline3d units coordinates
approximate maxError arc =
    segments (numApproximationSegments maxError arc) arc


{-| Get the start point of an elliptical arc.
-}
startPoint : EllipticalArc3d units coordinates -> Point3d units coordinates
startPoint arc =
    pointOn arc 0


{-| Get the end point of an elliptical arc.
-}
endPoint : EllipticalArc3d units coordinates -> Point3d units coordinates
endPoint arc =
    pointOn arc 1


{-| Project an elliptical arc onto an axis, returning the range of projected
distances along that axis.
-}
signedDistanceAlong : Axis3d units coordinates -> EllipticalArc3d units coordinates -> Interval Float units
signedDistanceAlong axis arc =
    let
        (Quantity dTheta) =
            sweptAngle arc

        p1 =
            startPoint arc

        p2 =
            endPoint arc

        (Quantity d1) =
            Point3d.signedDistanceAlong axis p1

        (Quantity d2) =
            Point3d.signedDistanceAlong axis p2
    in
    if dTheta == 0 then
        Interval.from (Quantity d1) (Quantity d2)

    else
        let
            (Types.Direction3d u) =
                Axis3d.direction axis

            (Types.Direction3d i) =
                xDirection arc

            (Types.Direction3d j) =
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
                i.x * u.x + i.y * u.y + i.z * u.z

            jDotU =
                j.x * u.x + j.y * u.y + j.z * u.z

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
                    Point3d.signedDistanceAlong axis (pointOn arc tA)

                else
                    Quantity d1

            (Quantity dB) =
                if 0 < tB && tB < 1 then
                    Point3d.signedDistanceAlong axis (pointOn arc tB)

                else
                    Quantity d1
        in
        Interval.from
            (Quantity (min (min d1 d2) (min dA dB)))
            (Quantity (max (max d1 d2) (max dA dB)))


{-| Get the bounding box of an elliptical arc.
-}
boundingBox : EllipticalArc3d units coordinates -> BoundingBox3d units coordinatets
boundingBox arc =
    BoundingBox3d.xyz
        (signedDistanceAlong Axis3d.x arc)
        (signedDistanceAlong Axis3d.y arc)
        (signedDistanceAlong Axis3d.z arc)


{-| -}
xDirection : EllipticalArc3d units coordinates -> Direction3d coordinates
xDirection arc =
    SketchPlane3d.xDirection (axes arc)


{-| -}
yDirection : EllipticalArc3d units coordinates -> Direction3d coordinates
yDirection arc =
    SketchPlane3d.yDirection (axes arc)


{-| Reverse the direction of an elliptical arc, so that the start point becomes
the end point and vice versa. Does not change the shape of the arc or any
properties of the underlying ellipse.
-}
reverse : EllipticalArc3d units coordinates -> EllipticalArc3d units coordinates
reverse (Types.EllipticalArc3d properties) =
    Types.EllipticalArc3d
        { properties
            | startAngle =
                properties.startAngle |> Quantity.plus properties.sweptAngle
            , sweptAngle = Quantity.negate properties.sweptAngle
        }


transformBy :
    (Ellipse3d units1 coordinates1 -> Ellipse3d units2 coordinates2)
    -> EllipticalArc3d units1 coordinates1
    -> EllipticalArc3d units2 coordinates2
transformBy ellipseTransformation (Types.EllipticalArc3d properties) =
    Types.EllipticalArc3d
        { ellipse = ellipseTransformation properties.ellipse
        , startAngle = properties.startAngle
        , sweptAngle = properties.sweptAngle
        }


{-| Scale an elliptical arc about a given point by a given scale.
-}
scaleAbout :
    Point3d units coordinates
    -> Float
    -> EllipticalArc3d units coordinates
    -> EllipticalArc3d units coordinates
scaleAbout point scale arc =
    transformBy (Ellipse3d.scaleAbout point scale) arc


{-| Rotate an elliptical arc around a given axis by a given angle.
-}
rotateAround :
    Axis3d units coordinates
    -> Angle
    -> EllipticalArc3d units coordinates
    -> EllipticalArc3d units coordinates
rotateAround axis angle arc =
    transformBy (Ellipse3d.rotateAround axis angle) arc


{-| Translate an elliptical arc by a given displacement.
-}
translateBy :
    Vector3d units coordinates
    -> EllipticalArc3d units coordinates
    -> EllipticalArc3d units coordinates
translateBy displacement arc =
    transformBy (Ellipse3d.translateBy displacement) arc


{-| Translate an elliptical arc in a given direction by a given distance.
-}
translateIn :
    Direction3d coordinates
    -> Quantity Float units
    -> EllipticalArc3d units coordinates
    -> EllipticalArc3d units coordinates
translateIn direction distance arc =
    translateBy (Vector3d.withLength distance direction) arc


{-| Mirror an elliptical arc across a given plane.
-}
mirrorAcross :
    Plane3d units coordinates
    -> EllipticalArc3d units coordinates
    -> EllipticalArc3d units coordinates
mirrorAcross plane arc =
    transformBy (Ellipse3d.mirrorAcross plane) arc


{-| Project an elliptical arc onto a plane.
-}
projectOnto :
    Plane3d units coordinates
    -> EllipticalArc3d units coordinates
    -> EllipticalArc3d units coordinates
projectOnto plane arc =
    let
        sketchPlane =
            SketchPlane3d.fromPlane plane
    in
    arc |> projectInto sketchPlane |> on sketchPlane


{-| Project a 3D elliptical arc into a sketch plane.
-}
projectInto :
    SketchPlane3d units coordinates { defines : coordinates2d }
    -> EllipticalArc3d units coordinates
    -> EllipticalArc2d units coordinates2d
projectInto sketchPlane arc =
    let
        centerPoint2d =
            Point3d.projectInto sketchPlane (centerPoint arc)

        xVector3d =
            Vector3d.withLength (xRadius arc) (xDirection arc)

        yVector3d =
            Vector3d.withLength (yRadius arc) (yDirection arc)

        xVector2d =
            Vector3d.projectInto sketchPlane xVector3d

        yVector2d =
            Vector3d.projectInto sketchPlane yVector3d

        -- From https://scicomp.stackexchange.com/questions/8899/robust-algorithm-for-2-times-2-svd
        ( m00, m10 ) =
            Vector2d.components xVector2d

        ( m01, m11 ) =
            Vector2d.components yVector2d

        e =
            Quantity.half (m00 |> Quantity.plus m11)

        f =
            Quantity.half (m00 |> Quantity.minus m11)

        g =
            Quantity.half (m10 |> Quantity.plus m01)

        h =
            Quantity.half (m10 |> Quantity.minus m01)

        q =
            Quantity.sqrt (Quantity.squared e |> Quantity.plus (Quantity.squared h))

        r =
            Quantity.sqrt (Quantity.squared f |> Quantity.plus (Quantity.squared g))

        sx =
            q |> Quantity.plus r

        sy =
            q |> Quantity.minus r

        a1 =
            Angle.atan2 g f

        a2 =
            Angle.atan2 h e

        theta =
            Quantity.half (a2 |> Quantity.minus a1)

        phi =
            Quantity.half (a2 |> Quantity.plus a1)

        axes2d =
            if sy |> Quantity.greaterThan Quantity.zero then
                Frame2d.withAngle phi centerPoint2d

            else
                Frame2d.withAngle phi centerPoint2d
                    |> Frame2d.reverseY
    in
    Types.EllipticalArc2d
        { ellipse =
            Types.Ellipse2d
                { axes = axes2d
                , xRadius = sx
                , yRadius = Quantity.abs sy
                }
        , startAngle = startAngle arc |> Quantity.plus theta
        , sweptAngle = sweptAngle arc
        }


{-| Take an elliptical arc defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> EllipticalArc3d units globalCoordinates
    -> EllipticalArc3d units localCoordinates
relativeTo frame arc =
    transformBy (Ellipse3d.relativeTo frame) arc


{-| Take an elliptical arc considered to be defined in local coordinates
relative to a given reference frame, and return that arc expressed in global
coordinates.
-}
placeIn :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> EllipticalArc3d units localCoordinates
    -> EllipticalArc3d units globalCoordinates
placeIn frame arc =
    transformBy (Ellipse3d.placeIn frame) arc


{-| Find a conservative upper bound on the magnitude of the second derivative of
an elliptical arc. This can be useful when determining error bounds for various
kinds of linear approximations.
-}
maxSecondDerivativeMagnitude : EllipticalArc3d units coordinates -> Quantity Float units
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


derivativeMagnitude : EllipticalArc3d units coordinates -> Float -> Quantity Float units
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
        { underlyingArc : EllipticalArc3d units coordinates
        , parameterization : ArcLengthParameterization units
        , nondegenerateArc : Nondegenerate units coordinates
        }


{-| Build an arc length parameterization of the given elliptical arc, with a
given accuracy.
-}
arcLengthParameterized :
    { maxError : Quantity Float units }
    -> Nondegenerate units coordinates
    -> ArcLengthParameterized units coordinates
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
pointAlong :
    ArcLengthParameterized units coordinates
    -> Quantity Float units
    -> Point3d units coordinates
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> pointOn parameterized.underlyingArc


{-| Get the midpoint of an elliptical arc. Note that this is the point half way along the
elliptical arc by arc length, which is not in general the same as evaluating at a
parameter value of 0.5.
-}
midpoint : ArcLengthParameterized units coordinates -> Point3d units coordinates
midpoint parameterized =
    let
        halfArcLength =
            Quantity.multiplyBy 0.5 (arcLength parameterized)
    in
    pointAlong parameterized halfArcLength


{-| Get the tangent direction along an elliptical arc at a given arc length.
-}
tangentDirectionAlong :
    ArcLengthParameterized units coordinates
    -> Quantity Float units
    -> Direction3d coordinates
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> tangentDirection parameterized.nondegenerateArc


{-| Get the point and tangent direction along an elliptical arc at a given arc
length.
-}
sampleAlong :
    ArcLengthParameterized units coordinates
    -> Quantity Float units
    -> ( Point3d units coordinates, Direction3d coordinates )
sampleAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> sample parameterized.nondegenerateArc


{-| -}
arcLengthParameterization :
    ArcLengthParameterized units coordinates
    -> ArcLengthParameterization units
arcLengthParameterization (ArcLengthParameterized parameterized) =
    parameterized.parameterization


{-| -}
fromArcLengthParameterized :
    ArcLengthParameterized units coordinates
    -> EllipticalArc3d units coordinates
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingArc


{-| Determine the number of linear segments needed to approximate an elliptical
arc to within a given tolerance.
-}
numApproximationSegments : Quantity Float units -> EllipticalArc3d units coordinats -> Int
numApproximationSegments maxError arc =
    Curve.numApproximationSegments
        { maxError = maxError
        , maxSecondDerivativeMagnitude = maxSecondDerivativeMagnitude arc
        }
