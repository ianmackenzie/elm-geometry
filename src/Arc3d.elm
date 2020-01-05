--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Arc3d exposing
    ( Arc3d
    , on, sweptAround, throughPoints
    , axialDirection, axis, centerPoint, radius, startPoint, midpoint, endPoint, sweptAngle
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , toPolyline
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectInto
    , at, at_
    , relativeTo, placeIn
    , firstDerivative
    )

{-| An `Arc3d` is a section of a circle in 3D, defined by its central axis,
start point and swept angle (the counterclockwise angle around the axis from the
start point to the arc's end point). This module includes functionality for

  - Constructing arcs through given points
  - Scaling, rotating, translating and mirroring arcs
  - Converting arcs between different coordinate systems

@docs Arc3d


# Constructors

@docs on, sweptAround, throughPoints


# Properties

@docs axialDirection, axis, centerPoint, radius, startPoint, midpoint, endPoint, sweptAngle


# Evaluation

@docs pointOn
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, sample


# Linear approximation

@docs toPolyline


# Transformations

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectInto


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative

-}

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Axis3d exposing (Axis3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Parameter1d
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity, Rate)
import Quantity.Extra as Quantity
import SketchPlane3d exposing (SketchPlane3d)
import Unsafe.Direction3d as Direction3d
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Arc3d units coordinates =
    Types.Arc3d units coordinates


{-| Construct a 3D arc lying _on_ a sketch plane by providing a 2D arc specified
in XY coordinates _within_ the sketch plane.

    arc =
        Arc3d.on SketchPlane3d.xz
            (Point2d.meters 3 1
                |> Arc2d.sweptAround (Point2d.meters 1 1)
                    (Angle.degrees 90)
            )

    Arc3d.startPoint arc
    --> Point3d.meters 3 0 1

    Arc3d.endPoint arc
    --> Point3d.meters 1 0 3

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Arc2d units coordinates2d -> Arc3d units coordinates3d
on sketchPlane (Types.Arc2d arc2d) =
    Types.Arc3d
        { startPoint = Point3d.on sketchPlane arc2d.startPoint
        , xDirection = Direction3d.on sketchPlane arc2d.xDirection
        , yDirection =
            Direction3d.on sketchPlane
                (Direction2d.perpendicularTo arc2d.xDirection)
        , sweptAngle = arc2d.sweptAngle
        , signedLength = arc2d.signedLength
        }


{-| Construct an arc by sweeping the given point around the given axis by the
given angle:

    exampleArc =
        Point3d.meters 1 1 0
            |> Arc3d.sweptAround Axis3d.z
                (Angle.degrees 90)

    Arc3d.endPoint exampleArc
    --> Point3d.meters -1 1 0

Positive swept angles result in a counterclockwise (right-handed) rotation
around the given axis and vice versa for negative swept angles. The center point
of the returned arc will lie on the given axis.

-}
sweptAround : Axis3d units coordinates -> Angle -> Point3d units coordinates -> Arc3d units coordinates
sweptAround givenAxis givenSweptAngle givenStartPoint =
    let
        computedCenterPoint =
            givenStartPoint |> Point3d.projectOntoAxis givenAxis

        axisDirection =
            Axis3d.direction givenAxis

        yVector =
            Vector3d.from givenStartPoint computedCenterPoint
    in
    case Vector3d.direction yVector of
        Just computedYDirection ->
            let
                computedRadius =
                    Vector3d.length yVector

                computedXDirection =
                    Direction3d.unsafeCrossProduct
                        computedYDirection
                        axisDirection
            in
            Types.Arc3d
                { startPoint = givenStartPoint
                , sweptAngle = givenSweptAngle
                , signedLength = Quantity.rTheta computedRadius givenSweptAngle
                , xDirection = computedXDirection
                , yDirection = computedYDirection
                }

        Nothing ->
            let
                ( computedXDirection, computedYDirection ) =
                    Direction3d.perpendicularBasis axisDirection
            in
            Types.Arc3d
                { startPoint = givenStartPoint
                , sweptAngle = givenSweptAngle
                , signedLength = Quantity.zero
                , xDirection = computedXDirection
                , yDirection = computedYDirection
                }


type TempCoordinates2d
    = TempCoordinates2d


{-| Attempt to construct an arc that starts at the first given point, passes
through the second given point and ends at the third given point. If the three
points are collinear, returns `Nothing`.

    Arc3d.throughPoints
        (Point3d.meters 0 0 1)
        Point3d.origin
        (Point3d.meters 0 1 0)
    --> Just <|
    -->     Arc3d.on SketchPlane3d.yz
    -->         (Point2d.meters 0 1
    -->             |> Arc2d.sweptAround
    -->                 (Point2d.meters 0.5 0.5)
    -->                 (Angle.degrees 180)
    -->         )

-}
throughPoints : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Maybe (Arc3d units coordinates)
throughPoints firstPoint secondPoint thirdPoint =
    SketchPlane3d.throughPoints firstPoint secondPoint thirdPoint
        |> Maybe.andThen
            (\sketchPlane ->
                Arc2d.throughPoints
                    (Point3d.projectInto sketchPlane firstPoint)
                    (Point3d.projectInto sketchPlane secondPoint)
                    (Point3d.projectInto sketchPlane thirdPoint)
                    |> Maybe.map (on sketchPlane)
            )


{-| Convert an arc from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Arc3d units1 coordinates -> Arc3d units2 coordinates
at rate (Types.Arc3d arc) =
    Types.Arc3d
        { startPoint = Point3d.at rate arc.startPoint
        , xDirection = arc.xDirection
        , yDirection = arc.yDirection
        , signedLength = Quantity.at rate arc.signedLength
        , sweptAngle = arc.sweptAngle
        }


{-| Convert an arc from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Arc3d units1 coordinates -> Arc3d units2 coordinates
at_ rate arc =
    at (Quantity.inverse rate) arc


{-| Get the axial direction of an arc.

    Arc3d.axialDirection exampleArc
    --> Direction3d.z

-}
axialDirection : Arc3d units coordinates -> Direction3d coordinates
axialDirection (Types.Arc3d arc) =
    Direction3d.unsafeCrossProduct arc.xDirection arc.yDirection


{-| Get the central axis of an arc. The origin point of the axis will be equal
to the center point of the arc.
-}
axis : Arc3d units coordinates -> Axis3d units coordinates
axis arc =
    Axis3d.through (centerPoint arc) (axialDirection arc)


{-| Get the center point of an arc.
-}
centerPoint : Arc3d units coordinates -> Point3d units coordinates
centerPoint (Types.Arc3d arc) =
    let
        computedRadius =
            Quantity.lOverTheta arc.signedLength arc.sweptAngle
    in
    arc.startPoint |> Point3d.translateIn arc.yDirection computedRadius


{-| Get the radius of an arc.
-}
radius : Arc3d units coordinates -> Quantity Float units
radius (Types.Arc3d arc) =
    Quantity.lOverTheta arc.signedLength arc.sweptAngle


{-| Get the start point of an arc.
-}
startPoint : Arc3d units coordinates -> Point3d units coordinates
startPoint (Types.Arc3d arc) =
    arc.startPoint


{-| Get the midpoint of an arc.
-}
midpoint : Arc3d units coordinates -> Point3d units coordinates
midpoint arc =
    pointOn arc 0.5


{-| Get the end point of an arc.
-}
endPoint : Arc3d units coordinates -> Point3d units coordinates
endPoint arc =
    pointOn arc 1


{-| Get the point along an arc at a given parameter value.
-}
pointOn : Arc3d units coordinates -> Float -> Point3d units coordinates
pointOn (Types.Arc3d arc) parameterValue =
    let
        x0 =
            Point3d.xCoordinate arc.startPoint

        y0 =
            Point3d.yCoordinate arc.startPoint

        z0 =
            Point3d.zCoordinate arc.startPoint

        x1 =
            Direction3d.xComponent arc.xDirection

        y1 =
            Direction3d.yComponent arc.xDirection

        z1 =
            Direction3d.zComponent arc.xDirection

        x2 =
            Direction3d.xComponent arc.yDirection

        y2 =
            Direction3d.yComponent arc.yDirection

        z2 =
            Direction3d.zComponent arc.yDirection

        arcSignedLength =
            arc.signedLength

        arcSweptAngle =
            arc.sweptAngle
    in
    if arcSweptAngle == Quantity.zero then
        let
            distance =
                Quantity.multiplyBy parameterValue arcSignedLength

            px =
                x0 |> Quantity.plus (distance |> Quantity.multiplyBy x1)

            py =
                y0 |> Quantity.plus (distance |> Quantity.multiplyBy y1)

            pz =
                z0 |> Quantity.plus (distance |> Quantity.multiplyBy z1)
        in
        Point3d.xyz px py pz

    else
        let
            theta =
                Quantity.multiplyBy parameterValue arcSweptAngle

            arcRadius =
                Quantity.lOverTheta arcSignedLength arcSweptAngle

            x =
                Quantity.rSinTheta arcRadius theta

            y =
                if
                    Quantity.abs theta
                        |> Quantity.lessThan
                            (Angle.radians (pi / 2))
                then
                    x
                        |> Quantity.multiplyBy
                            (Angle.tan (Quantity.multiplyBy 0.5 theta))

                else
                    Quantity.multiplyBy (1 - Angle.cos theta) arcRadius

            px =
                x0 |> Quantity.plus (Quantity.aXbY x1 x x2 y)

            py =
                y0 |> Quantity.plus (Quantity.aXbY y1 x y2 y)

            pz =
                z0 |> Quantity.plus (Quantity.aXbY z1 x z2 y)
        in
        Point3d.xyz px py pz


{-| Get the first derivative of an arc at a given parameter value.
-}
firstDerivative : Arc3d units coordinates -> Float -> Vector3d units coordinates
firstDerivative (Types.Arc3d arc) =
    let
        x1 =
            Direction3d.xComponent arc.xDirection

        y1 =
            Direction3d.yComponent arc.xDirection

        z1 =
            Direction3d.zComponent arc.xDirection

        x2 =
            Direction3d.xComponent arc.yDirection

        y2 =
            Direction3d.yComponent arc.yDirection

        z2 =
            Direction3d.zComponent arc.yDirection

        arcSweptAngle =
            arc.sweptAngle

        arcSignedLength =
            arc.signedLength
    in
    \parameterValue ->
        let
            angle =
                Quantity.multiplyBy parameterValue arcSweptAngle

            cosAngle =
                Angle.cos angle

            sinAngle =
                Angle.sin angle
        in
        Vector3d.xyz
            (arcSignedLength
                |> Quantity.multiplyBy (cosAngle * x1 + sinAngle * x2)
            )
            (arcSignedLength
                |> Quantity.multiplyBy (cosAngle * y1 + sinAngle * y2)
            )
            (arcSignedLength
                |> Quantity.multiplyBy (cosAngle * z1 + sinAngle * z2)
            )


{-| Represents a nondegenerate spline (one that has finite, non-zero length).
-}
type Nondegenerate units coordinates
    = Nondegenerate (Arc3d units coordinates)


{-| Attempt to construct a nondegenerate arc from a general `Arc3d`. If the arc
is in fact degenerate (consists of a single point), returns an `Err` with that
point.
-}
nondegenerate : Arc3d units coordinates -> Result (Point3d units coordinates) (Nondegenerate units coordinates)
nondegenerate arc =
    let
        (Types.Arc3d properties) =
            arc
    in
    if properties.signedLength == Quantity.zero then
        Err (startPoint arc)

    else
        Ok (Nondegenerate arc)


{-| Convert a nondegenerate arc back to a general `Arc3d`.
-}
fromNondegenerate : Nondegenerate units coordinates -> Arc3d units coordinates
fromNondegenerate (Nondegenerate arc) =
    arc


{-| Get the tangent direction to a nondegenerate arc at a given parameter
value.
-}
tangentDirection : Nondegenerate units coordinates -> Float -> Direction3d coordinates
tangentDirection (Nondegenerate (Types.Arc3d arc)) parameterValue =
    let
        x1 =
            Direction3d.xComponent arc.xDirection

        y1 =
            Direction3d.yComponent arc.xDirection

        z1 =
            Direction3d.zComponent arc.xDirection

        x2 =
            Direction3d.xComponent arc.yDirection

        y2 =
            Direction3d.yComponent arc.yDirection

        z2 =
            Direction3d.zComponent arc.yDirection

        arcSweptAngle =
            arc.sweptAngle

        angle =
            Quantity.multiplyBy parameterValue arcSweptAngle

        cosAngle =
            Angle.cos angle

        sinAngle =
            Angle.sin angle
    in
    Direction3d.unsafe
        { x = cosAngle * x1 + sinAngle * x2
        , y = cosAngle * y1 + sinAngle * y2
        , z = cosAngle * z1 + sinAngle * z2
        }


{-| Get both the point and tangent direction of a nondegenerate arc at a given
parameter value.
-}
sample : Nondegenerate units coordinates -> Float -> ( Point3d units coordinates, Direction3d coordinates )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


numApproximationSegments : Quantity Float units -> Arc3d units coordinates -> Int
numApproximationSegments maxError arc =
    if sweptAngle arc == Quantity.zero then
        1

    else if maxError |> Quantity.lessThanOrEqualTo Quantity.zero then
        0

    else if
        maxError
            |> Quantity.greaterThanOrEqualTo
                (Quantity.multiplyBy 2 (radius arc))
    then
        1

    else
        let
            maxSegmentAngle =
                Quantity.multiplyBy 2
                    (Angle.acos (1 - Quantity.ratio maxError (radius arc)))
        in
        ceiling (Quantity.ratio (Quantity.abs (sweptAngle arc)) maxSegmentAngle)


{-| Approximate an arc as a polyline, within a given tolerance:

    exampleArc
        |> Arc3d.toPolyline
            { maxError = Length.meters 0.1 }
    --> Polyline3d.fromVertices
    -->     [ Point3d.meters 1 1 0
    -->     , Point3d.meters 0.366 1.366 0
    -->     , Point3d.meters -0.366 1.366 0
    -->     , Point3d.meters -1 1 0
    -->     ]

In this example, every point on the returned polyline will be within 0.1 meters
of the original arc.

-}
toPolyline : { maxError : Quantity Float units } -> Arc3d units coordinates -> Polyline3d units coordinates
toPolyline { maxError } arc =
    let
        numSegments =
            numApproximationSegments maxError arc

        points =
            Parameter1d.steps numSegments (pointOn arc)
    in
    Polyline3d.fromVertices points


{-| Get the swept angle of an arc. A positive swept angle means that the arc is
formed by rotating the given start point counterclockwise around the central
axis, and vice versa for a negative angle.
-}
sweptAngle : Arc3d units coordinates -> Angle
sweptAngle (Types.Arc3d properties) =
    properties.sweptAngle


{-| Reverse the direction of an arc, so that the start point becomes the end
point and vice versa. The resulting arc will have the same axis as the original
but a swept angle with the opposite sign.
-}
reverse : Arc3d units coordinates -> Arc3d units coordinates
reverse ((Types.Arc3d arc) as arc_) =
    let
        x1 =
            Direction3d.xComponent arc.xDirection

        y1 =
            Direction3d.yComponent arc.xDirection

        z1 =
            Direction3d.zComponent arc.xDirection

        x2 =
            Direction3d.xComponent arc.yDirection

        y2 =
            Direction3d.yComponent arc.yDirection

        z2 =
            Direction3d.zComponent arc.yDirection

        arcSweptAngle =
            arc.sweptAngle

        cosAngle =
            Angle.cos arcSweptAngle

        sinAngle =
            Angle.sin arcSweptAngle
    in
    Types.Arc3d
        { startPoint = endPoint arc_
        , sweptAngle = Quantity.negate arcSweptAngle
        , signedLength = Quantity.negate arc.signedLength
        , xDirection =
            Direction3d.unsafe
                { x = x1 * cosAngle + x2 * sinAngle
                , y = y1 * cosAngle + y2 * sinAngle
                , z = z1 * cosAngle + z2 * sinAngle
                }
        , yDirection =
            Direction3d.unsafe
                { x = x2 * cosAngle - x1 * sinAngle
                , y = y2 * cosAngle - y1 * sinAngle
                , z = z2 * cosAngle - z1 * sinAngle
                }
        }


{-| Scale an arc about the given center point by the given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> Arc3d units coordinates -> Arc3d units coordinates
scaleAbout point scale (Types.Arc3d arc) =
    Types.Arc3d
        { startPoint = Point3d.scaleAbout point scale arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = Quantity.multiplyBy (abs scale) arc.signedLength
        , xDirection =
            if scale >= 0 then
                arc.xDirection

            else
                Direction3d.reverse arc.xDirection
        , yDirection =
            if scale >= 0 then
                arc.yDirection

            else
                Direction3d.reverse arc.yDirection
        }


{-| Rotate an arc around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> Arc3d units coordinates -> Arc3d units coordinates
rotateAround rotationAxis angle (Types.Arc3d arc) =
    Types.Arc3d
        { startPoint = Point3d.rotateAround rotationAxis angle arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = arc.signedLength
        , xDirection =
            Direction3d.rotateAround rotationAxis angle arc.xDirection
        , yDirection =
            Direction3d.rotateAround rotationAxis angle arc.yDirection
        }


{-| Translate an arc by a given displacement.
-}
translateBy : Vector3d units coordinates -> Arc3d units coordinates -> Arc3d units coordinates
translateBy displacement (Types.Arc3d arc) =
    Types.Arc3d
        { startPoint = Point3d.translateBy displacement arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = arc.signedLength
        , xDirection = arc.xDirection
        , yDirection = arc.yDirection
        }


{-| Translate an arc in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> Arc3d units coordinates -> Arc3d units coordinates
translateIn direction distance arc =
    translateBy (Vector3d.withLength distance direction) arc


{-| Mirror an arc across a given plane. This flips the sign of the arc's swept
angle.
-}
mirrorAcross : Plane3d units coordinates -> Arc3d units coordinates -> Arc3d units coordinates
mirrorAcross plane (Types.Arc3d arc) =
    Types.Arc3d
        { startPoint = Point3d.mirrorAcross plane arc.startPoint
        , sweptAngle = Quantity.negate arc.sweptAngle
        , signedLength = Quantity.negate arc.signedLength
        , xDirection =
            Direction3d.reverse (Direction3d.mirrorAcross plane arc.xDirection)
        , yDirection = Direction3d.mirrorAcross plane arc.yDirection
        }


{-| Project an arc into a sketch plane. Note that the result is an elliptical
arc, not a circular one!
-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Arc3d units coordinates3d -> Types.EllipticalArc2d units coordinates2d
projectInto sketchPlane arc =
    let
        arcAxialDirection =
            axialDirection arc

        radialVector =
            Vector3d.from (centerPoint arc) (startPoint arc)

        projectedRadialVector =
            radialVector |> Vector3d.projectInto sketchPlane

        candidateXDirection2d =
            case Direction3d.projectInto sketchPlane arcAxialDirection of
                Just tempYDirection2d ->
                    tempYDirection2d |> Direction2d.rotateClockwise

                Nothing ->
                    case Vector2d.direction projectedRadialVector of
                        Just projectedRadialDirection ->
                            projectedRadialDirection

                        Nothing ->
                            Direction2d.x

        xDirection2d =
            if
                projectedRadialVector
                    |> Vector2d.componentIn candidateXDirection2d
                    |> Quantity.greaterThanOrEqualTo Quantity.zero
            then
                candidateXDirection2d

            else
                Direction2d.reverse candidateXDirection2d

        xDirection3d =
            Direction3d.on sketchPlane xDirection2d

        xVector3d =
            Direction3d.toVector xDirection3d

        arcRadius =
            radius arc

        normalComponent =
            arcAxialDirection
                |> Direction3d.componentIn
                    (SketchPlane3d.normalDirection sketchPlane)

        arcPlaneSameOrientation =
            normalComponent >= 0

        yRatio =
            abs normalComponent

        ellipticalStartAngle =
            let
                x =
                    xVector3d |> Vector3d.dot radialVector

                y =
                    xVector3d
                        |> Vector3d.cross radialVector
                        |> Vector3d.componentIn arcAxialDirection

                arcStartAngle =
                    Angle.atan2 y x
            in
            if arcPlaneSameOrientation then
                arcStartAngle

            else
                Quantity.negate arcStartAngle

        ellipticalSweptAngle =
            if arcPlaneSameOrientation then
                sweptAngle arc

            else
                Quantity.negate (sweptAngle arc)
    in
    Types.EllipticalArc2d
        { ellipse =
            Types.Ellipse2d
                { axes =
                    Frame2d.withXDirection xDirection2d
                        (centerPoint arc |> Point3d.projectInto sketchPlane)
                , xRadius = arcRadius
                , yRadius = Quantity.multiplyBy yRatio arcRadius
                }
        , startAngle = ellipticalStartAngle
        , sweptAngle = ellipticalSweptAngle
        }


{-| Take an arc defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Arc3d units globalCoordinates -> Arc3d units localCoordinates
relativeTo frame (Types.Arc3d arc) =
    if Frame3d.isRightHanded frame then
        Types.Arc3d
            { startPoint = Point3d.relativeTo frame arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = Direction3d.relativeTo frame arc.xDirection
            , yDirection = Direction3d.relativeTo frame arc.yDirection
            }

    else
        Types.Arc3d
            { startPoint = Point3d.relativeTo frame arc.startPoint
            , sweptAngle = Quantity.negate arc.sweptAngle
            , signedLength = Quantity.negate arc.signedLength
            , xDirection =
                Direction3d.relativeTo frame arc.xDirection
                    |> Direction3d.reverse
            , yDirection = Direction3d.relativeTo frame arc.yDirection
            }


{-| Take an arc considered to be defined in local coordinates relative to a
given reference frame, and return that arc expressed in global coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Arc3d units localCoordinates -> Arc3d units globalCoordinates
placeIn frame (Types.Arc3d arc) =
    if Frame3d.isRightHanded frame then
        Types.Arc3d
            { startPoint = Point3d.placeIn frame arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = Direction3d.placeIn frame arc.xDirection
            , yDirection = Direction3d.placeIn frame arc.yDirection
            }

    else
        Types.Arc3d
            { startPoint = Point3d.placeIn frame arc.startPoint
            , sweptAngle = Quantity.negate arc.sweptAngle
            , signedLength = Quantity.negate arc.signedLength
            , xDirection =
                Direction3d.placeIn frame arc.xDirection
                    |> Direction3d.reverse
            , yDirection = Direction3d.placeIn frame arc.yDirection
            }
