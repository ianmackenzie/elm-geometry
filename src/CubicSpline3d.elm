--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module CubicSpline3d exposing
    ( CubicSpline3d
    , fromControlPoints, fromEndpoints, on, fromQuadraticSpline
    , bSplineSegments, bSplineIntervals
    , startPoint, firstControlPoint, secondControlPoint, thirdControlPoint, fourthControlPoint, endPoint, startDerivative, endDerivative, boundingBox
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , segments, approximate
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto
    , at, at_
    , relativeTo, placeIn, projectInto
    , bisect, splitAt
    , ArcLengthParameterized, arcLengthParameterized, arcLength, midpoint
    , pointAlong, tangentDirectionAlong, sampleAlong
    , arcLengthParameterization, fromArcLengthParameterized
    , firstDerivative, secondDerivative, thirdDerivative, maxSecondDerivativeMagnitude, numApproximationSegments
    )

{-| A `CubicSpline3d` is a cubic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 3D defined by a start point, end point and two control points. This module
contains functionality for

  - Constructing splines
  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs CubicSpline3d


# Constructors

@docs fromControlPoints, fromEndpoints, on, fromQuadraticSpline


## B-splines

@docs bSplineSegments, bSplineIntervals


# Properties

@docs startPoint, firstControlPoint, secondControlPoint, thirdControlPoint, fourthControlPoint, endPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, sample


# Linear approximation

@docs segments, approximate


# Transformations

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn, projectInto


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, midpoint

For the following evaluation functions, the given arc length will be clamped to
the arc length of the spline, so the result will always be on the spline.

@docs pointAlong, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `CubicSpline3d`. If you need to do something fancy, you can extract
these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Advanced

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, secondDerivative, thirdDerivative, maxSecondDerivativeMagnitude, numApproximationSegments

-}

import Angle exposing (Angle)
import ArcLengthParameterization exposing (ArcLengthParameterization)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import CubicSpline2d exposing (CubicSpline2d)
import Curve
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Interval exposing (Interval)
import LineSegment3d exposing (LineSegment3d)
import Parameter1d
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity(..), Rate)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias CubicSpline3d units coordinates =
    Types.CubicSpline3d units coordinates


{-| Construct a spline from its four control points:

    exampleSpline =
        CubicSpline3d.fromControlPoints
            (Point3d.meters 1 1 1)
            (Point3d.meters 3 1 1)
            (Point3d.meters 3 3 1)
            (Point3d.meters 3 3 3)

-}
fromControlPoints : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> CubicSpline3d units coordinates
fromControlPoints p1 p2 p3 p4 =
    Types.CubicSpline3d
        { firstControlPoint = p1
        , secondControlPoint = p2
        , thirdControlPoint = p3
        , fourthControlPoint = p4
        }


{-| Construct a spline from a given start point with a given start derivative,
to a given end point with a given end derivative, like so:

![Cubic spline from endpoints](https://ianmackenzie.github.io/elm-geometry/1.0.0/CubicSpline2d/fromEndpoints.svg)

For good results, in most cases the length of each derivative vector should be
roughly equal to the length of the resulting spline.

-}
fromEndpoints : Point3d units coordinates -> Vector3d units coordinates -> Point3d units coordinates -> Vector3d units coordinates -> CubicSpline3d units coordinates
fromEndpoints givenStartPoint givenStartDerivative givenEndPoint givenEndDerivative =
    fromControlPoints
        givenStartPoint
        (givenStartPoint |> Point3d.translateBy (Vector3d.scaleBy (1 / 3) givenStartDerivative))
        (givenEndPoint |> Point3d.translateBy (Vector3d.scaleBy (-1 / 3) givenEndDerivative))
        givenEndPoint


{-| Construct a 3D spline lying _on_ a sketch plane by providing a 2D spline
specified in XY coordinates _within_ the sketch plane.

    CubicSpline3d.on SketchPlane3d.xz <|
        CubicSpline2d.fromControlPoints
            (Point2d.meters 1 1)
            (Point2d.meters 3 4)
            (Point2d.meters 5 1)
            (Point2d.meters 7 4)
    --> CubicSpline3d.fromControlPoints
    -->     (Point3d.meters 1 0 1)
    -->     (Point3d.meters 3 0 4)
    -->     (Point3d.meters 5 0 1)
    -->     (Point3d.meters 7 0 4)

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> CubicSpline2d units coordinates2d -> CubicSpline3d units coordinates3d
on sketchPlane spline2d =
    fromControlPoints
        (Point3d.on sketchPlane (CubicSpline2d.firstControlPoint spline2d))
        (Point3d.on sketchPlane (CubicSpline2d.secondControlPoint spline2d))
        (Point3d.on sketchPlane (CubicSpline2d.thirdControlPoint spline2d))
        (Point3d.on sketchPlane (CubicSpline2d.fourthControlPoint spline2d))


{-| Convert a quadratic spline into the equivalent cubic spline (every quadratic
spline can be represented exactly as a cubic spline).

    quadraticSpline =
        QuadraticSpline3d.fromControlPoints
            Point3d.origin
            (Point3d.meters 3 0 0)
            (Point3d.meters 3 3 0)

    CubicSpline3d.fromQuadraticSpline quadraticSpline
    --> CubicSpline3d.fromControlPoints
    -->     Point3d.origin
    -->     (Point3d.meters 2 0 0)
    -->     (Point3d.meters 3 1 0)
    -->     (Point3d.meters 3 3 0)

-}
fromQuadraticSpline : QuadraticSpline3d units coordinates -> CubicSpline3d units coordinates
fromQuadraticSpline quadraticSpline =
    let
        quadraticFirstControlPoint =
            QuadraticSpline3d.firstControlPoint quadraticSpline

        quadraticSecondControlPoint =
            QuadraticSpline3d.secondControlPoint quadraticSpline

        quadraticThirdControlPoint =
            QuadraticSpline3d.thirdControlPoint quadraticSpline

        cubicFirstControlPoint =
            quadraticFirstControlPoint

        cubicSecondControlPoint =
            Point3d.interpolateFrom quadraticFirstControlPoint quadraticSecondControlPoint (2 / 3)

        cubicThirdControlPoint =
            Point3d.interpolateFrom quadraticThirdControlPoint quadraticSecondControlPoint (2 / 3)

        cubicFourthControlPoint =
            quadraticThirdControlPoint
    in
    fromControlPoints
        cubicFirstControlPoint
        cubicSecondControlPoint
        cubicThirdControlPoint
        cubicFourthControlPoint


{-| Construct a [B-spline](https://mathworld.wolfram.com/B-Spline.html) from a
list of knot values and a list of control points, and return the individual
segments of that B-spline as a list.

The number of knots should be two greater than the number of control points; any
extra knots or control points will be dropped. In most cases the first and last
knots will be repeated three times; for example, the knots

    [ 0, 0, 0, 1, 2, 3, 4, 4, 4 ]

could be used along with 7 control points to form 4 spline segments.

Note that a popular alternate convention uses two extra 'dummy' knot values at
the start and end, so if you see an example where the number of knots is _four_
greater than the number of control points (especially if you also notice that
the first and last knots are repeated four times instead of three!) then you
should drop the first and last knot values.

Knot values should be given in ascending order but will be sorted if necessary.

-}
bSplineSegments : List Float -> List (Point3d units coordinates) -> List (CubicSpline3d units coordinates)
bSplineSegments givenKnots givenControlPoints =
    case ( List.sort givenKnots, givenControlPoints ) of
        ( u0 :: u1 :: u2 :: u3 :: u4 :: u5s, b012 :: b123 :: b234 :: b345s ) ->
            bSplineSegmentsHelp u0 u1 u2 u3 u4 u5s b012 b123 b234 b345s []

        _ ->
            []


bSplineSegmentsHelp :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> List Float
    -> Point3d units coordinates
    -> Point3d units coordinates
    -> Point3d units coordinates
    -> List (Point3d units coordinates)
    -> List (CubicSpline3d units coordinates)
    -> List (CubicSpline3d units coordinates)
bSplineSegmentsHelp u0 u1 u2 u3 u4 u5s b012 b123 b234 b345s accumulated =
    case ( u5s, b345s ) of
        ( u5 :: u6s, b345 :: b456s ) ->
            if u2 == u3 then
                bSplineSegmentsHelp u1 u2 u3 u4 u5 u6s b123 b234 b345 b456s accumulated

            else
                let
                    b122 =
                        Point3d.interpolateFrom b012 b123 ((u2 - u0) / (u3 - u0))

                    b223 =
                        Point3d.interpolateFrom b123 b234 ((u2 - u1) / (u4 - u1))

                    b233 =
                        Point3d.interpolateFrom b123 b234 ((u3 - u1) / (u4 - u1))

                    b334 =
                        Point3d.interpolateFrom b234 b345 ((u3 - u2) / (u5 - u2))

                    b222 =
                        Point3d.interpolateFrom b122 b223 ((u2 - u1) / (u3 - u1))

                    b333 =
                        Point3d.interpolateFrom b233 b334 ((u3 - u2) / (u4 - u2))

                    segment =
                        fromControlPoints b222 b223 b233 b333
                in
                bSplineSegmentsHelp u1 u2 u3 u4 u5 u6s b123 b234 b345 b456s (segment :: accumulated)

        _ ->
            List.reverse accumulated


{-| For a given set of B-spline knots, return the corresponding intervals
between knots that correspond to individual spline [segments](#bSplineSegments).
-}
bSplineIntervals : List Float -> List (Interval Float)
bSplineIntervals givenKnots =
    case List.sort givenKnots of
        _ :: _ :: u2 :: u3 :: u4 :: u5s ->
            bSplineIntervalsHelp u2 u3 u4 u5s []

        _ ->
            []


bSplineIntervalsHelp :
    Float
    -> Float
    -> Float
    -> List Float
    -> List (Interval Float)
    -> List (Interval Float)
bSplineIntervalsHelp u2 u3 u4 u5s accumulated =
    case u5s of
        u5 :: u6s ->
            if u2 == u3 then
                bSplineIntervalsHelp u3 u4 u5 u6s accumulated

            else
                bSplineIntervalsHelp u3 u4 u5 u6s (Interval.from u2 u3 :: accumulated)

        [] ->
            List.reverse accumulated


{-| Convert a spline from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> CubicSpline3d units1 coordinates -> CubicSpline3d units2 coordinates
at rate (Types.CubicSpline3d spline) =
    Types.CubicSpline3d
        { firstControlPoint = Point3d.at rate spline.firstControlPoint
        , secondControlPoint = Point3d.at rate spline.secondControlPoint
        , thirdControlPoint = Point3d.at rate spline.thirdControlPoint
        , fourthControlPoint = Point3d.at rate spline.fourthControlPoint
        }


{-| Convert a spline from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> CubicSpline3d units1 coordinates -> CubicSpline3d units2 coordinates
at_ rate spline =
    at (Quantity.inverse rate) spline


{-| Get the start point of a spline. Equal to
[`firstControlPoint`](#firstControlPoint).
-}
startPoint : CubicSpline3d units coordinates -> Point3d units coordinates
startPoint (Types.CubicSpline3d spline) =
    spline.firstControlPoint


{-| Get the end point of a spline. Equal to
[`fourthControlPoint`](#fourthControlPoint).
-}
endPoint : CubicSpline3d units coordinates -> Point3d units coordinates
endPoint (Types.CubicSpline3d spline) =
    spline.fourthControlPoint


{-| Get the first control point of a spline. Equal to
[`startPoint`](#startPoint).
-}
firstControlPoint : CubicSpline3d units coordinates -> Point3d units coordinates
firstControlPoint (Types.CubicSpline3d spline) =
    spline.firstControlPoint


{-| Get the second control point of a spline.
-}
secondControlPoint : CubicSpline3d units coordinates -> Point3d units coordinates
secondControlPoint (Types.CubicSpline3d spline) =
    spline.secondControlPoint


{-| Get the third control point of a spline.
-}
thirdControlPoint : CubicSpline3d units coordinates -> Point3d units coordinates
thirdControlPoint (Types.CubicSpline3d spline) =
    spline.thirdControlPoint


{-| Get the fourth and last control point of a spline.
Equal to [`endPoint`](#endPoint).
-}
fourthControlPoint : CubicSpline3d units coordinates -> Point3d units coordinates
fourthControlPoint (Types.CubicSpline3d spline) =
    spline.fourthControlPoint


{-| Get the start derivative of a spline. This is equal to three times the
vector from the spline's start point to its start control point.

    CubicSpline3d.startDerivative exampleSpline
    --> Vector3d.meters 6 0 0

-}
startDerivative : CubicSpline3d units coordinates -> Vector3d units coordinates
startDerivative spline =
    Vector3d.from (firstControlPoint spline) (secondControlPoint spline)
        |> Vector3d.scaleBy 3


{-| Get the end derivative of a spline. This is equal to three times the vector
from the spline's end control point to its end point.

    CubicSpline3d.endDerivative exampleSpline
    --> Vector3d.meters 0 0 6

-}
endDerivative : CubicSpline3d units coordinates -> Vector3d units coordinates
endDerivative spline =
    Vector3d.from (thirdControlPoint spline) (fourthControlPoint spline)
        |> Vector3d.scaleBy 3


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
volume than the spline itself).
-}
boundingBox : CubicSpline3d units coordinates -> BoundingBox3d units coordinates
boundingBox spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        x1 =
            Point3d.xCoordinate p1

        y1 =
            Point3d.yCoordinate p1

        z1 =
            Point3d.zCoordinate p1

        x2 =
            Point3d.xCoordinate p2

        y2 =
            Point3d.yCoordinate p2

        z2 =
            Point3d.zCoordinate p2

        x3 =
            Point3d.xCoordinate p3

        y3 =
            Point3d.yCoordinate p3

        z3 =
            Point3d.zCoordinate p3

        x4 =
            Point3d.xCoordinate p4

        y4 =
            Point3d.yCoordinate p4

        z4 =
            Point3d.zCoordinate p4
    in
    BoundingBox3d.fromExtrema
        { minX = Quantity.min (Quantity.min x1 x2) (Quantity.min x3 x4)
        , maxX = Quantity.max (Quantity.max x1 x2) (Quantity.max x3 x4)
        , minY = Quantity.min (Quantity.min y1 y2) (Quantity.min y3 y4)
        , maxY = Quantity.max (Quantity.max y1 y2) (Quantity.max y3 y4)
        , minZ = Quantity.min (Quantity.min z1 z2) (Quantity.min z3 z4)
        , maxZ = Quantity.max (Quantity.max z1 z2) (Quantity.max z3 z4)
        }


{-| Get a point on a spline at a given parameter value.
-}
pointOn : CubicSpline3d units coordinates -> Float -> Point3d units coordinates
pointOn spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 parameterValue

        q2 =
            Point3d.interpolateFrom p2 p3 parameterValue

        q3 =
            Point3d.interpolateFrom p3 p4 parameterValue

        r1 =
            Point3d.interpolateFrom q1 q2 parameterValue

        r2 =
            Point3d.interpolateFrom q2 q3 parameterValue
    in
    Point3d.interpolateFrom r1 r2 parameterValue


{-| Represents a nondegenerate spline (one that has finite, non-zero length).
-}
type Nondegenerate units coordinates
    = NonZeroThirdDerivative (CubicSpline3d units coordinates) (Direction3d coordinates)
    | NonZeroSecondDerivative (CubicSpline3d units coordinates) (Direction3d coordinates)
    | NonZeroFirstDerivative (CubicSpline3d units coordinates) (Direction3d coordinates)


{-| Attempt to construct a nondegenerate spline from a general `CubicSpline3d`.
If the spline is in fact degenerate (consists of a single point), returns an
`Err` with that point.
-}
nondegenerate : CubicSpline3d units coordinates -> Result (Point3d units coordinates) (Nondegenerate units coordinates)
nondegenerate spline =
    case Vector3d.direction (thirdDerivative spline) of
        Just direction ->
            -- Third derivative is non-zero, so if all else fails we can fall
            -- back on it to provide a tangent direction
            Ok (NonZeroThirdDerivative spline direction)

        Nothing ->
            let
                -- Third derivative is zero, so second derivative is constant -
                -- evaluate it at an arbitrary point to get its value
                secondDerivativeVector =
                    secondDerivative spline 0
            in
            case Vector3d.direction secondDerivativeVector of
                Just direction ->
                    -- Second derivative is non-zero, so if all else fails we
                    -- can fall back on it to provide a tangent direction
                    Ok (NonZeroSecondDerivative spline direction)

                Nothing ->
                    let
                        -- Second and third derivatives are zero, so first
                        -- derivative is constant - evaluate it at an arbitrary
                        -- point to get its value
                        firstDerivativeVector =
                            firstDerivative spline 0
                    in
                    case Vector3d.direction firstDerivativeVector of
                        Just direction ->
                            -- First derivative is constant and non-zero, so the
                            -- tangent direction will always be equal to the
                            -- first derivative direction
                            Ok (NonZeroFirstDerivative spline direction)

                        Nothing ->
                            Err (startPoint spline)


{-| Convert a nondegenerate spline back to a general `CubicSpline3d`.
-}
fromNondegenerate : Nondegenerate units coordinates -> CubicSpline3d units coordinates
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroThirdDerivative spline _ ->
            spline

        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| Get the tangent direction to a nondegenerate spline at a given parameter
value.
-}
tangentDirection : Nondegenerate units coordinates -> Float -> Direction3d coordinates
tangentDirection nondegenerateSpline parameterValue =
    case nondegenerateSpline of
        NonZeroFirstDerivative spline firstDerivativeDirection ->
            -- Tangent direction is always equal to the (constant) first
            -- derivative direction
            firstDerivativeDirection

        NonZeroSecondDerivative spline secondDerivativeDirection ->
            let
                firstDerivativeVector =
                    firstDerivative spline parameterValue
            in
            case Vector3d.direction firstDerivativeVector of
                Just firstDerivativeDirection ->
                    -- First derivative is non-zero, so use its direction as the
                    -- tangent direction (normal case)
                    firstDerivativeDirection

                Nothing ->
                    -- Zero first derivative and non-zero second derivative mean
                    -- we have reached a reversal point, where the tangent
                    -- direction just afterwards is equal to the second
                    -- derivative direction and the tangent direction just
                    -- before is equal to the reversed second derivative
                    -- direction. If we happen to be right at the end of the
                    -- spline, choose the tangent direction just before the end
                    -- (instead of one that is off the spline!), otherwise
                    -- choose the tangent direction just after the point
                    -- (necessary for t = 0, arbitrary for all other points).
                    if parameterValue == 1 then
                        Direction3d.reverse secondDerivativeDirection

                    else
                        secondDerivativeDirection

        NonZeroThirdDerivative spline thirdDerivativeDirection ->
            let
                firstDerivativeVector =
                    firstDerivative spline parameterValue
            in
            case Vector3d.direction firstDerivativeVector of
                Just firstDerivativeDirection ->
                    -- First derivative is non-zero, so use its direction as the
                    -- tangent direction (normal case)
                    firstDerivativeDirection

                Nothing ->
                    let
                        secondDerivativeVector =
                            secondDerivative spline parameterValue
                    in
                    case Vector3d.direction secondDerivativeVector of
                        Just secondDerivativeDirection ->
                            -- Zero first derivative and non-zero second
                            -- derivative mean we have reached a reversal point,
                            -- as above in the NonZeroSecondDerivative case
                            if parameterValue == 1 then
                                Direction3d.reverse secondDerivativeDirection

                            else
                                secondDerivativeDirection

                        Nothing ->
                            -- First and second derivatives are zero, so fall
                            -- back to the third derivative direction
                            thirdDerivativeDirection


{-| Get both the point and tangent direction of a nondegenerate spline at a
given parameter value.
-}
sample : Nondegenerate units coordinates -> Float -> ( Point3d units coordinates, Direction3d coordinates )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| Approximate a cubic spline by a given number of line segments. Note that the
number of points in the polyline will be one more than the number of segments.
-}
segments : Int -> CubicSpline3d units coordinates -> Polyline3d units coordinates
segments numSegments spline =
    Polyline3d.fromVertices (Parameter1d.steps numSegments (pointOn spline))


{-| Approximate a cubic spline as a polyline, within a given tolerance. Every
point on the returned polyline will be within the given tolerance of the spline.
-}
approximate : Quantity Float units -> CubicSpline3d units coordinates -> Polyline3d units coordinates
approximate maxError spline =
    segments (numApproximationSegments maxError spline) spline


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.
-}
reverse : CubicSpline3d units coordinates -> CubicSpline3d units coordinates
reverse spline =
    fromControlPoints
        (fourthControlPoint spline)
        (thirdControlPoint spline)
        (secondControlPoint spline)
        (firstControlPoint spline)


{-| Scale a spline about the given center point by the given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point3d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
rotateAround axis angle spline =
    mapControlPoints (Point3d.rotateAround axis angle) spline


{-| Translate a spline by a given displacement.
-}
translateBy : Vector3d units coordinates -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
translateBy displacement spline =
    mapControlPoints (Point3d.translateBy displacement) spline


{-| Translate a spline in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across a plane.
-}
mirrorAcross : Plane3d units coordinates -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
mirrorAcross plane spline =
    mapControlPoints (Point3d.mirrorAcross plane) spline


{-| Find the orthographic projection of a spline onto a plane.
-}
projectOnto : Plane3d units coordinates -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
projectOnto plane spline =
    mapControlPoints (Point3d.projectOnto plane) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> CubicSpline3d units globalCoordinates -> CubicSpline3d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point3d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> CubicSpline3d units localCoordinates -> CubicSpline3d units globalCoordinates
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| Project a spline into a given sketch plane.
-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> CubicSpline3d units coordinates3d -> CubicSpline2d units coordinates2d
projectInto sketchPlane spline =
    CubicSpline2d.fromControlPoints
        (Point3d.projectInto sketchPlane (firstControlPoint spline))
        (Point3d.projectInto sketchPlane (secondControlPoint spline))
        (Point3d.projectInto sketchPlane (thirdControlPoint spline))
        (Point3d.projectInto sketchPlane (fourthControlPoint spline))


mapControlPoints : (Point3d units1 coordinates1 -> Point3d units2 coordinates2) -> CubicSpline3d units1 coordinates1 -> CubicSpline3d units2 coordinates2
mapControlPoints function spline =
    fromControlPoints
        (function (firstControlPoint spline))
        (function (secondControlPoint spline))
        (function (thirdControlPoint spline))
        (function (fourthControlPoint spline))


{-| Split a spline into two roughly equal halves. Equivalent to
`CubicSpline3d.splitAt 0.5`.
-}
bisect : CubicSpline3d units coordinates -> ( CubicSpline3d units coordinates, CubicSpline3d units coordinates )
bisect spline =
    splitAt 0.5 spline


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.
-}
splitAt : Float -> CubicSpline3d units coordinates -> ( CubicSpline3d units coordinates, CubicSpline3d units coordinates )
splitAt parameterValue spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 parameterValue

        q2 =
            Point3d.interpolateFrom p2 p3 parameterValue

        q3 =
            Point3d.interpolateFrom p3 p4 parameterValue

        r1 =
            Point3d.interpolateFrom q1 q2 parameterValue

        r2 =
            Point3d.interpolateFrom q2 q3 parameterValue

        s =
            Point3d.interpolateFrom r1 r2 parameterValue
    in
    ( fromControlPoints p1 q1 r1 s
    , fromControlPoints s r2 q3 p4
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized units coordinates
    = ArcLengthParameterized
        { underlyingSpline : CubicSpline3d units coordinates
        , parameterization : ArcLengthParameterization units
        , nondegenerateSpline : Nondegenerate units coordinates
        }


{-| Build an arc length parameterization of the given spline, with a given
accuracy.
-}
arcLengthParameterized : { maxError : Quantity Float units } -> Nondegenerate units coordinates -> ArcLengthParameterized units coordinates
arcLengthParameterized { maxError } nondegenerateSpline =
    let
        spline =
            fromNondegenerate nondegenerateSpline

        parameterization =
            ArcLengthParameterization.build
                { maxError = maxError
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude =
                    maxSecondDerivativeMagnitude spline
                }
    in
    ArcLengthParameterized
        { underlyingSpline = spline
        , parameterization = parameterization
        , nondegenerateSpline = nondegenerateSpline
        }


{-| Find the total arc length of a spline, to within the accuracy specified
when constructing the given arc length parameterization.
-}
arcLength : ArcLengthParameterized units coordinates -> Quantity Float units
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| Get the midpoint of a spline. Note that this is the point half way along the
spline by arc length, which is not in general the same as evaluating at a
parameter value of 0.5.
-}
midpoint : ArcLengthParameterized units coordinates -> Point3d units coordinates
midpoint parameterized =
    let
        halfArcLength =
            Quantity.multiplyBy 0.5 (arcLength parameterized)
    in
    pointAlong parameterized halfArcLength


{-| Get the point along a spline at a given arc length.
-}
pointAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Point3d units coordinates
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> pointOn parameterized.underlyingSpline


{-| Get the tangent direction along a spline at a given arc length.
-}
tangentDirectionAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Direction3d coordinates
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> tangentDirection parameterized.nondegenerateSpline


{-| Get the point and tangent direction along a spline at a given arc
length.
-}
sampleAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> ( Point3d units coordinates, Direction3d coordinates )
sampleAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> sample parameterized.nondegenerateSpline


{-| -}
arcLengthParameterization : ArcLengthParameterized units coordinates -> ArcLengthParameterization units
arcLengthParameterization (ArcLengthParameterized parameterized) =
    parameterized.parameterization


{-| -}
fromArcLengthParameterized : ArcLengthParameterized units coordinates -> CubicSpline3d units coordinates
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingSpline


{-| Get the first derivative of a spline at a given parameter value.
-}
firstDerivative : CubicSpline3d units coordinates -> Float -> Vector3d units coordinates
firstDerivative spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        x1 =
            Point3d.xCoordinate p1

        y1 =
            Point3d.yCoordinate p1

        z1 =
            Point3d.zCoordinate p1

        x2 =
            Point3d.xCoordinate p2

        y2 =
            Point3d.yCoordinate p2

        z2 =
            Point3d.zCoordinate p2

        x3 =
            Point3d.xCoordinate p3

        y3 =
            Point3d.yCoordinate p3

        z3 =
            Point3d.zCoordinate p3

        x4 =
            Point3d.xCoordinate p4

        y4 =
            Point3d.yCoordinate p4

        z4 =
            Point3d.zCoordinate p4

        vx1 =
            x2 |> Quantity.minus x1

        vy1 =
            y2 |> Quantity.minus y1

        vz1 =
            z2 |> Quantity.minus z1

        vx2 =
            x3 |> Quantity.minus x2

        vy2 =
            y3 |> Quantity.minus y2

        vz2 =
            z3 |> Quantity.minus z2

        vx3 =
            x4 |> Quantity.minus x3

        vy3 =
            y4 |> Quantity.minus y3

        vz3 =
            z4 |> Quantity.minus z3

        wx1 =
            Quantity.interpolateFrom vx1 vx2 parameterValue

        wy1 =
            Quantity.interpolateFrom vy1 vy2 parameterValue

        wz1 =
            Quantity.interpolateFrom vz1 vz2 parameterValue

        wx2 =
            Quantity.interpolateFrom vx2 vx3 parameterValue

        wy2 =
            Quantity.interpolateFrom vy2 vy3 parameterValue

        wz2 =
            Quantity.interpolateFrom vz2 vz3 parameterValue
    in
    Vector3d.xyz
        (Quantity.multiplyBy 3
            (Quantity.interpolateFrom wx1 wx2 parameterValue)
        )
        (Quantity.multiplyBy 3
            (Quantity.interpolateFrom wy1 wy2 parameterValue)
        )
        (Quantity.multiplyBy 3
            (Quantity.interpolateFrom wz1 wz2 parameterValue)
        )


{-| Get the second derivative value at a point along a spline.
-}
secondDerivative : CubicSpline3d units coordinates -> Float -> Vector3d units coordinates
secondDerivative spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        u1 =
            Vector3d.from p1 p2

        u2 =
            Vector3d.from p2 p3

        u3 =
            Vector3d.from p3 p4

        v1 =
            u2 |> Vector3d.minus u1

        v2 =
            u3 |> Vector3d.minus u2
    in
    Vector3d.scaleBy 6 (Vector3d.interpolateFrom v1 v2 parameterValue)


{-| Get the third derivative of a spline (for a cubic spline, this is a
constant).
-}
thirdDerivative : CubicSpline3d units coordinates -> Vector3d units coordinates
thirdDerivative spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        u1 =
            Vector3d.from p1 p2

        u2 =
            Vector3d.from p2 p3

        u3 =
            Vector3d.from p3 p4

        v1 =
            u2 |> Vector3d.minus u1

        v2 =
            u3 |> Vector3d.minus u2
    in
    Vector3d.scaleBy 6 (v2 |> Vector3d.minus v1)


{-| Find a conservative upper bound on the magnitude of the second derivative of
a spline. This can be useful when determining error bounds for various kinds of
linear approximations.
-}
maxSecondDerivativeMagnitude : CubicSpline3d units coordinates -> Quantity Float units
maxSecondDerivativeMagnitude spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        u1 =
            Vector3d.from p1 p2

        u2 =
            Vector3d.from p2 p3

        u3 =
            Vector3d.from p3 p4

        v1 =
            u2 |> Vector3d.minus u1

        v2 =
            u3 |> Vector3d.minus u2
    in
    Quantity.multiplyBy 6 <|
        Quantity.max (Vector3d.length v1) (Vector3d.length v2)


derivativeMagnitude : CubicSpline3d units coordinates -> Float -> Quantity Float units
derivativeMagnitude (Types.CubicSpline3d spline) =
    let
        (Types.Point3d p1) =
            spline.firstControlPoint

        (Types.Point3d p2) =
            spline.secondControlPoint

        (Types.Point3d p3) =
            spline.thirdControlPoint

        (Types.Point3d p4) =
            spline.fourthControlPoint

        x1 =
            p1.x

        y1 =
            p1.y

        z1 =
            p1.z

        x2 =
            p2.x

        y2 =
            p2.y

        z2 =
            p2.z

        x3 =
            p3.x

        y3 =
            p3.y

        z3 =
            p3.z

        x4 =
            p4.x

        y4 =
            p4.y

        z4 =
            p4.z

        x12 =
            x2 - x1

        y12 =
            y2 - y1

        z12 =
            z2 - z1

        x23 =
            x3 - x2

        y23 =
            y3 - y2

        z23 =
            z3 - z2

        x34 =
            x4 - x3

        y34 =
            y4 - y3

        z34 =
            z4 - z3

        x123 =
            x23 - x12

        y123 =
            y23 - y12

        z123 =
            z23 - z12

        x234 =
            x34 - x23

        y234 =
            y34 - y23

        z234 =
            z34 - z23
    in
    \parameterValue ->
        let
            x13 =
                x12 + parameterValue * x123

            y13 =
                y12 + parameterValue * y123

            z13 =
                z12 + parameterValue * z123

            x24 =
                x23 + parameterValue * x234

            y24 =
                y23 + parameterValue * y234

            z24 =
                z23 + parameterValue * z234

            x14 =
                x13 + parameterValue * (x24 - x13)

            y14 =
                y13 + parameterValue * (y24 - y13)

            z14 =
                z13 + parameterValue * (z24 - z13)
        in
        Quantity (3 * sqrt (x14 * x14 + y14 * y14 + z14 * z14))


{-| Determine the number of linear segments needed to approximate a cubic
spline to within a given tolerance.
-}
numApproximationSegments : Quantity Float units -> CubicSpline3d units coordinats -> Int
numApproximationSegments maxError spline =
    Curve.numApproximationSegments
        { maxError = maxError
        , maxSecondDerivativeMagnitude = maxSecondDerivativeMagnitude spline
        }
