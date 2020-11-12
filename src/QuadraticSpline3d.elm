--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module QuadraticSpline3d exposing
    ( QuadraticSpline3d
    , fromControlPoints, on
    , bSplineSegments, bSplineIntervals
    , startPoint, firstControlPoint, secondControlPoint, thirdControlPoint, endPoint, startDerivative, endDerivative, boundingBox
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , segments, approximate
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto
    , at, at_
    , relativeTo, placeIn
    , projectInto
    , bisect, splitAt
    , ArcLengthParameterized, arcLengthParameterized, arcLength
    , pointAlong, midpoint, tangentDirectionAlong, sampleAlong
    , arcLengthParameterization, fromArcLengthParameterized
    , firstDerivative, secondDerivative, numApproximationSegments
    )

{-| A `QuadraticSpline3d` is a quadratic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 3D defined by a start point, control point and end point. This module
contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs QuadraticSpline3d


# Constructors

@docs fromControlPoints, on


## B-splines

@docs bSplineSegments, bSplineIntervals


# Properties

@docs startPoint, firstControlPoint, secondControlPoint, thirdControlPoint, endPoint, startDerivative, endDerivative, boundingBox


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

@docs relativeTo, placeIn


# Sketch planes

@docs projectInto


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength

For the following evaluation functions, the given arc length will be clamped to
the arc length of the spline, so the result will always be on the spline.

@docs pointAlong, midpoint, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `QuadraticSpline3d`. If you need to do something fancy, you can
extract these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Advanced

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, secondDerivative, numApproximationSegments

-}

import Angle exposing (Angle)
import ArcLengthParameterization exposing (ArcLengthParameterization)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Curve
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Interval exposing (Interval)
import Parameter1d
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity(..), Rate)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias QuadraticSpline3d units coordinates =
    Types.QuadraticSpline3d units coordinates


{-| Construct a spline from its start point, inner control point and end point:

    exampleSpline =
        QuadraticSpline3d.fromControlPoints
            (Point3d.meters 1 1 1)
            (Point3d.meters 3 2 1)
            (Point3d.meters 3 3 3)

-}
fromControlPoints : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> QuadraticSpline3d units coordinates
fromControlPoints p1 p2 p3 =
    Types.QuadraticSpline3d
        { firstControlPoint = p1
        , secondControlPoint = p2
        , thirdControlPoint = p3
        }


{-| Construct a [B-spline](https://mathworld.wolfram.com/B-Spline.html) from a
list of knot values and a list of control points, and return the individual
segments of that B-spline as a list.

The number of knots should be one greater than the number of control points; any
extra knots or control points will be dropped. In most cases the first and last
knots will be repeated two times; for example, the knots

    [ 0, 0, 1, 2, 3, 4, 4 ]

could be used along with 6 control points to form 4 spline segments.

Note that a popular alternate convention uses two extra 'dummy' knot values at
the start and end, so if you see an example where the number of knots is _three_
greater than the number of control points (especially if you also notice that
the first and last knots are repeated three times instead of two!) then you
should drop the first and last knot values.

Knot values should be given in ascending order but will be sorted if necessary.

-}
bSplineSegments : List Float -> List (Point3d units coordinates) -> List (QuadraticSpline3d units coordinates)
bSplineSegments givenKnots givenControlPoints =
    case ( List.sort givenKnots, givenControlPoints ) of
        ( u0 :: u1 :: u2 :: us, b01 :: b12 :: bs ) ->
            bSplineSegmentsHelp u0 u1 u2 us b01 b12 bs []

        _ ->
            []


bSplineSegmentsHelp :
    Float
    -> Float
    -> Float
    -> List Float
    -> Point3d units coordinates
    -> Point3d units coordinates
    -> List (Point3d units coordinates)
    -> List (QuadraticSpline3d units coordinates)
    -> List (QuadraticSpline3d units coordinates)
bSplineSegmentsHelp u0 u1 u2 u3s b01 b12 b23s accumulated =
    case ( u3s, b23s ) of
        ( u3 :: us, b23 :: bs ) ->
            if u1 == u2 then
                bSplineSegmentsHelp u1 u2 u3 us b12 b23 bs accumulated

            else
                let
                    b11 =
                        Point3d.interpolateFrom b01 b12 ((u1 - u0) / (u2 - u0))

                    b22 =
                        Point3d.interpolateFrom b12 b23 ((u2 - u1) / (u3 - u1))

                    segment =
                        fromControlPoints b11 b12 b22
                in
                bSplineSegmentsHelp u1 u2 u3 us b12 b23 bs (segment :: accumulated)

        _ ->
            List.reverse accumulated


{-| For a given set of B-spline knots, return the corresponding intervals
between knots that correspond to individual spline [segments](#bSplineSegments).
-}
bSplineIntervals : List Float -> List (Interval Float)
bSplineIntervals givenKnots =
    case List.sort givenKnots of
        _ :: u1 :: u2 :: u3s ->
            bSplineIntervalsHelp u1 u2 u3s []

        _ ->
            []


bSplineIntervalsHelp :
    Float
    -> Float
    -> List Float
    -> List (Interval Float)
    -> List (Interval Float)
bSplineIntervalsHelp u1 u2 u3s accumulated =
    case u3s of
        u3 :: u4s ->
            if u1 == u2 then
                bSplineIntervalsHelp u2 u3 u4s accumulated

            else
                bSplineIntervalsHelp u2 u3 u4s (Interval.from u1 u2 :: accumulated)

        [] ->
            List.reverse accumulated


{-| Construct a 3D spline lying _on_ a sketch plane by providing a 2D spline
specified in XY coordinates _within_ the sketch plane.

    QuadraticSpline3d.on SketchPlane3d.xz <|
        QuadraticSpline2d.fromControlPoints
            (Point2d.meters 1 1)
            (Point2d.meters 3 4)
            (Point2d.meters 5 1)
    --> QuadraticSpline3d.fromControlPoints
    -->     (Point3d.meters 1 0 1)
    -->     (Point3d.meters 3 0 4)
    -->     (Point3d.meters 5 0 1)

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> QuadraticSpline2d units coordinates2d -> QuadraticSpline3d units coordinates3d
on sketchPlane spline2d =
    fromControlPoints
        (Point3d.on sketchPlane (QuadraticSpline2d.firstControlPoint spline2d))
        (Point3d.on sketchPlane (QuadraticSpline2d.secondControlPoint spline2d))
        (Point3d.on sketchPlane (QuadraticSpline2d.thirdControlPoint spline2d))


{-| Convert a spline from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> QuadraticSpline3d units1 coordinates -> QuadraticSpline3d units2 coordinates
at rate (Types.QuadraticSpline3d spline) =
    Types.QuadraticSpline3d
        { firstControlPoint = Point3d.at rate spline.firstControlPoint
        , secondControlPoint = Point3d.at rate spline.secondControlPoint
        , thirdControlPoint = Point3d.at rate spline.thirdControlPoint
        }


{-| Convert a spline from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> QuadraticSpline3d units1 coordinates -> QuadraticSpline3d units2 coordinates
at_ rate spline =
    at (Quantity.inverse rate) spline


{-| Get the start point of a spline. Equal to [`firstControlPoint`](#firstControlPoint).
-}
startPoint : QuadraticSpline3d units coordinates -> Point3d units coordinates
startPoint (Types.QuadraticSpline3d spline) =
    spline.firstControlPoint


{-| Get the end point of a spline. Equal to [`thirdControlPoint`](#firstControlPoint).
-}
endPoint : QuadraticSpline3d units coordinates -> Point3d units coordinates
endPoint (Types.QuadraticSpline3d spline) =
    spline.thirdControlPoint


{-| Get the first control point of a spline. Equal to [`startPoint`](#startPoint).
-}
firstControlPoint : QuadraticSpline3d units coordinates -> Point3d units coordinates
firstControlPoint (Types.QuadraticSpline3d spline) =
    spline.firstControlPoint


{-| Get the second control point of a spline.
-}
secondControlPoint : QuadraticSpline3d units coordinates -> Point3d units coordinates
secondControlPoint (Types.QuadraticSpline3d spline) =
    spline.secondControlPoint


{-| Get the third and last control point of a spline. Equal to [`endPoint`](#endPoint).
-}
thirdControlPoint : QuadraticSpline3d units coordinates -> Point3d units coordinates
thirdControlPoint (Types.QuadraticSpline3d spline) =
    spline.thirdControlPoint


{-| Get the start derivative of a spline. This is equal to twice the vector from
the spline's first control point to its second.

    QuadraticSpline3d.startDerivative exampleSpline
    --> Vector3d.meters 4 2 0

-}
startDerivative : QuadraticSpline3d units coordinates -> Vector3d units coordinates
startDerivative spline =
    Vector3d.twice <|
        Vector3d.from (firstControlPoint spline) (secondControlPoint spline)


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline3d.endDerivative exampleSpline
    --> Vector3d.meters 0 2 4

-}
endDerivative : QuadraticSpline3d units coordinates -> Vector3d units coordinates
endDerivative spline =
    Vector3d.twice <|
        Vector3d.from (secondControlPoint spline) (thirdControlPoint spline)


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
volume than the spline itself).

    QuadraticSpline3d.boundingBox exampleSpline
    --> BoundingBox3d.from
    -->     (Point3d.meters 1 1 1)
    -->     (Point3d.meters 3 3 3)

-}
boundingBox : QuadraticSpline3d units coordinates -> BoundingBox3d units coordinates
boundingBox spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

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
    in
    BoundingBox3d.fromExtrema
        { minX = Quantity.min x1 (Quantity.min x2 x3)
        , maxX = Quantity.max x1 (Quantity.max x2 x3)
        , minY = Quantity.min y1 (Quantity.min y2 y3)
        , maxY = Quantity.max y1 (Quantity.max y2 y3)
        , minZ = Quantity.min z1 (Quantity.min z2 z3)
        , maxZ = Quantity.max z1 (Quantity.max z2 z3)
        }


{-| Get the point along a spline at a given parameter value.
-}
pointOn : QuadraticSpline3d units coordinates -> Float -> Point3d units coordinates
pointOn spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 parameterValue

        q2 =
            Point3d.interpolateFrom p2 p3 parameterValue
    in
    Point3d.interpolateFrom q1 q2 parameterValue


{-| Get the first derivative of a spline at a given parameter value. Note that
the derivative interpolates linearly from end to end.
-}
firstDerivative : QuadraticSpline3d units coordinates -> Float -> Vector3d units coordinates
firstDerivative spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.twice (Vector3d.interpolateFrom v1 v2 parameterValue)


derivativeMagnitude : QuadraticSpline3d units coordinates -> Float -> Quantity Float units
derivativeMagnitude (Types.QuadraticSpline3d spline) =
    let
        (Types.Point3d p1) =
            spline.firstControlPoint

        (Types.Point3d p2) =
            spline.secondControlPoint

        (Types.Point3d p3) =
            spline.thirdControlPoint

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

        x123 =
            x23 - x12

        y123 =
            y23 - y12

        z123 =
            z23 - z12
    in
    \parameterValue ->
        let
            x13 =
                x12 + parameterValue * x123

            y13 =
                y12 + parameterValue * y123

            z13 =
                z12 + parameterValue * z123
        in
        Quantity (2 * sqrt (x13 * x13 + y13 * y13 + z13 * z13))


{-| Represents a nondegenerate spline (one that has finite, non-zero length).
-}
type Nondegenerate units coordinates
    = NonZeroSecondDerivative (QuadraticSpline3d units coordinates) (Direction3d coordinates)
    | NonZeroFirstDerivative (QuadraticSpline3d units coordinates) (Direction3d coordinates)


{-| Attempt to construct a nondegenerate spline from a general
`QuadraticSpline3d`. If the spline is in fact degenerate (consists of a single
point), returns an `Err` with that point.
-}
nondegenerate : QuadraticSpline3d units coordinates -> Result (Point3d units coordinates) (Nondegenerate units coordinates)
nondegenerate spline =
    case Vector3d.direction (secondDerivative spline) of
        Just direction ->
            Ok (NonZeroSecondDerivative spline direction)

        Nothing ->
            let
                -- Second derivative is zero, so first derivative is constant -
                -- evaluate it at an arbitrary point to get its value
                firstDerivativeVector =
                    firstDerivative spline 0
            in
            case Vector3d.direction firstDerivativeVector of
                Just direction ->
                    Ok (NonZeroFirstDerivative spline direction)

                Nothing ->
                    Err (startPoint spline)


{-| Convert a nondegenerate spline back to a general `QuadraticSpline3d`.
-}
fromNondegenerate : Nondegenerate units coordinates -> QuadraticSpline3d units coordinates
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
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
        NonZeroSecondDerivative spline secondDerivativeDirection ->
            let
                firstDerivativeVector =
                    firstDerivative spline parameterValue
            in
            case Vector3d.direction firstDerivativeVector of
                Just firstDerivativeDirection ->
                    -- First derivative is non-zero, so use its direction as the
                    -- tangent direction
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

        NonZeroFirstDerivative spline firstDerivativeDirection ->
            -- Tangent direction is always equal to the (constant) first
            -- derivative direction
            firstDerivativeDirection


{-| Get both the point and tangent direction of a nondegenerate spline at a
given parameter value.
-}
sample : Nondegenerate units coordinates -> Float -> ( Point3d units coordinates, Direction3d coordinates )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| Approximate a quadratic spline by a given number of line segments. Note that
the number of points in the polyline will be one more than the number of
segments.
-}
segments : Int -> QuadraticSpline3d units coordinates -> Polyline3d units coordinates
segments numSegments spline =
    Polyline3d.fromVertices (Parameter1d.steps numSegments (pointOn spline))


{-| Approximate a quadratic spline as a polyline, within a given tolerance.
Every point on the returned polyline will be within the given tolerance of the
spline.
-}
approximate : Quantity Float units -> QuadraticSpline3d units coordinates -> Polyline3d units coordinates
approximate maxError spline =
    segments (numApproximationSegments maxError spline) spline


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.
-}
reverse : QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
reverse spline =
    fromControlPoints
        (thirdControlPoint spline)
        (secondControlPoint spline)
        (firstControlPoint spline)


{-| Scale a spline about the given center point by the given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point3d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
rotateAround axis angle spline =
    mapControlPoints (Point3d.rotateAround axis angle) spline


{-| Translate a spline by a given displacement.
-}
translateBy : Vector3d units coordinates -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
translateBy displacement spline =
    mapControlPoints (Point3d.translateBy displacement) spline


{-| Translate an arc in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across a plane.
-}
mirrorAcross : Plane3d units coordinates -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
mirrorAcross plane spline =
    mapControlPoints (Point3d.mirrorAcross plane) spline


{-| Find the orthographic projection of a spline onto a plane.
-}
projectOnto : Plane3d units coordinates -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
projectOnto plane spline =
    mapControlPoints (Point3d.projectOnto plane) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> QuadraticSpline3d units globalCoordinates -> QuadraticSpline3d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point3d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> QuadraticSpline3d units localCoordinates -> QuadraticSpline3d units globalCoordinates
placeIn frame spline =
    mapControlPoints (Point3d.placeIn frame) spline


{-| Project a spline into a given sketch plane. Conceptually, finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the spline onto the plane and then expresses the projected spline in 2D
sketch coordinates.
-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> QuadraticSpline3d units coordinates3d -> QuadraticSpline2d units coordinates2d
projectInto sketchPlane spline =
    QuadraticSpline2d.fromControlPoints
        (Point3d.projectInto sketchPlane (firstControlPoint spline))
        (Point3d.projectInto sketchPlane (secondControlPoint spline))
        (Point3d.projectInto sketchPlane (thirdControlPoint spline))


mapControlPoints : (Point3d units1 coordinates1 -> Point3d units2 coordinates2) -> QuadraticSpline3d units1 coordinates1 -> QuadraticSpline3d units2 coordinates2
mapControlPoints function spline =
    fromControlPoints
        (function (firstControlPoint spline))
        (function (secondControlPoint spline))
        (function (thirdControlPoint spline))


{-| Split a spline into two roughly equal halves. Equivalent to
`QuadraticSpline3d.splitAt 0.5`.
-}
bisect : QuadraticSpline3d units coordinates -> ( QuadraticSpline3d units coordinates, QuadraticSpline3d units coordinates )
bisect spline =
    splitAt 0.5 spline


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.
-}
splitAt : Float -> QuadraticSpline3d units coordinates -> ( QuadraticSpline3d units coordinates, QuadraticSpline3d units coordinates )
splitAt parameterValue spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 parameterValue

        q2 =
            Point3d.interpolateFrom p2 p3 parameterValue

        r =
            Point3d.interpolateFrom q1 q2 parameterValue
    in
    ( fromControlPoints p1 q1 r
    , fromControlPoints r q2 p3
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized units coordinates
    = ArcLengthParameterized
        { underlyingSpline : QuadraticSpline3d units coordinates
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
                , maxSecondDerivativeMagnitude = Vector3d.length (secondDerivative spline)
                }
    in
    ArcLengthParameterized
        { underlyingSpline = spline
        , parameterization = parameterization
        , nondegenerateSpline = nondegenerateSpline
        }


{-| Find the total arc length of a spline, to within the accuracy specified
when calling [`arcLengthParameterized`](#arcLengthParameterized).
-}
arcLength : ArcLengthParameterized units coordinates -> Quantity Float units
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| Get the point along a spline at a given arc length.
-}
pointAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Point3d units coordinates
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> pointOn parameterized.underlyingSpline


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


{-| Get the tangent direction along a spline at a given arc length.
-}
tangentDirectionAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Direction3d coordinates
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> tangentDirection parameterized.nondegenerateSpline


{-| Get the point and tangent direction along a spline at a given arc length.
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


{-| Get the original `QuadraticSpline3d` from which an `ArcLengthParameterized`
value was constructed.
-}
fromArcLengthParameterized : ArcLengthParameterized units coordinates -> QuadraticSpline3d units coordinates
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingSpline


{-| Get the second derivative of a spline (for a quadratic spline, this is a
constant).
-}
secondDerivative : QuadraticSpline3d units coordinates -> Vector3d units coordinates
secondDerivative spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.twice (v2 |> Vector3d.minus v1)


{-| Determine the number of linear segments needed to approximate a quadratic
spline to within a given tolerance.
-}
numApproximationSegments : Quantity Float units -> QuadraticSpline3d units coordinats -> Int
numApproximationSegments maxError spline =
    Curve.numApproximationSegments
        { maxError = maxError
        , maxSecondDerivativeMagnitude = Vector3d.length (secondDerivative spline)
        }
