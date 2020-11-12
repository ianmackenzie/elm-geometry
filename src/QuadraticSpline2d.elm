--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module QuadraticSpline2d exposing
    ( QuadraticSpline2d
    , fromControlPoints
    , bSplineSegments, bSplineIntervals
    , startPoint, firstControlPoint, secondControlPoint, thirdControlPoint, endPoint, startDerivative, endDerivative, boundingBox
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , segments, approximate
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    , bisect, splitAt
    , ArcLengthParameterized, arcLengthParameterized, arcLength
    , pointAlong, midpoint, tangentDirectionAlong, sampleAlong
    , arcLengthParameterization, fromArcLengthParameterized
    , firstDerivative, secondDerivative, numApproximationSegments
    )

{-| A `QuadraticSpline2d` is a quadratic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by a start point, control point and end point. This module
contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs QuadraticSpline2d


# Constructors

@docs fromControlPoints


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

These transformations generally behave just like [the ones in the `Point2d`
module](Point2d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


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
underlying `QuadraticSpline2d`. If you need to do something fancy, you can
extract these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Advanced

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, secondDerivative, numApproximationSegments

-}

import Angle exposing (Angle)
import ArcLengthParameterization exposing (ArcLengthParameterization)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Curve
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Interval exposing (Interval)
import Parameter1d
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity(..), Rate)
import Vector2d exposing (Vector2d)


{-| -}
type alias QuadraticSpline2d units coordinates =
    Types.QuadraticSpline2d units coordinates


{-| Construct a spline from its start point, inner control point and end point:

    exampleSpline =
        QuadraticSpline2d.fromControlPoints
            (Point2d.meters 1 1)
            (Point2d.meters 3 4)
            (Point2d.meters 5 1)

-}
fromControlPoints : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> QuadraticSpline2d units coordinates
fromControlPoints p1 p2 p3 =
    Types.QuadraticSpline2d
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
bSplineSegments : List Float -> List (Point2d units coordinates) -> List (QuadraticSpline2d units coordinates)
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
    -> Point2d units coordinates
    -> Point2d units coordinates
    -> List (Point2d units coordinates)
    -> List (QuadraticSpline2d units coordinates)
    -> List (QuadraticSpline2d units coordinates)
bSplineSegmentsHelp u0 u1 u2 u3s b01 b12 b23s accumulated =
    case ( u3s, b23s ) of
        ( u3 :: us, b23 :: bs ) ->
            if u1 == u2 then
                bSplineSegmentsHelp u1 u2 u3 us b12 b23 bs accumulated

            else
                let
                    b11 =
                        Point2d.interpolateFrom b01 b12 ((u1 - u0) / (u2 - u0))

                    b22 =
                        Point2d.interpolateFrom b12 b23 ((u2 - u1) / (u3 - u1))

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


{-| Convert a spline from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> QuadraticSpline2d units1 coordinates -> QuadraticSpline2d units2 coordinates
at rate (Types.QuadraticSpline2d spline) =
    Types.QuadraticSpline2d
        { firstControlPoint = Point2d.at rate spline.firstControlPoint
        , secondControlPoint = Point2d.at rate spline.secondControlPoint
        , thirdControlPoint = Point2d.at rate spline.thirdControlPoint
        }


{-| Convert a spline from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> QuadraticSpline2d units1 coordinates -> QuadraticSpline2d units2 coordinates
at_ rate spline =
    at (Quantity.inverse rate) spline


{-| Get the start point of a spline. Equal to [`firstControlPoint`](#firstControlPoint).
-}
startPoint : QuadraticSpline2d units coordinates -> Point2d units coordinates
startPoint (Types.QuadraticSpline2d spline) =
    spline.firstControlPoint


{-| Get the end point of a spline. Equal to [`thirdControlPoint`](#thirdControlPoint).
-}
endPoint : QuadraticSpline2d units coordinates -> Point2d units coordinates
endPoint (Types.QuadraticSpline2d spline) =
    spline.thirdControlPoint


{-| Get the first control point of a spline. Equal to [`startPoint`](#startPoint).
-}
firstControlPoint : QuadraticSpline2d units coordinates -> Point2d units coordinates
firstControlPoint (Types.QuadraticSpline2d spline) =
    spline.firstControlPoint


{-| Get the second control point of a spline.
-}
secondControlPoint : QuadraticSpline2d units coordinates -> Point2d units coordinates
secondControlPoint (Types.QuadraticSpline2d spline) =
    spline.secondControlPoint


{-| Get the third and last control point of a spline. Equal to [`endPoint`](#endPoint).
-}
thirdControlPoint : QuadraticSpline2d units coordinates -> Point2d units coordinates
thirdControlPoint (Types.QuadraticSpline2d spline) =
    spline.thirdControlPoint


{-| Get the start derivative of a spline. This is equal to twice the vector from
the spline's first control point to its second.

    QuadraticSpline2d.startDerivative exampleSpline
    --> Vector2d.meters 4 6

-}
startDerivative : QuadraticSpline2d units coordinates -> Vector2d units coordinates
startDerivative spline =
    Vector2d.twice <|
        Vector2d.from (firstControlPoint spline) (secondControlPoint spline)


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline2d.endDerivative exampleSpline
    --> Vector2d.meters 4 -6

-}
endDerivative : QuadraticSpline2d units coordinates -> Vector2d units coordinates
endDerivative spline =
    Vector2d.twice <|
        Vector2d.from (secondControlPoint spline) (thirdControlPoint spline)


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
area than the spline itself).

    QuadraticSpline2d.boundingBox exampleSpline
    --> BoundingBox2d.from
    -->     (Point2d.meters 1 1)
    -->     (Point2d.meters 5 4)

-}
boundingBox : QuadraticSpline2d units coordinates -> BoundingBox2d units coordinates
boundingBox spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        x1 =
            Point2d.xCoordinate p1

        y1 =
            Point2d.yCoordinate p1

        x2 =
            Point2d.xCoordinate p2

        y2 =
            Point2d.yCoordinate p2

        x3 =
            Point2d.xCoordinate p3

        y3 =
            Point2d.yCoordinate p3
    in
    BoundingBox2d.fromExtrema
        { minX = Quantity.min x1 (Quantity.min x2 x3)
        , maxX = Quantity.max x1 (Quantity.max x2 x3)
        , minY = Quantity.min y1 (Quantity.min y2 y3)
        , maxY = Quantity.max y1 (Quantity.max y2 y3)
        }


{-| Get the point along a spline at a given parameter value.
-}
pointOn : QuadraticSpline2d units coordinates -> Float -> Point2d units coordinates
pointOn spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        q1 =
            Point2d.interpolateFrom p1 p2 parameterValue

        q2 =
            Point2d.interpolateFrom p2 p3 parameterValue
    in
    Point2d.interpolateFrom q1 q2 parameterValue


{-| Get the first derivative of a spline at a given parameter value. Note that
the derivative interpolates linearly from end to end.
-}
firstDerivative : QuadraticSpline2d units coordinates -> Float -> Vector2d units coordinates
firstDerivative spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        v1 =
            Vector2d.from p1 p2

        v2 =
            Vector2d.from p2 p3
    in
    Vector2d.twice (Vector2d.interpolateFrom v1 v2 parameterValue)


derivativeMagnitude : QuadraticSpline2d units coordinates -> Float -> Quantity Float units
derivativeMagnitude (Types.QuadraticSpline2d spline) =
    let
        (Types.Point2d p1) =
            spline.firstControlPoint

        (Types.Point2d p2) =
            spline.secondControlPoint

        (Types.Point2d p3) =
            spline.thirdControlPoint

        x1 =
            p1.x

        y1 =
            p1.y

        x2 =
            p2.x

        y2 =
            p2.y

        x3 =
            p3.x

        y3 =
            p3.y

        x12 =
            x2 - x1

        y12 =
            y2 - y1

        x23 =
            x3 - x2

        y23 =
            y3 - y2

        x123 =
            x23 - x12

        y123 =
            y23 - y12
    in
    \parameterValue ->
        let
            x13 =
                x12 + parameterValue * x123

            y13 =
                y12 + parameterValue * y123
        in
        Quantity (2 * sqrt (x13 * x13 + y13 * y13))


{-| Represents a nondegenerate spline (one that has finite, non-zero length).
-}
type Nondegenerate units coordinates
    = NonZeroSecondDerivative (QuadraticSpline2d units coordinates) (Direction2d coordinates)
    | NonZeroFirstDerivative (QuadraticSpline2d units coordinates) (Direction2d coordinates)


{-| Attempt to construct a nondegenerate spline from a general
`QuadraticSpline2d`. If the spline is in fact degenerate (consists of a single
point), returns an `Err` with that point.
-}
nondegenerate : QuadraticSpline2d units coordinates -> Result (Point2d units coordinates) (Nondegenerate units coordinates)
nondegenerate spline =
    case Vector2d.direction (secondDerivative spline) of
        Just direction ->
            Ok (NonZeroSecondDerivative spline direction)

        Nothing ->
            let
                -- Second derivative is zero, so first derivative is constant -
                -- evaluate it at an arbitrary point to get its value
                firstDerivativeVector =
                    firstDerivative spline 0
            in
            case Vector2d.direction firstDerivativeVector of
                Just direction ->
                    Ok (NonZeroFirstDerivative spline direction)

                Nothing ->
                    Err (startPoint spline)


{-| Convert a nondegenerate spline back to a general `QuadraticSpline2d`.
-}
fromNondegenerate : Nondegenerate units coordinates -> QuadraticSpline2d units coordinates
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| Get the tangent direction to a nondegenerate spline at a given parameter
value.
-}
tangentDirection : Nondegenerate units coordinates -> Float -> Direction2d coordinates
tangentDirection nondegenerateSpline parameterValue =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline secondDerivativeDirection ->
            let
                firstDerivativeVector =
                    firstDerivative spline parameterValue
            in
            case Vector2d.direction firstDerivativeVector of
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
                        Direction2d.reverse secondDerivativeDirection

                    else
                        secondDerivativeDirection

        NonZeroFirstDerivative spline firstDerivativeDirection ->
            -- Tangent direction is always equal to the (constant) first
            -- derivative direction
            firstDerivativeDirection


{-| Get both the point and tangent direction of a nondegenerate spline at a
given parameter value.
-}
sample : Nondegenerate units coordinates -> Float -> ( Point2d units coordinates, Direction2d coordinates )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| Approximate a quadratic spline by a given number of line segments. Note that
the number of points in the polyline will be one more than the number of
segments.
-}
segments : Int -> QuadraticSpline2d units coordinates -> Polyline2d units coordinates
segments numSegments spline =
    Polyline2d.fromVertices (Parameter1d.steps numSegments (pointOn spline))


{-| Approximate a quadratic spline as a polyline, within a given tolerance.
Every point on the returned polyline will be within the given tolerance of the
spline.
-}
approximate : Quantity Float units -> QuadraticSpline2d units coordinates -> Polyline2d units coordinates
approximate maxError spline =
    segments (numApproximationSegments maxError spline) spline


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.
-}
reverse : QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
reverse spline =
    fromControlPoints
        (thirdControlPoint spline)
        (secondControlPoint spline)
        (firstControlPoint spline)


{-| Scale a spline about the given center point by the given scale.
-}
scaleAbout : Point2d units coordinates -> Float -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point2d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given center point by a given
angle.
-}
rotateAround : Point2d units coordinates -> Angle -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
rotateAround point angle spline =
    mapControlPoints (Point2d.rotateAround point angle) spline


{-| Translate a spline by a given displacement.
-}
translateBy : Vector2d units coordinates -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
translateBy displacement spline =
    mapControlPoints (Point2d.translateBy displacement) spline


{-| Translate a spline in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
translateIn direction distance spline =
    translateBy (Vector2d.withLength distance direction) spline


{-| Mirror a spline across an axis.
-}
mirrorAcross : Axis2d units coordinates -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
mirrorAcross axis spline =
    mapControlPoints (Point2d.mirrorAcross axis) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> QuadraticSpline2d units globalCoordinates -> QuadraticSpline2d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point2d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> QuadraticSpline2d units localCoordinates -> QuadraticSpline2d units globalCoordinates
placeIn frame spline =
    mapControlPoints (Point2d.placeIn frame) spline


mapControlPoints : (Point2d units1 coordinates1 -> Point2d units2 coordinates2) -> QuadraticSpline2d units1 coordinates1 -> QuadraticSpline2d units2 coordinates2
mapControlPoints function spline =
    fromControlPoints
        (function (firstControlPoint spline))
        (function (secondControlPoint spline))
        (function (thirdControlPoint spline))


{-| Split a spline into two roughly equal halves. Equivalent to
`QuadraticSpline2d.splitAt 0.5`.
-}
bisect : QuadraticSpline2d units coordinates -> ( QuadraticSpline2d units coordinates, QuadraticSpline2d units coordinates )
bisect spline =
    splitAt 0.5 spline


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.
-}
splitAt : Float -> QuadraticSpline2d units coordinates -> ( QuadraticSpline2d units coordinates, QuadraticSpline2d units coordinates )
splitAt parameterValue spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        q1 =
            Point2d.interpolateFrom p1 p2 parameterValue

        q2 =
            Point2d.interpolateFrom p2 p3 parameterValue

        r =
            Point2d.interpolateFrom q1 q2 parameterValue
    in
    ( fromControlPoints p1 q1 r
    , fromControlPoints r q2 p3
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized units coordinates
    = ArcLengthParameterized
        { nondegenerateSpline : Nondegenerate units coordinates
        , underlyingSpline : QuadraticSpline2d units coordinates
        , parameterization : ArcLengthParameterization units
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
                , maxSecondDerivativeMagnitude = Vector2d.length (secondDerivative spline)
                }
    in
    ArcLengthParameterized
        { nondegenerateSpline = nondegenerateSpline
        , underlyingSpline = spline
        , parameterization = parameterization
        }


{-| Find the total arc length of a spline, to within the accuracy specified
when calling `arcLengthParameterized`.
-}
arcLength : ArcLengthParameterized units coordinates -> Quantity Float units
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| Get the point along a spline at a given arc length.
-}
pointAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Point2d units coordinates
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> pointOn parameterized.underlyingSpline


{-| Get the midpoint of a spline. Note that this is the point half way along the
spline by arc length, which is not in general the same as evaluating at a
parameter value of 0.5.
-}
midpoint : ArcLengthParameterized units coordinates -> Point2d units coordinates
midpoint parameterized =
    let
        halfArcLength =
            Quantity.multiplyBy 0.5 (arcLength parameterized)
    in
    pointAlong parameterized halfArcLength


{-| Get the tangent direction along a spline at a given arc length.
-}
tangentDirectionAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Direction2d coordinates
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    let
        parameterValue =
            ArcLengthParameterization.arcLengthToParameterValue
                distance
                parameterized.parameterization
    in
    tangentDirection parameterized.nondegenerateSpline parameterValue


{-| Get the point and tangent direction along a spline at a given arc length.
-}
sampleAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> ( Point2d units coordinates, Direction2d coordinates )
sampleAlong (ArcLengthParameterized parameterized) distance =
    let
        parameterValue =
            ArcLengthParameterization.arcLengthToParameterValue
                distance
                parameterized.parameterization
    in
    sample parameterized.nondegenerateSpline parameterValue


{-| -}
arcLengthParameterization : ArcLengthParameterized units coordinates -> ArcLengthParameterization units
arcLengthParameterization (ArcLengthParameterized parameterized) =
    parameterized.parameterization


{-| Get the original `QuadraticSpline2d` from which an `ArcLengthParameterized`
value was constructed.
-}
fromArcLengthParameterized : ArcLengthParameterized units coordinates -> QuadraticSpline2d units coordinates
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingSpline


{-| Get the second derivative of a spline (for a quadratic spline, this is a
constant).
-}
secondDerivative : QuadraticSpline2d units coordinates -> Vector2d units coordinates
secondDerivative spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        v1 =
            Vector2d.from p1 p2

        v2 =
            Vector2d.from p2 p3
    in
    Vector2d.twice (v2 |> Vector2d.minus v1)


{-| Determine the number of linear segments needed to approximate a quadratic
spline to within a given tolerance.
-}
numApproximationSegments : Quantity Float units -> QuadraticSpline2d units coordinats -> Int
numApproximationSegments maxError spline =
    Curve.numApproximationSegments
        { maxError = maxError
        , maxSecondDerivativeMagnitude = Vector2d.length (secondDerivative spline)
        }
