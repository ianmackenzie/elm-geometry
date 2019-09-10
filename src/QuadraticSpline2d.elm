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
    , startPoint, endPoint, firstControlPoint, secondControlPoint, thirdControlPoint, startDerivative, endDerivative, boundingBox
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , relativeTo, placeIn
    , bisect, splitAt
    , ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, midpoint, tangentDirectionAlong, sampleAlong
    , arcLengthParameterization, fromArcLengthParameterized
    , firstDerivative, secondDerivative
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


# Properties

@docs startPoint, endPoint, firstControlPoint, secondControlPoint, thirdControlPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, sample


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, midpoint, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `QuadraticSpline2d`. If you need to do something fancy, you can
extract these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, secondDerivative

-}

import Angle exposing (Angle)
import ArcLengthParameterization exposing (ArcLengthParameterization)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)


{-| -}
type alias QuadraticSpline2d units coordinates =
    Types.QuadraticSpline2d units coordinates


{-| Construct a spline from its start point, inner control point and end point:

    exampleSpline =
        QuadraticSpline2d.fromControlPoints
            (Point2d.fromTuple meters ( 1, 1 ))
            (Point2d.fromTuple meters ( 3, 4 ))
            (Point2d.fromTuple meters ( 5, 1 ))

-}
fromControlPoints : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> QuadraticSpline2d units coordinates
fromControlPoints p1 p2 p3 =
    Types.QuadraticSpline2d
        { firstControlPoint = p1
        , secondControlPoint = p2
        , thirdControlPoint = p3
        }


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
    Vector2d.from (firstControlPoint spline) (secondControlPoint spline)
        |> Vector2d.scaleBy 2


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline2d.endDerivative exampleSpline
    --> Vector2d.meters 4 -6

-}
endDerivative : QuadraticSpline2d units coordinates -> Vector2d units coordinates
endDerivative spline =
    Vector2d.from (secondControlPoint spline) (thirdControlPoint spline)
        |> Vector2d.scaleBy 2


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
area than the spline itself).

    QuadraticSpline2d.boundingBox exampleSpline
    --> BoundingBox2d.fromExtrema
    -->     { minX = Length.meters 1
    -->     , maxX = Length.meters 5
    -->     , minY = Length.meters 1
    -->     , maxY = Length.meters 4
    -->     }

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


{-| Get the point along a spline at a given parameter value:

    QuadraticSpline2d.pointOn exampleSpline 0
    --> Point2d.meters 1 1

    QuadraticSpline2d.pointOn exampleSpline 0.5
    --> Point2d.meters 3 2.5

    QuadraticSpline2d.pointOn exampleSpline 1
    --> Point2d.meters 5 1

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


{-| Get the first derivative of a spline at a given parameter value.

    QuadraticSpline2d.derivative exampleSpline 0
    --> Vector2d.meters 4 6

    QuadraticSpline2d.derivative exampleSpline 0.5
    --> Vector2d.meters 4 0

    QuadraticSpline2d.derivative exampleSpline 1
    --> Vector2d.meters 4 -6

Note that the derivative interpolates linearly from end to end.

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
    Vector2d.interpolateFrom v1 v2 parameterValue |> Vector2d.scaleBy 2


derivativeMagnitude : QuadraticSpline2d units coordinates -> Float -> Quantity Float units
derivativeMagnitude spline =
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

        x12 =
            x2 |> Quantity.minus x1

        y12 =
            y2 |> Quantity.minus y1

        x23 =
            x3 |> Quantity.minus x2

        y23 =
            y3 |> Quantity.minus y2

        x123 =
            x23 |> Quantity.minus x12

        y123 =
            y23 |> Quantity.minus y12
    in
    \parameterValue ->
        let
            x13 =
                x12 |> Quantity.plus (Quantity.multiplyBy parameterValue x123)

            y13 =
                y12 |> Quantity.plus (Quantity.multiplyBy parameterValue y123)
        in
        Quantity.multiplyBy 2
            (Quantity.sqrt
                (Quantity.squared x13 |> Quantity.plus (Quantity.squared y13))
            )


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents a spline that is definitely not degenerate.
It is used as input to functions such as `QuadraticSpline2d.tangentDirection`
and can be constructed using `QuadraticSpline2d.nondegenerate`.

-}
type Nondegenerate units coordinates
    = NonZeroSecondDerivative (QuadraticSpline2d units coordinates) (Direction2d coordinates)
    | NonZeroFirstDerivative (QuadraticSpline2d units coordinates) (Direction2d coordinates)


{-| Attempt to construct a nondegenerate spline from a general
`QuadraticSpline2d`. If the spline is in fact degenerate (consists of a single
point), returns an `Err` with that point.

    QuadraticSpline2d.nondegenerate exampleSpline
    --> Ok nondegenerateExampleSpline

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

    QuadraticSpline2d.fromNondegenerate
        nondegenerateExampleSpline
    --> exampleSpline

-}
fromNondegenerate : Nondegenerate units coordinates -> QuadraticSpline2d units coordinates
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| Get the tangent direction to a nondegenerate spline at a given parameter
value:

    QuadraticSpline2d.tangentDirection
        nondegenerateExampleSpline
        0
    --> Direction2d.degrees 56.31

    QuadraticSpline2d.tangentDirection
        nondegenerateExampleSpline
        0.5
    --> Direction2d.x

    QuadraticSpline2d.tangentDirection
        nondegenerateExampleSpline
        1
    --> Direction2d.degrees -56.31

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
given parameter value:

    QuadraticSpline2d.sample nondegenerateExampleSpline 0.5
    --> ( Point2d.meters 3 2.5
    --> , Direction2d.x
    --> )

-}
sample : Nondegenerate units coordinates -> Float -> ( Point2d units coordinates, Direction2d coordinates )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    QuadraticSpline2d.reverse exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 5 1
    -->     , controlPoint =
    -->         Point2d.meters 3 4
    -->     , endPoint =
    -->         Point2d.meters 1 1
    -->     }

-}
reverse : QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
reverse spline =
    fromControlPoints
        (thirdControlPoint spline)
        (secondControlPoint spline)
        (firstControlPoint spline)


{-| Scale a spline about the given center point by the given scale.

    examplePolyline
        |> QuadraticSpline2d.scaleAbout Point2d.origin 2
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 2 2
    -->     , controlPoint =
    -->         Point2d.meters 6 8
    -->     , endPoint =
    -->         Point2d.meters 10 2
    -->     }

-}
scaleAbout : Point2d units coordinates -> Float -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point2d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given center point by a given
angle.

    examplePolyline
        |> QuadraticSpline2d.rotateAround Point2d.origin
            (Angle.degrees 90)
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters -1 1
    -->     , controlPoint =
    -->         Point2d.meters -4 3
    -->     , endPoint =
    -->         Point2d.meters -1 5
    -->     }

-}
rotateAround : Point2d units coordinates -> Angle -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
rotateAround point angle spline =
    mapControlPoints (Point2d.rotateAround point angle) spline


{-| Translate a spline by a given displacement.

    displacement =
        Vector2d.meters 2 3

    exampleSpline
        |> QuadraticSpline2d.translateBy displacement
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 3 4
    -->     , controlPoint =
    -->         Point2d.meters 5 7
    -->     , endPoint =
    -->         Point2d.meters 7 4
    -->     )

-}
translateBy : Vector2d units coordinates -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
translateBy displacement spline =
    mapControlPoints (Point2d.translateBy displacement) spline


{-| Translate a spline in a given direction by a given distance;

    QuadraticSpline2d.translateIn direction distance

is equivalent to

    QuadraticSpline2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d coordinates -> Quantity Float units -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
translateIn direction distance spline =
    translateBy (Vector2d.withLength distance direction) spline


{-| Mirror a spline across an axis.

    QuadraticSpline2d.mirrorAcross Axis2d.x exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 1 -1
    -->     , controlPoint =
    -->         Point2d.meters 3 -4
    -->     , endPoint =
    -->         Point2d.meters 5 -1
    -->     }

-}
mirrorAcross : Axis2d units coordinates -> QuadraticSpline2d units coordinates -> QuadraticSpline2d units coordinates
mirrorAcross axis spline =
    mapControlPoints (Point2d.mirrorAcross axis) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atCoordinates ( 1, 2 )

    QuadraticSpline2d.relativeTo localFrame exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 0 -1
    -->     , controlPoint =
    -->         Point2d.meters 2 2
    -->     , endPoint =
    -->         Point2d.meters 4 -1
    -->     }

-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> QuadraticSpline2d units globalCoordinates -> QuadraticSpline2d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point2d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame2d.atCoordinates ( 1, 2 )

    QuadraticSpline2d.placeIn localFrame exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 2 3
    -->     , controlPoint =
    -->         Point2d.meters 4 6
    -->     , endPoint =
    -->         Point2d.meters 6 3
    -->     }

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


{-| Split a spline into two roughly equal halves.

    QuadraticSpline2d.bisect exampleSpline
    --> ( QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 1 1
    -->     , controlPoint =
    -->         Point2d.meters 2 2.5
    -->     , endPoint =
    -->         Point2d.meters 3 2.5
    -->     }
    --> , QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 3 2.5
    -->     , controlPoint =
    -->         Point2d.meters 4 2.5
    -->     , endPoint =
    -->         Point2d.meters 5 1
    -->     }
    --> )

Equivalent to `QuadraticSpline2d.splitAt 0.5`.

-}
bisect : QuadraticSpline2d units coordinates -> ( QuadraticSpline2d units coordinates, QuadraticSpline2d units coordinates )
bisect spline =
    splitAt 0.5 spline


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.

    parameterValue =
        ParameterValue.clamped 0.75

    QuadraticSpline2d.splitAt parameterValue exampleSpline
    --> ( QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 1 1
    -->     , controlPoint =
    -->         Point2d.meters 2.5 3.25
    -->     , endPoint =
    -->         Point2d.meters 4 2.125
    -->     }
    --> , QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 4 2.125
    -->     , controlPoint =
    -->         Point2d.meters 4.5 1.75
    -->     , endPoint =
    -->         Point2d.meters 5 1
    -->     }
    --> )

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
accuracy. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the specified maximum
error.

    parameterizedSpline =
        exampleSpline
            |> QuadraticSpline2d.arcLengthParameterized
                { maxError = 1.0e-4 }

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


{-| Find the total arc length of a spline:

    QuadraticSpline2d.arcLength parameterizedSpline
    --> 5.1986

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized units coordinates -> Quantity Float units
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| Try to get the point along a spline at a given arc length. For example, to
get the point a quarter of the way along `exampleSpline`:

    QuadraticSpline2d.pointAlong parameterizedSpline
        (arcLength / 4)
    --> Just (Point2d.meters 1.8350 1.9911)

Note that this is not the same as evaulating at a parameter value of 1/4:

    QuadraticSpline2d.pointOn exampleSpline 0.25
    --> Point2d.meters 2 2.125

If the given arc length is less than zero or greater than the arc length of the
spline, returns `Nothing`.

-}
pointAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Point2d units coordinates
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> pointOn parameterized.underlyingSpline


{-| Get the midpoint of a spline.

    QuadraticSpline2d.midpoint parameterizedSpline
    --> Point2d.meters 3 2.5

Note that this is the point half way along the spline by arc length, which is
not in general the same as evaluating at a parameter value of 0.5.

-}
midpoint : ArcLengthParameterized units coordinates -> Point2d units coordinates
midpoint parameterized =
    let
        halfArcLength =
            Quantity.multiplyBy 0.5 (arcLength parameterized)
    in
    pointAlong parameterized halfArcLength


{-| Try to get the tangent direction along a spline at a given arc length. To
get the tangent direction a quarter of the way along `exampleSpline`:

    QuadraticSpline2d.tangentDirectionAlong
        parameterizedSpline
        (arcLength / 4)
    --> Just (Direction2d.degrees 41.145)

If the given arc length is less than zero or greater than the arc length of the
spline (or if the derivative of the spline happens to be exactly zero at the
given arc length), returns `Nothing`.

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


{-| Try to get the point and tangent direction along a spline at a given arc
length. To get the point and tangent direction a quarter of the way along
`exampleSpline`:

    QuadraticSpline2d.sampleAlong parameterizedSpline
        (0.25 * arcLength)
    --> Just
    -->     ( Point2d.meters 1.8350 1.9911
    -->     , Direction2d.degrees 41.145
    -->     )

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), returns `Nothing`.

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
    Vector2d.scaleBy 2 (v2 |> Vector2d.minus v1)
