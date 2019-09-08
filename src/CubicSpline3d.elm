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
    , startPoint, endPoint, firstControlPoint, secondControlPoint, thirdControlPoint, fourthControlPoint, startDerivative, endDerivative, boundingBox
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto
    , relativeTo, placeIn, projectInto
    , bisect, splitAt
    , ArcLengthParameterized, arcLengthParameterized, arcLength, midpoint, pointAlong, tangentDirectionAlong, sampleAlong
    , arcLengthParameterization, fromArcLengthParameterized
    , firstDerivative, secondDerivative, thirdDerivative, maxSecondDerivativeMagnitude
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


# Properties

@docs startPoint, endPoint, firstControlPoint, secondControlPoint, thirdControlPoint, fourthControlPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, sample


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn, projectInto


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, midpoint, pointAlong, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `CubicSpline3d`. If you need to do something fancy, you can extract
these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, secondDerivative, thirdDerivative, maxSecondDerivativeMagnitude

-}

import Angle exposing (Angle)
import ArcLengthParameterization exposing (ArcLengthParameterization)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import CubicSpline2d exposing (CubicSpline2d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment3d exposing (fromEndpoints, midpoint)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias CubicSpline3d units coordinates =
    Types.CubicSpline3d units coordinates


{-| Construct a spline from its four control points:

    exampleSpline =
        CubicSpline3d.fromControlPoints
            (Point3d.fromTuple meters ( 1, 1, 1 ))
            (Point3d.fromTuple meters ( 3, 1, 1 ))
            (Point3d.fromTuple meters ( 3, 3, 1 ))
            (Point3d.fromTuple meters ( 3, 3, 3 ))

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

The spline is based on a parameter that ranges from 0 to 1; as a result, in most
cases the length of each derivative vector should be roughly equal to the length
of the resulting spline.

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
        CubicSpline2d.with
            { startPoint =
                Point2d.meters 1 1
            , startControlPoint =
                Point2d.meters 3 4
            , endControlPoint =
                Point2d.meters 5 1
            , endPoint =
                Point2d.meters 7 4
            }
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 1 0 1
    -->     , startControlPoint =
    -->         Point3d.meters 3 0 4
    -->     , endControlPoint =
    -->         Point3d.meters 5 0 1
    -->     , endPoint =
    -->         Point3d.meters 7 0 4
    -->     }

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
        QuadraticSpline3d.with
            { startPoint =
                Point3d.fromCoordinates ( 0, 0, 0  )
            , controlPoint =
                Point3d.meters 3 0 0
            , endPoint =
                Point3d.meters 3 3 0
            }

    CubicSpline3d.fromQuadraticSpline quadraticSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 0 0 0
    -->     , startControlPoint =
    -->         Point3d.meters 2 0 0
    -->     , endControlPoint =
    -->         Point3d.meters 3 1 0
    -->     , endPoint =
    -->         Point3d.meters 3 3 0
    -->     )

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


{-| Get the start point of a spline. Equal to [`firstControlPoint`](#firstControlPoint).
-}
startPoint : CubicSpline3d units coordinates -> Point3d units coordinates
startPoint (Types.CubicSpline3d spline) =
    spline.firstControlPoint


{-| Get the end point of a spline. Equal to [`fourthControlPoint`](#fourthControlPoint).
-}
endPoint : CubicSpline3d units coordinates -> Point3d units coordinates
endPoint (Types.CubicSpline3d spline) =
    spline.fourthControlPoint


{-| Get the first control point of the spline. Equal to [`startPoint`](#startPoint).
-}
firstControlPoint : CubicSpline3d units coordinates -> Point3d units coordinates
firstControlPoint (Types.CubicSpline3d spline) =
    spline.firstControlPoint


{-| Get the second control point of the spline.
-}
secondControlPoint : CubicSpline3d units coordinates -> Point3d units coordinates
secondControlPoint (Types.CubicSpline3d spline) =
    spline.secondControlPoint


{-| Get the third control point of the spline.
-}
thirdControlPoint : CubicSpline3d units coordinates -> Point3d units coordinates
thirdControlPoint (Types.CubicSpline3d spline) =
    spline.thirdControlPoint


{-| Get the fourth and last control point of the spline. Equal to [`endPoint`](#endPoint).
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

    CubicSpline3d.boundingBox exampleSpline
    --> BoundingBox3d.fromExtrema
    -->     { minX = Length.meters 1
    -->     , maxX = Length.meters 3
    -->     , minY = Length.meters 1
    -->     , maxY = Length.meters 3
    -->     , minZ = Length.meters 1
    -->     , maxZ = Length.meters 3
    -->     }

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


{-| Get a point at a given parameter value.

    CubicSpline3d.pointOn exampleSpline ParameterValue.zero
    --> Point3d.meters 1 1 1

    CubicSpline3d.pointOn exampleSpline ParameterValue.half
    --> Point3d.meters 2.75 2 1.25

    CubicSpline3d.pointOn exampleSpline ParameterValue.one
    --> Point3d.meters 3 3 3

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


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents a spline that is definitely not degenerate.
It is used as input to functions such as `CubicSpline3d.tangentDirection` and
can be constructed using `CubicSpline3d.nondegenerate`.

-}
type Nondegenerate units coordinates
    = NonZeroThirdDerivative (CubicSpline3d units coordinates) (Direction3d coordinates)
    | NonZeroSecondDerivative (CubicSpline3d units coordinates) (Direction3d coordinates)
    | NonZeroFirstDerivative (CubicSpline3d units coordinates) (Direction3d coordinates)


{-| Attempt to construct a nondegenerate spline from a general `CubicSpline3d`.
If the spline is in fact degenerate (consists of a single point), returns an
`Err` with that point.

    CubicSpline3d.nondegenerate exampleSpline
    --> Ok nondegenerateExampleSpline

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

    CubicSpline3d.fromNondegenerate
        nondegenerateExampleSpline
    --> exampleSpline

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
value:

    CubicSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.zero
    --> Direction3d.x

    CubicSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.half
    --> Direction3d.fromAzimuthAndElevation
    -->     (Angle.degrees 63.43)
    -->     (Angle.degrees 24.09)

    CubicSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.one
    --> Direction3d.z

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
given parameter value:

    CubicSpline3d.sample nondegenerateExampleSpline
        ParameterValue.half
    --> ( Point3d.meters 2.75 2 1.25
    --> , Direction3d.fromAzimuthAndElevation
    -->     (Angle.degrees 63.43)
    -->     (Angle.degrees 24.09)
    --> )

-}
sample : Nondegenerate units coordinates -> Float -> ( Point3d units coordinates, Direction3d coordinates )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    CubicSpline3d.reverse exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 3 3 3
    -->     , startControlPoint =
    -->         Point3d.meters 3 3 1
    -->     , endControlPoint =
    -->         Point3d.meters 3 1 1
    -->     , endPoint =
    -->         Point3d.meters 1 1 1
    -->     }

-}
reverse : CubicSpline3d units coordinates -> CubicSpline3d units coordinates
reverse spline =
    fromControlPoints
        (fourthControlPoint spline)
        (thirdControlPoint spline)
        (secondControlPoint spline)
        (firstControlPoint spline)


{-| Scale a spline about the given center point by the given scale.

    CubicSpline3d.scaleAbout Point3d.origin 2 exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 2 2 2
    -->     , startControlPoint =
    -->         Point3d.meters 6 2 2
    -->     , endControlPoint =
    -->         Point3d.meters 6 6 2
    -->     , endPoint =
    -->         Point3d.meters 6 6 6
    -->     }

-}
scaleAbout : Point3d units coordinates -> Float -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point3d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given axis by a given angle (in
radians).

    exampleSpline
        |> CubicSpline3d.rotateAround Axis3d.z (Angle.degrees 90)
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters -1 1 1
    -->     , startControlPoint =
    -->         Point3d.meters -1 3 1
    -->     , endControlPoint =
    -->         Point3d.meters -3 3 1
    -->     , endPoint =
    -->         Point3d.meters -3 3 3
    -->     }

-}
rotateAround : Axis3d units coordinates -> Angle -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
rotateAround axis angle spline =
    mapControlPoints (Point3d.rotateAround axis angle) spline


{-| Translate a spline by a given displacement.

    displacement =
        Vector3d.meters 2 3 1

    CubicSpline3d.translateBy displacement exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 3 4 2
    -->     , startControlPoint =
    -->         Point3d.meters 5 4 2
    -->     , endControlPoint =
    -->         Point3d.meters 5 6 2
    -->     , endPoint =
    -->         Point3d.meters 5 6 4
    -->     }

-}
translateBy : Vector3d units coordinates -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
translateBy displacement spline =
    mapControlPoints (Point3d.translateBy displacement) spline


{-| Translate a spline in a given direction by a given distance;

    CubicSpline3d.translateIn direction distance

is equivalent to

    CubicSpline3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d coordinates -> Quantity Float units -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across a plane.

    CubicSpline3d.mirrorAcross Plane3d.xy exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 1 1 -1
    -->     , startControlPoint =
    -->         Point3d.meters 3 1 -1
    -->     , endControlPoint =
    -->         Point3d.meters 3 3 -1
    -->     , endPoint =
    -->         Point3d.meters 3 3 -3
    -->     }

-}
mirrorAcross : Plane3d units coordinates -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
mirrorAcross plane spline =
    mapControlPoints (Point3d.mirrorAcross plane) spline


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a spline onto a plane.

    CubicSpline3d.projectOnto Plane3d.xy exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 1 1 0
    -->     , startControlPoint =
    -->         Point3d.meters 3 1 0
    -->     , endControlPoint =
    -->         Point3d.meters 3 3 0
    -->     , endPoint =
    -->         Point3d.meters 3 3 0
    -->     }

-}
projectOnto : Plane3d units coordinates -> CubicSpline3d units coordinates -> CubicSpline3d units coordinates
projectOnto plane spline =
    mapControlPoints (Point3d.projectOnto plane) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.meters 1 2 3)

    CubicSpline3d.relativeTo localFrame exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 0 -1 -2
    -->     , startControlPoint =
    -->         Point3d.meters 2 -1 -2
    -->     , endControlPoint =
    -->         Point3d.meters 2 1 -2
    -->     , endPoint =
    -->         Point3d.meters 2 1 0
    -->     }

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> CubicSpline3d units globalCoordinates -> CubicSpline3d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point3d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.meters 1 2 3)

    CubicSpline3d.placeIn localFrame exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 2 3 4
    -->     , startControlPoint =
    -->         Point3d.meters 4 3 4
    -->     , endControlPoint =
    -->         Point3d.meters 4 5 4
    -->     , endPoint =
    -->         Point3d.meters 4 5 6
    -->     }

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> CubicSpline3d units localCoordinates -> CubicSpline3d units globalCoordinates
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| Project a spline into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the spline onto the plane and then expresses the projected spline in 2D
sketch coordinates.

    exampleSpline
        |> CubicSpline3d.projectInto SketchPlane3d.yz
    --> CubicSpline2d.with
    -->     { startPoint =
    -->         Point2d.meters 1 1
    -->     , startControlPoint =
    -->         Point2d.meters 1 1
    -->     , endControlPoint =
    -->         Point2d.meters 3 1
    -->     , endPoint =
    -->         Point2d.meters 3 3
    -->     }

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


{-| Split a spline into two roughly equal halves.

    CubicSpline3d.bisect exampleSpline
    --> ( CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 1 1 1
    -->     , startControlPoint =
    -->         Point3d.meters 2 1 1
    -->     , endControlPoint =
    -->         Point3d.meters 2.5 1.5 1
    -->     , endPoint =
    -->         Point3d.meters 2.75 2 1.25
    -->     }
    --> , CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 2.75 2 1.25
    -->     , startControlPoint =
    -->         Point3d.meters 3 2.5 1.5
    -->     , endControlPoint =
    -->         Point3d.meters 3 3 2
    -->     , endPoint =
    -->         Point3d.meters 3 3 3
    -->     }
    --> )

Equivalent to `CubicSpline3d.splitAt ParameterValue.half`.

-}
bisect : CubicSpline3d units coordinates -> ( CubicSpline3d units coordinates, CubicSpline3d units coordinates )
bisect spline =
    splitAt 0.5 spline


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.

    parameterValue =
        ParameterValue.clamped 0.75

    CubicSpline3d.splitAt parameterValue exampleSpline
    --> ( CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 1 1 1
    -->     , startControlPoint =
    -->         Point3d.meters 2.5 1 1
    -->     , endControlPoint =
    -->         Point3d.meters 2.88 2.13 1
    -->     , endPoint =
    -->         Point3d.meters 2.97 2.69 1.84
    -->     }
    --> , CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.meters 2.97 2.69 1.84
    -->     , startControlPoint =
    -->         Point3d.meters 3 2.88 2.13
    -->     , endControlPoint =
    -->         Point3d.meters 3 3 2.5
    -->     , endPoint =
    -->         Point3d.meters 3 3 3
    -->     }
    --> )

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
accuracy. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the specified maximum
error.

    parameterizedSpline =
        exampleSpline
            |> CubicSpline3d.arcLengthParameterized
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
                , maxSecondDerivativeMagnitude =
                    maxSecondDerivativeMagnitude spline
                }
    in
    ArcLengthParameterized
        { underlyingSpline = spline
        , parameterization = parameterization
        , nondegenerateSpline = nondegenerateSpline
        }


{-| Find the total arc length of a spline:

    arcLength =
        CubicSpline3d.arcLength parameterizedSpline

    arcLength
    --> 4.3303

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized units coordinates -> Quantity Float units
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| Get the midpoint of a spline.

    CubicSpline3d.midpoint parameterizedSpline
    --> Point3d.meters 2.75 2 1.25

Note that this is the point half way along the spline by arc length, which is
not in general the same as evaluating at a parameter value of 0.5.

-}
midpoint : ArcLengthParameterized units coordinates -> Point3d units coordinates
midpoint parameterized =
    let
        halfArcLength =
            Quantity.multiplyBy 0.5 (arcLength parameterized)
    in
    pointAlong parameterized halfArcLength


{-| Try to get the point along a spline at a given arc length. For example, to
get the point a quarter of the way along `exampleSpline`:

    CubicSpline3d.pointAlong parameterizedSpline
        (arcLength / 4)
    --> Just <|
    -->     Point3d.fromCoordinates
    -->         ( 2.0425, 1.2431, 1.0206 )

Note that this is not the same as evaulating at a parameter value of 1/4:

    CubicSpline3d.pointOn exampleSpline
        (ParameterValue.clamped 0.25)
    --> Point3d.meters 2.1563 1.3125 1.0313

If the given arc length is less than zero or greater than the arc length of the
spline, returns `Nothing`.

-}
pointAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Point3d units coordinates
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> pointOn parameterized.underlyingSpline


{-| Try to get the tangent direction along a spline at a given arc length. To
get the tangent direction a quarter of the way along `exampleSpline`:

    CubicSpline3d.tangentDirectionAlong parameterizedSpline
        (0.25 * arcLength)
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (Angle.degrees 29.1)
    -->         (Angle.degrees 3.871)
    -->     )

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), returns `Nothing`.

-}
tangentDirectionAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Direction3d coordinates
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> tangentDirection parameterized.nondegenerateSpline


{-| Try to get the point and tangent direction along a spline at a given arc
length. To get the point and tangent direction a quarter of the way along
`exampleSpline`:

    CubicSpline3d.sampleAlong parameterizedSpline
        (0.25 * arcLength)
    --> Just
    -->     ( Point3d.fromCoordinates
    -->         ( 2.0425, 1.2431, 1.0206 )
    -->     , Direction3d.fromAzimuthAndElevation
    -->         (Angle.degrees 29.1)
    -->         (Angle.degrees 3.871)
    -->     )

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), returns `Nothing`.

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

    CubicSpline3d.derivative exampleSpline
        ParameterValue.zero
    --> Vector3d.meters 6 0 0

    CubicSpline3d.derivative exampleSpline
        ParameterValue.half
    --> Vector3d.meters 1.5 3 1.5

    CubicSpline3d.derivative exampleSpline
        ParameterValue.one
    --> Vector3d.meters 0 0 6

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


{-| Get the second derivative value at a point along a spline, based on a
parameter that ranges from 0 to 1. A parameter value of 0 corresponds to the
start of the spline and a value of 1 corresponds to the end.

    CubicSpline3d.secondDerivative exampleSpline
        ParameterValue.zero
    --> Vector3d.meters -12 12 0

    CubicSpline3d.secondDerivative exampleSpline
        ParameterValue.half
    --> Vector3d.meters -6 0 6

    CubicSpline3d.secondDerivative exampleSpline
        ParameterValue.one
    --> Vector3d.meters 0 -12 12

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
derivativeMagnitude spline =
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

        x12 =
            x2 |> Quantity.minus x1

        y12 =
            y2 |> Quantity.minus y1

        z12 =
            z2 |> Quantity.minus z1

        x23 =
            x3 |> Quantity.minus x2

        y23 =
            y3 |> Quantity.minus y2

        z23 =
            z3 |> Quantity.minus z2

        x34 =
            x4 |> Quantity.minus x3

        y34 =
            y4 |> Quantity.minus y3

        z34 =
            z4 |> Quantity.minus z3

        x123 =
            x23 |> Quantity.minus x12

        y123 =
            y23 |> Quantity.minus y12

        z123 =
            z23 |> Quantity.minus z12

        x234 =
            x34 |> Quantity.minus x23

        y234 =
            y34 |> Quantity.minus y23

        z234 =
            z34 |> Quantity.minus z23
    in
    \parameterValue ->
        let
            x13 =
                x12 |> Quantity.plus (Quantity.multiplyBy parameterValue x123)

            y13 =
                y12 |> Quantity.plus (Quantity.multiplyBy parameterValue y123)

            z13 =
                z12 |> Quantity.plus (Quantity.multiplyBy parameterValue z123)

            x24 =
                x23 |> Quantity.plus (Quantity.multiplyBy parameterValue x234)

            y24 =
                y23 |> Quantity.plus (Quantity.multiplyBy parameterValue y234)

            z24 =
                z23 |> Quantity.plus (Quantity.multiplyBy parameterValue z234)

            x14 =
                Quantity.interpolateFrom x13 x24 parameterValue

            y14 =
                Quantity.interpolateFrom y13 y24 parameterValue

            z14 =
                Quantity.interpolateFrom z13 z24 parameterValue
        in
        Quantity.multiplyBy 3 <|
            Quantity.sqrt
                (Quantity.squared x14
                    |> Quantity.plus (Quantity.squared y14)
                    |> Quantity.plus (Quantity.squared z14)
                )
