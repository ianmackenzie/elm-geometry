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
    , startPoint, endPoint, firstControlPoint, secondControlPoint, thirdControlPoint, startDerivative, endDerivative, boundingBox
    , pointOn, pointsAt
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, tangentDirectionsAt, sample, samplesAt
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto
    , relativeTo, placeIn
    , projectInto
    , bisect, splitAt
    , ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, midpoint, tangentDirectionAlong, sampleAlong
    , arcLengthParameterization, fromArcLengthParameterized
    , firstDerivative, firstDerivativesAt, secondDerivative
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


# Properties

@docs startPoint, endPoint, firstControlPoint, secondControlPoint, thirdControlPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn, pointsAt
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, tangentDirectionsAt, sample, samplesAt


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn


# Sketch planes

@docs projectInto


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, midpoint, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `QuadraticSpline3d`. If you need to do something fancy, you can
extract these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, firstDerivativesAt, secondDerivative

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Curve.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias QuadraticSpline3d units coordinates =
    Types.QuadraticSpline3d units coordinates


{-| Construct a spline from its start point, inner control point and end point:

    exampleSpline =
        QuadraticSpline3d.fromControlPoints
            (Point3d.fromTuple meters ( 1, 1, 1 ))
            (Point3d.fromTuple meters ( 3, 2, 1 ))
            (Point3d.fromTuple meters ( 3, 3, 3 ))

-}
fromControlPoints : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> QuadraticSpline3d units coordinates
fromControlPoints p1 p2 p3 =
    Types.QuadraticSpline3d
        { firstControlPoint = p1
        , secondControlPoint = p2
        , thirdControlPoint = p3
        }


{-| Construct a 3D spline lying _on_ a sketch plane by providing a 2D spline
specified in XY coordinates _within_ the sketch plane.

    QuadraticSpline3d.on SketchPlane3d.xz <|
        QuadraticSpline2d.with
            { startPoint =
                Point2d.fromCoordinates ( 1, 1 )
            , controlPoint =
                Point2d.fromCoordinates ( 3, 4 )
            , endPoint =
                Point2d.fromCoordinates ( 5, 1 )
            }
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 0, 1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 0, 4 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 5, 0, 1 )
    -->     }

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> QuadraticSpline2d units coordinates2d -> QuadraticSpline3d units coordinates3d
on sketchPlane spline2d =
    fromControlPoints
        (Point3d.on sketchPlane (QuadraticSpline2d.firstControlPoint spline2d))
        (Point3d.on sketchPlane (QuadraticSpline2d.secondControlPoint spline2d))
        (Point3d.on sketchPlane (QuadraticSpline2d.thirdControlPoint spline2d))


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
    --> Vector3d.fromComponents ( 4, 2, 0 )

-}
startDerivative : QuadraticSpline3d units coordinates -> Vector3d units coordinates
startDerivative spline =
    Vector3d.from (firstControlPoint spline) (secondControlPoint spline)
        |> Vector3d.scaleBy 2


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline3d.endDerivative exampleSpline
    --> Vector3d.fromComponents ( 0, 2, 4 )

-}
endDerivative : QuadraticSpline3d units coordinates -> Vector3d units coordinates
endDerivative spline =
    Vector3d.from (secondControlPoint spline) (thirdControlPoint spline)
        |> Vector3d.scaleBy 2


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
volume than the spline itself).

    QuadraticSpline3d.boundingBox exampleSpline
    --> BoundingBox3d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 3
    -->     , minY = 1
    -->     , maxY = 3
    -->     , minZ = 1
    -->     , maxZ = 3
    -->     }

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


{-| Get the point along a spline at a given parameter value:

    QuadraticSpline3d.pointOn exampleSpline 0
    --> Point3d.fromCoordinates ( 1, 1, 1 )

    QuadraticSpline3d.pointOn exampleSpline 0.5
    --> Point3d.fromCoordinates ( 2.5, 2, 1.5 )

    QuadraticSpline3d.pointOn exampleSpline 1
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
pointOn : QuadraticSpline3d units coordinates -> ParameterValue -> Point3d units coordinates
pointOn spline parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t
    in
    Point3d.interpolateFrom q1 q2 t


{-| Get points along a spline at a given set of parameter values:

    exampleSpline
        |> QuadraticSpline3d.pointsAt
            (ParameterValue.steps 2)
    --> [ Point3d.fromCoordinates ( 1, 1, 1 )
    --> , Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    --> , Point3d.fromCoordinates ( 3, 3, 3 )
    --> ]

-}
pointsAt : List ParameterValue -> QuadraticSpline3d units coordinates -> List (Point3d units coordinates)
pointsAt parameterValues spline =
    List.map (pointOn spline) parameterValues


{-| Get the first derivative of a spline at a given parameter value:

    QuadraticSpline3d.derivative exampleSpline
        ParameterValue.zero
    --> Vector3d.fromComponents ( 4, 2, 0 )

    QuadraticSpline3d.derivative exampleSpline
        ParameterValue.half
    --> Vector3d.fromComponents ( 2, 2, 2 )

    QuadraticSpline3d.derivative exampleSpline
        ParameterValue.one
    --> Vector3d.fromComponents ( 0, 2, 4 )

Note that the derivative interpolates linearly from end to end.

-}
firstDerivative : QuadraticSpline3d units coordinates -> ParameterValue -> Vector3d units coordinates
firstDerivative spline parameterValue =
    let
        t =
            ParameterValue.value parameterValue

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
    Vector3d.interpolateFrom v1 v2 t |> Vector3d.scaleBy 2


{-| Evaluate the first derivative of a spline at a range of parameter values:

    exampleSpline
        |> QuadraticSpline3d.firstDerivativesAt
            (ParameterValue.steps 2)
    --> [ Vector3d.fromComponents ( 4, 2, 0 )
    --> , Vector3d.fromComponents ( 2, 2, 2 )
    --> , Vector3d.fromComponents ( 0, 2, 4 )
    --> ]

-}
firstDerivativesAt : List ParameterValue -> QuadraticSpline3d units coordinates -> List (Vector3d units coordinates)
firstDerivativesAt parameterValues spline =
    List.map (firstDerivative spline) parameterValues


derivativeMagnitude : QuadraticSpline3d units coordinates -> ParameterValue -> Quantity Float units
derivativeMagnitude spline =
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

        x123 =
            x23 |> Quantity.minus x12

        y123 =
            y23 |> Quantity.minus y12

        z123 =
            z23 |> Quantity.minus z12
    in
    \parameterValue ->
        let
            t =
                ParameterValue.value parameterValue

            x13 =
                x12 |> Quantity.plus (Quantity.multiplyBy t x123)

            y13 =
                y12 |> Quantity.plus (Quantity.multiplyBy t y123)

            z13 =
                z12 |> Quantity.plus (Quantity.multiplyBy t z123)
        in
        Quantity.multiplyBy 2 <|
            Quantity.sqrt
                (Quantity.squared x13
                    |> Quantity.plus (Quantity.squared y13)
                    |> Quantity.plus (Quantity.squared z13)
                )


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents a spline that is definitely not degenerate.
It is used as input to functions such as `QuadraticSpline3d.tangentDirection`
and can be constructed using `QuadraticSpline3d.nondegenerate`.

-}
type Nondegenerate units coordinates
    = NonZeroSecondDerivative (QuadraticSpline3d units coordinates) (Direction3d coordinates)
    | NonZeroFirstDerivative (QuadraticSpline3d units coordinates) (Direction3d coordinates)


{-| Attempt to construct a nondegenerate spline from a general
`QuadraticSpline3d`. If the spline is in fact degenerate (consists of a single
point), returns an `Err` with that point.

    QuadraticSpline3d.nondegenerate exampleSpline
    --> Ok nondegenerateExampleSpline

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
                    firstDerivative spline ParameterValue.zero
            in
            case Vector3d.direction firstDerivativeVector of
                Just direction ->
                    Ok (NonZeroFirstDerivative spline direction)

                Nothing ->
                    Err (startPoint spline)


{-| Convert a nondegenerate spline back to a general `QuadraticSpline3d`.

    QuadraticSpline3d.fromNondegenerate
        nondegenerateExampleSpline
    --> exampleSpline

-}
fromNondegenerate : Nondegenerate units coordinates -> QuadraticSpline3d units coordinates
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| Get the tangent direction to a nondegenerate spline at a given parameter
value:

    QuadraticSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.zero
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 26.57)
    -->     (degrees 0)

    QuadraticSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.half
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 45)
    -->     (degrees 35.26)

    QuadraticSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.one
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 90)
    -->     (degrees 63.43)

-}
tangentDirection : Nondegenerate units coordinates -> ParameterValue -> Direction3d coordinates
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
                    if parameterValue == ParameterValue.one then
                        Direction3d.reverse secondDerivativeDirection

                    else
                        secondDerivativeDirection

        NonZeroFirstDerivative spline firstDerivativeDirection ->
            -- Tangent direction is always equal to the (constant) first
            -- derivative direction
            firstDerivativeDirection


{-| Get tangent directions to a nondegenerate spline at a given set of parameter
values:

    nondegenerateExampleSpline
        |> QuadraticSpline3d.tangentDirectionsAt
            (ParameterValue.steps 2)
    --> [ Direction3d.fromAzimuthAndElevation
    -->     (degrees 26.57)
    -->     (degrees 0)
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 45)
    -->     (degrees 35.26)
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 90)
    -->     (degrees 63.43)
    --> ]

-}
tangentDirectionsAt : List ParameterValue -> Nondegenerate units coordinates -> List (Direction3d coordinates)
tangentDirectionsAt parameterValues nondegenerateSpline =
    List.map (tangentDirection nondegenerateSpline) parameterValues


{-| Get both the point and tangent direction of a nondegenerate spline at a
given parameter value:

    QuadraticSpline3d.sample nondegenerateExampleSpline
        ParameterValue.half
    --> ( Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 45)
    -->     (degrees 35.26)
    --> )

-}
sample : Nondegenerate units coordinates -> ParameterValue -> ( Point3d units coordinates, Direction3d coordinates )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| Get points and tangent directions of a nondegenerate spline at a given set
of parameter values:

    nondegenerateExampleSpline
        |> QuadraticSpline3d.samplesAt
            (ParameterValue.steps 2)
    --> [ ( Point3d.fromCoordinates ( 1, 1, 1 )
    -->   , Direction3d.fromAzimuthAndElevation
    -->         (degrees 26.57)
    -->         (degrees 0)
    -->   )
    --> , ( Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    -->   , Direction3d.fromAzimuthAndElevation
    -->         (degrees 45)
    -->         (degrees 35.26)
    -->   )
    --> , ( Point3d.fromCoordinates ( 3, 3, 3 )
    -->   , Direction3d.fromAzimuthAndElevation
    -->         (degrees 90)
    -->         (degrees 63.43)
    -->   )
    --> ]

-}
samplesAt : List ParameterValue -> Nondegenerate units coordinates -> List ( Point3d units coordinates, Direction3d coordinates )
samplesAt parameterValues nondegenerateSpline =
    List.map (sample nondegenerateSpline) parameterValues


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    QuadraticSpline3d.reverse exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 3 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 2, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 1 )
    -->     }

-}
reverse : QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
reverse spline =
    fromControlPoints
        (thirdControlPoint spline)
        (secondControlPoint spline)
        (firstControlPoint spline)


{-| Scale a spline about the given center point by the given scale.

    exampleSpline
        |> QuadraticSpline3d.scaleAbout Point3d.origin 2
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2, 2, 2 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 6, 4, 2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 6, 6, 6 )
    -->     }

-}
scaleAbout : Point3d units coordinates -> Float -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point3d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given axis by a given angle (in
radians).

    exampleSpline
        |> QuadraticSpline3d.rotateAround Axis3d.z
            (degrees 90)
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( -1, 1, 1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( -2, 3, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( -3, 3, 3 )
    -->     }

-}
rotateAround : Axis3d units coordinates -> Angle -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
rotateAround axis angle spline =
    mapControlPoints (Point3d.rotateAround axis angle) spline


{-| Translate a spline by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 3, 1 )

    exampleSpline
        |> QuadraticSpline3d.translateBy displacement
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 3, 4, 2 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 5, 5, 2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 5, 6, 4 )
    -->     }

-}
translateBy : Vector3d units coordinates -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
translateBy displacement spline =
    mapControlPoints (Point3d.translateBy displacement) spline


{-| Translate an arc in a given direction by a given distance;

    QuadraticSpline3d.translateIn direction distance

is equivalent to

    QuadraticSpline3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d coordinates -> Quantity Float units -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across a plane.

    QuadraticSpline3d.mirrorAcross Plane3d.xy exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, -1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 2, -1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, -3 )
    -->     }

-}
mirrorAcross : Plane3d units coordinates -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
mirrorAcross plane spline =
    mapControlPoints (Point3d.mirrorAcross plane) spline


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a spline onto a plane.

    QuadraticSpline3d.projectOnto Plane3d.xy exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 0 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 2, 0 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 0 )
    -->     }

-}
projectOnto : Plane3d units coordinates -> QuadraticSpline3d units coordinates -> QuadraticSpline3d units coordinates
projectOnto plane spline =
    mapControlPoints (Point3d.projectOnto plane) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    QuadraticSpline3d.relativeTo localFrame exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 0, -1, -2 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 2, 0, -2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 2, 1, 0 )
    -->     }

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> QuadraticSpline3d units globalCoordinates -> QuadraticSpline3d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point3d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    QuadraticSpline3d.placeIn localFrame exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2, 3, 4 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 4, 4, 4 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 4, 5, 6 )
    -->     }

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> QuadraticSpline3d units localCoordinates -> QuadraticSpline3d units globalCoordinates
placeIn frame spline =
    mapControlPoints (Point3d.placeIn frame) spline


{-| Project a spline into a given sketch plane. Conceptually, finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the spline onto the plane and then expresses the projected spline in 2D
sketch coordinates.

    exampleSpline
        |> QuadraticSpline3d.projectInto SketchPlane3d.yz
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 1, 1 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 2, 1 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 3, 3 )
    -->     }

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


{-| Split a spline into two roughly equal halves.

    QuadraticSpline3d.bisect exampleSpline
    --> ( QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 2, 2.5 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 2.5 )
    -->     }
    --> , QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 3, 2.5 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 4, 2.5 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 3 )
    -->     }
    --> )

Equivalent to `QuadraticSpline3d.splitAt ParameterValue.half`.

-}
bisect : QuadraticSpline3d units coordinates -> ( QuadraticSpline3d units coordinates, QuadraticSpline3d units coordinates )
bisect =
    splitAt ParameterValue.half


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.

    parameterValue =
        ParameterValue.clamped 0.75

    QuadraticSpline3d.splitAt parameterValue exampleSpline
    --> ( QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 2, 1.5, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    -->     }
    --> , QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 2.5, 2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 3 )
    -->     }
    --> )

-}
splitAt : ParameterValue -> QuadraticSpline3d units coordinates -> ( QuadraticSpline3d units coordinates, QuadraticSpline3d units coordinates )
splitAt parameterValue spline =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t

        r =
            Point3d.interpolateFrom q1 q2 t
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
        , nondegenerateSpline : Maybe (Nondegenerate units coordinates)
        }


{-| Build an arc length parameterization of the given spline, with a given
accuracy. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the specified maximum
error.

    parameterizedSpline =
        exampleSpline
            |> QuadraticSpline3d.arcLengthParameterized
                { maxError = 1.0e-4 }

-}
arcLengthParameterized : { maxError : Quantity Float units } -> QuadraticSpline3d units coordinates -> ArcLengthParameterized units coordinates
arcLengthParameterized { maxError } spline =
    let
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
        , nondegenerateSpline = Result.toMaybe (nondegenerate spline)
        }


{-| Find the total arc length of a spline.

    QuadraticSpline3d.arcLength parameterizedSpline
    --> 3.8175

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized units coordinates -> Quantity Float units
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| Try to get the point along a spline at a given arc length. For example, to
get the point a quarter of the way along `exampleSpline`:

    QuadraticSpline3d.pointAlong parameterizedSpline
        (arcLength / 4)
    --> Just <|
    -->     Point3d.fromCoordinates
    -->         ( 1.8227, 1.4655, 1.1083 )

Note that this is not the same as evaulating at a parameter value of 1/4:

    QuadraticSpline3d.pointOn exampleSpline 0.25
    --> Point3d.fromCoordinates ( 1.875, 1.5, 1.125 )

If the given arc length is less than zero or greater than the arc length of the
spline, returns `Nothing`.

-}
pointAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Maybe (Point3d units coordinates)
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> Maybe.map (pointOn parameterized.underlyingSpline)


{-| Get the midpoint of a spline.

    QuadraticSpline3d.midpoint parameterizedSpline
    --> Point3d.fromCoordinates ( 2.5, 2, 1.5 )

Note that this is the point half way along the spline by arc length, which is
not in general the same as evaluating at a parameter value of 0.5.

-}
midpoint : ArcLengthParameterized units coordinates -> Point3d units coordinates
midpoint parameterized =
    let
        halfArcLength =
            Quantity.multiplyBy 0.5 (arcLength parameterized)
    in
    case pointAlong parameterized halfArcLength of
        Just point ->
            point

        Nothing ->
            -- Should never happen since half of total arc length will always
            -- be a valid distance along the curve, but let's default to something
            -- reasonable anyways
            startPoint (fromArcLengthParameterized parameterized)


{-| Try to get the tangent direction along a spline at a given arc length. To
get the tangent direction a quarter of the way along `exampleSpline`:

    QuadraticSpline3d.tangentDirectionAlong
        parameterizedSpline
        (arcLength / 4)
    --> Just <|
    -->     Direction3d.fromAzimuthAndElevation
    -->         (degrees 33.09)
    -->         (degrees 14.26)

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), returns `Nothing`.

-}
tangentDirectionAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Maybe (Direction3d coordinates)
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    case parameterized.nondegenerateSpline of
        Just nondegenerateSpline ->
            parameterized.parameterization
                |> ArcLengthParameterization.arcLengthToParameterValue distance
                |> Maybe.map (tangentDirection nondegenerateSpline)

        Nothing ->
            Nothing


{-| Try to get the point and tangent direction along a spline at a given arc
length. To get the point and tangent direction a quarter of the way along
`exampleSpline`:

    QuadraticSpline3d.sampleAlong parameterizedSpline
        (0.25 * arcLength)
    --> Just
    -->     ( Point3d.fromCoordinates
    -->         ( 1.8227, 1.4655, 1.1083 )
    -->     , Direction3d.fromAzimuthAndElevation
    -->         (degrees 33.09)
    -->         (degrees 14.26)
    -->     )

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), `Nothing` is returned.

-}
sampleAlong : ArcLengthParameterized units coordinates -> Quantity Float units -> Maybe ( Point3d units coordinates, Direction3d coordinates )
sampleAlong (ArcLengthParameterized parameterized) distance =
    case parameterized.nondegenerateSpline of
        Just nondegenerateSpline ->
            parameterized.parameterization
                |> ArcLengthParameterization.arcLengthToParameterValue distance
                |> Maybe.map (sample nondegenerateSpline)

        Nothing ->
            Nothing


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
    Vector3d.scaleBy 2 (v2 |> Vector3d.minus v1)
