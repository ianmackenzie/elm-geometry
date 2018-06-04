module CubicSpline3d
    exposing
        ( ArcLengthParameterized
        , CubicSpline3d
        , arcLength
        , arcLengthParameterization
        , arcLengthParameterized
        , bisect
        , boundingBox
        , endControlPoint
        , endDerivative
        , endPoint
        , firstDerivative
        , firstDerivativesAt
        , fromEndpoints
        , fromQuadraticSpline
        , maxSecondDerivativeMagnitude
        , mirrorAcross
        , on
        , placeIn
        , pointAlong
        , pointOn
        , pointsAt
        , projectInto
        , projectOnto
        , relativeTo
        , reverse
        , rotateAround
        , sampleAlong
        , sampler
        , samplesAt
        , scaleAbout
        , secondDerivative
        , secondDerivativesAt
        , splitAt
        , startControlPoint
        , startDerivative
        , startPoint
        , thirdDerivative
        , translateBy
        , translateIn
        , underlyingSpline
        , with
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/CubicSpline3d/icon.svg" alt="CubicSpline3d" width="160">

A `CubicSpline3d` is a cubic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 3D defined by a start point, end point and two control points. This module
contains functionality for

  - Constructing splines
  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs CubicSpline3d


# Constructors

@docs with, fromEndpoints, on, fromQuadraticSpline


# Properties

@docs startPoint, endPoint, startControlPoint, endControlPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn, pointsAt, sampler, samplesAt


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn, projectInto


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `CubicSpline3d`. If you need to do something fancy, you can extract
these two values separately.

@docs arcLengthParameterization, underlyingSpline


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms. As with the other curve
evaluation functions, passing a parameter value outside the range 0 to 1 will
result in `Nothing`.

@docs firstDerivative, firstDerivativesAt, secondDerivative, secondDerivativesAt, thirdDerivative, maxSecondDerivativeMagnitude

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import CubicSpline2d exposing (CubicSpline2d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Accuracy exposing (Accuracy)
import Geometry.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Geometry.ParameterValue as ParameterValue exposing (ParameterValue)
import Geometry.ParameterValues as ParameterValues exposing (ParameterValues)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias CubicSpline3d =
    Types.CubicSpline3d


{-| Construct a spline from its four control points:

    exampleSpline =
        CubicSpline3d.with
            { startPoint =
                Point3d.fromCoordinates ( 1, 1, 1 )
            , startControlPoint =
                Point3d.fromCoordinates ( 3, 1, 1 )
            , endControlPoint =
                Point3d.fromCoordinates ( 3, 3, 1 )
            , endPoint =
                Point3d.fromCoordinates ( 3, 3, 3 )
            }

-}
with : { startPoint : Point3d, startControlPoint : Point3d, endControlPoint : Point3d, endPoint : Point3d } -> CubicSpline3d
with =
    Types.CubicSpline3d


{-| Construct a spline from a given start point with a given start derivative,
to a given end point with a given end derivative, like so:

![Cubic spline from endpoints](https://ianmackenzie.github.io/elm-geometry/1.0.0/CubicSpline2d/fromEndpoints.svg)

The spline is based on a parameter that ranges from 0 to 1; as a result, in most
cases the length of each derivative vector should be roughly equal to the length
of the resulting spline.

-}
fromEndpoints : { startPoint : Point3d, startDerivative : Vector3d, endPoint : Point3d, endDerivative : Vector3d } -> CubicSpline3d
fromEndpoints arguments =
    let
        startControlPoint_ =
            arguments.startPoint
                |> Point3d.translateBy
                    (Vector3d.scaleBy (1 / 3) arguments.startDerivative)

        endControlPoint_ =
            arguments.endPoint
                |> Point3d.translateBy
                    (Vector3d.scaleBy (-1 / 3) arguments.endDerivative)
    in
    with
        { startPoint = arguments.startPoint
        , startControlPoint = startControlPoint_
        , endControlPoint = endControlPoint_
        , endPoint = arguments.endPoint
        }


{-| Construct a 3D spline lying _on_ a sketch plane by providing a 2D spline
specified in XY coordinates _within_ the sketch plane.

    CubicSpline3d.on SketchPlane3d.xz <|
        CubicSpline2d.with
            { startPoint =
                Point2d.fromCoordinates ( 1, 1 )
            , startControlPoint =
                Point2d.fromCoordinates ( 3, 4 )
            , endControlPoint =
                Point2d.fromCoordinates ( 5, 1 )
            , endPoint =
                Point2d.fromCoordinates ( 7, 4 )
            }
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 0, 1 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 3, 0, 4 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 5, 0, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 7, 0, 4 )
    -->     }

-}
on : SketchPlane3d -> CubicSpline2d -> CubicSpline3d
on sketchPlane spline2d =
    with
        { startPoint =
            Point3d.on sketchPlane (CubicSpline2d.startPoint spline2d)
        , startControlPoint =
            Point3d.on sketchPlane (CubicSpline2d.startControlPoint spline2d)
        , endControlPoint =
            Point3d.on sketchPlane (CubicSpline2d.endControlPoint spline2d)
        , endPoint =
            Point3d.on sketchPlane (CubicSpline2d.endPoint spline2d)
        }


{-| Convert a quadratic spline into the equivalent cubic spline (every quadratic
spline can be represented exactly as a cubic spline).

    quadraticSpline =
        QuadraticSpline3d.with
            { startPoint =
                Point3d.fromCoordinates ( 0, 0, 0  )
            , controlPoint =
                Point3d.fromCoordinates ( 3, 0, 0 )
            , endPoint =
                Point3d.fromCoordinates ( 3, 3, 0 )
            }

    CubicSpline3d.fromQuadraticSpline quadraticSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 0, 0, 0 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 2, 0, 0 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 3, 1, 0 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 0 )
    -->     )

-}
fromQuadraticSpline : QuadraticSpline3d -> CubicSpline3d
fromQuadraticSpline quadraticSpline =
    let
        startPoint_ =
            QuadraticSpline3d.startPoint quadraticSpline

        controlPoint_ =
            QuadraticSpline3d.controlPoint quadraticSpline

        endPoint_ =
            QuadraticSpline3d.endPoint quadraticSpline

        startControlPoint_ =
            Point3d.interpolateFrom startPoint_ controlPoint_ (2 / 3)

        endControlPoint_ =
            Point3d.interpolateFrom endPoint_ controlPoint_ (2 / 3)
    in
    with
        { startPoint = startPoint_
        , startControlPoint = startControlPoint_
        , endControlPoint = endControlPoint_
        , endPoint = endPoint_
        }


{-| Get the start point of a spline.

    CubicSpline3d.startPoint exampleSpline
    --> Point3d.fromCoordinates ( 1, 1, 1 )

-}
startPoint : CubicSpline3d -> Point3d
startPoint (Types.CubicSpline3d spline) =
    spline.startPoint


{-| Get the end point of a spline.

    CubicSpline3d.endPoint exampleSpline
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
endPoint : CubicSpline3d -> Point3d
endPoint (Types.CubicSpline3d spline) =
    spline.endPoint


{-| Get the start control point of a spline (the control point next to the
start point).
-}
startControlPoint : CubicSpline3d -> Point3d
startControlPoint (Types.CubicSpline3d spline) =
    spline.startControlPoint


{-| Get the end control point of a spline (the control point next to the
end point).
-}
endControlPoint : CubicSpline3d -> Point3d
endControlPoint (Types.CubicSpline3d spline) =
    spline.endControlPoint


{-| Get the start derivative of a spline. This is equal to three times the
vector from the spline's start point to its start control point.

    CubicSpline3d.startDerivative exampleSpline
    --> Vector3d.fromComponents ( 6, 0, 0 )

-}
startDerivative : CubicSpline3d -> Vector3d
startDerivative spline =
    Vector3d.from (startPoint spline) (startControlPoint spline)
        |> Vector3d.scaleBy 3


{-| Get the end derivative of a spline. This is equal to three times the vector
from the spline's end control point to its end point.

    CubicSpline3d.endDerivative exampleSpline
    --> Vector3d.fromComponents ( 0, 0, 6 )

-}
endDerivative : CubicSpline3d -> Vector3d
endDerivative spline =
    Vector3d.from (endControlPoint spline) (endPoint spline)
        |> Vector3d.scaleBy 3


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
volume than the spline itself).

    CubicSpline3d.boundingBox exampleSpline
    --> BoundingBox3d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 3
    -->     , minY = 1
    -->     , maxY = 3
    -->     , minZ = 1
    -->     , maxZ = 3
    -->     }

-}
boundingBox : CubicSpline3d -> BoundingBox3d
boundingBox spline =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates (startPoint spline)

        ( x2, y2, z2 ) =
            Point3d.coordinates (startControlPoint spline)

        ( x3, y3, z3 ) =
            Point3d.coordinates (endControlPoint spline)

        ( x4, y4, z4 ) =
            Point3d.coordinates (endPoint spline)
    in
    BoundingBox3d.fromExtrema
        { minX = min (min x1 x2) (min x3 x4)
        , maxX = max (max x1 x2) (max x3 x4)
        , minY = min (min y1 y2) (min y3 y4)
        , maxY = max (max y1 y2) (max y3 y4)
        , minZ = min (min z1 z2) (min z3 z4)
        , maxZ = max (max z1 z2) (max z3 z4)
        }


{-| Get a point at a given parameter value.

    CubicSpline3d.pointOn exampleSpline
        ParameterValue.zero
    --> Point3d.fromCoordinates ( 1, 1, 1 )

    CubicSpline3d.pointOn exampleSpline
        ParameterValue.oneHalf
    --> Point3d.fromCoordinates ( 2.75, 2, 1.25 )

    CubicSpline3d.pointOn exampleSpline
        ParameterValue.one
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
pointOn : CubicSpline3d -> ParameterValue -> Point3d
pointOn spline parameterValue =
    let
        t =
            ParameterValue.toFloat parameterValue

        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t

        q3 =
            Point3d.interpolateFrom p3 p4 t

        r1 =
            Point3d.interpolateFrom q1 q2 t

        r2 =
            Point3d.interpolateFrom q2 q3 t
    in
    Point3d.interpolateFrom r1 r2 t


{-| Get points along a spline at a given set of parameter values.

    exampleSpline
        |> CubicSpline3d.pointsAt
            (ParameterValues.steps 2)
    --> [ Point2d.fromCoordinates ( 1, 1, 1 )
    --> , Point2d.fromCoordinates ( 2.75, 2, 1.25 )
    --> , Point2d.fromCoordinates ( 3, 3, 3 )
    --> ]

-}
pointsAt : ParameterValues -> CubicSpline3d -> List Point3d
pointsAt parameterValues spline =
    ParameterValues.map (pointOn spline) parameterValues


{-| Attempt to construct a function for evaluating points and tangent directions
along a spline; if

    CubicSpline3d.sampler spline

returns `Just sampleAt`, then

    sampleAt ParameterValue.zero

will return the start point of the spline along with the tangent direction at
the start point,

    sampleAt ParameterValue.one

will return the end point and the corresponding tangent tangent direction, etc.

If the spline is degenerate (all control points are identical), returns
`Nothing`.

-}
sampler : CubicSpline3d -> Maybe (ParameterValue -> ( Point3d, Direction3d ))
sampler spline =
    case Vector3d.direction (thirdDerivative spline) of
        Just thirdDerivativeDirection ->
            -- Third derivative is non-zero, so if all else fails we can fall
            -- back on it to provide a tangent direction
            Just <|
                nonZeroThirdDerivativeSampler spline
                    thirdDerivativeDirection

        Nothing ->
            let
                -- Third derivative is zero, so second derivative is constant -
                -- evaluate it at an arbitrary point to get its value
                secondDerivativeVector =
                    secondDerivative spline ParameterValue.zero
            in
            case Vector3d.direction secondDerivativeVector of
                Just secondDerivativeDirection ->
                    -- Second derivative is non-zero, so if all else fails we
                    -- can fall back on it to provide a tangent direction
                    Just <|
                        nonZeroSecondDerivativeSampler spline
                            secondDerivativeDirection

                Nothing ->
                    let
                        -- Second and third derivatives are zero, so first
                        -- derivative is constant - evaluate it at an arbitrary
                        -- point to get its value
                        firstDerivativeVector =
                            firstDerivative spline ParameterValue.zero
                    in
                    case Vector3d.direction firstDerivativeVector of
                        Just firstDerivativeDirection ->
                            Just <|
                                nonZeroFirstDerivativeSampler spline
                                    firstDerivativeDirection

                        Nothing ->
                            Nothing


nonZeroFirstDerivativeSampler : CubicSpline3d -> Direction3d -> ParameterValue -> ( Point3d, Direction3d )
nonZeroFirstDerivativeSampler spline firstDerivativeDirection =
    -- Tangent direction is always equal to the (constant) first derivative
    -- direction
    \parameterValue ->
        ( pointOn spline parameterValue
        , firstDerivativeDirection
        )


nonZeroSecondDerivativeSampler : CubicSpline3d -> Direction3d -> ParameterValue -> ( Point3d, Direction3d )
nonZeroSecondDerivativeSampler spline secondDerivativeDirection =
    \parameterValue ->
        let
            point =
                pointOn spline parameterValue

            firstDerivativeVector =
                firstDerivative spline parameterValue
        in
        case Vector3d.direction firstDerivativeVector of
            Just firstDerivativeDirection ->
                -- First derivative is non-zero, so use its direction as the
                -- tangent direction
                ( point, firstDerivativeDirection )

            Nothing ->
                -- Zero first derivative and non-zero second derivative mean we
                -- have reached a reversal point, where the tangent direction
                -- just afterwards is equal to the second derivative direction
                -- and the tangent direction just before is equal to the flipped
                -- second derivative direction. If we happen to be right at the
                -- end of the spline, choose the tangent direction just before
                -- the end (instead of one that is off the spline!), otherwise
                -- choose the tangent direction just after the point (necessary
                -- for t = 0, arbitrary for all other points).
                if parameterValue == ParameterValue.one then
                    ( point, Direction3d.flip secondDerivativeDirection )
                else
                    ( point, secondDerivativeDirection )


nonZeroThirdDerivativeSampler : CubicSpline3d -> Direction3d -> ParameterValue -> ( Point3d, Direction3d )
nonZeroThirdDerivativeSampler spline thirdDerivativeDirection =
    \parameterValue ->
        let
            point =
                pointOn spline parameterValue

            firstDerivativeVector =
                firstDerivative spline parameterValue
        in
        case Vector3d.direction firstDerivativeVector of
            Just firstDerivativeDirection ->
                -- First derivative is non-zero, so just use its
                -- direction as the tangent direction (normal case)
                ( point, firstDerivativeDirection )

            Nothing ->
                let
                    secondDerivativeVector =
                        secondDerivative spline parameterValue
                in
                case Vector3d.direction secondDerivativeVector of
                    Just secondDerivativeDirection ->
                        -- Zero first derivative and non-zero second
                        -- derivative mean we have reached a
                        -- reversal point, as above in
                        -- nonZeroSecondDerivativeSampler
                        if parameterValue == ParameterValue.one then
                            ( point
                            , Direction3d.flip secondDerivativeDirection
                            )
                        else
                            ( point, secondDerivativeDirection )

                    Nothing ->
                        -- First and second derivatives are zero, so
                        -- fall back to the third dervative
                        -- direction
                        ( point, thirdDerivativeDirection )


{-| Find the positions and tangent directions at several points along a spline,
given by a set of parameter values.

    exampleSpline
        |> CubicSpline3d.samplesAt
            (ParameterValues.steps 2)
    --> [ ( Point2d.fromCoordinates ( 1, 1 )
    -->   , Direction2d.fromAngle (degrees 56.31)
    -->   )
    --> , ( Point2d.fromCoordinates ( 4, 2.5 )
    -->   , Direction2d.x
    -->   )
    --> , ( Point2d.fromCoordinates ( 7, 4 )
    -->   , Direction2d.fromAngle (degrees 56.31)
    -->   )
    --> ]

If the given spline is degenerate (all control points are identical), it has no
tangent directions and so the result will always be an empty list.

-}
samplesAt : ParameterValues -> CubicSpline3d -> List ( Point3d, Direction3d )
samplesAt parameterValues spline =
    case sampler spline of
        Just sampleAt ->
            ParameterValues.map sampleAt parameterValues

        Nothing ->
            []


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    CubicSpline3d.reverse exampleSpline
    --> CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 3, 3, 3 )
    -->     , Point3d.fromCoordinates ( 3, 3, 1 )
    -->     , Point3d.fromCoordinates ( 3, 1, 1 )
    -->     , Point3d.fromCoordinates ( 1, 1, 1 )
    -->     )

-}
reverse : CubicSpline3d -> CubicSpline3d
reverse spline =
    with
        { startPoint = endPoint spline
        , startControlPoint = endControlPoint spline
        , endControlPoint = startControlPoint spline
        , endPoint = startPoint spline
        }


{-| Scale a spline about the given center point by the given scale.

    CubicSpline3d.scaleAbout Point3d.origin 2 exampleSpline
    --> CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 2, 2, 2 )
    -->     , Point3d.fromCoordinates ( 6, 2, 2 )
    -->     , Point3d.fromCoordinates ( 6, 6, 2 )
    -->     , Point3d.fromCoordinates ( 6, 6, 6 )
    -->     )

-}
scaleAbout : Point3d -> Float -> CubicSpline3d -> CubicSpline3d
scaleAbout point scale =
    mapControlPoints (Point3d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given axis by a given angle (in
radians).

    exampleSpline
        |> CubicSpline3d.rotateAround Axis3d.z (degrees 90)
    --> CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( -1, 1, 1 )
    -->     , Point3d.fromCoordinates ( -1, 3, 1 )
    -->     , Point3d.fromCoordinates ( -3, 3, 1 )
    -->     , Point3d.fromCoordinates ( -3, 3, 3 )
    -->     )

-}
rotateAround : Axis3d -> Float -> CubicSpline3d -> CubicSpline3d
rotateAround axis angle =
    mapControlPoints (Point3d.rotateAround axis angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 3, 1 )

    CubicSpline3d.translateBy displacement exampleSpline
    --> CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 3, 4, 2 )
    -->     , Point3d.fromCoordinates ( 5, 4, 2 )
    -->     , Point3d.fromCoordinates ( 5, 6, 2 )
    -->     , Point3d.fromCoordinates ( 5, 6, 4 )
    -->     )

-}
translateBy : Vector3d -> CubicSpline3d -> CubicSpline3d
translateBy displacement =
    mapControlPoints (Point3d.translateBy displacement)


{-| Translate a spline in a given direction by a given distance;

    CubicSpline3d.translateIn direction distance

is equivalent to

    CubicSpline3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> CubicSpline3d -> CubicSpline3d
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across a plane.

    CubicSpline3d.mirrorAcross Plane3d.xy exampleSpline
    --> CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 1, 1, -1 )
    -->     , Point3d.fromCoordinates ( 3, 1, -1 )
    -->     , Point3d.fromCoordinates ( 3, 3, -1 )
    -->     , Point3d.fromCoordinates ( 3, 3, -3 )
    -->     )

-}
mirrorAcross : Plane3d -> CubicSpline3d -> CubicSpline3d
mirrorAcross plane =
    mapControlPoints (Point3d.mirrorAcross plane)


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a spline onto a plane.

    CubicSpline3d.projectOnto Plane3d.xy exampleSpline
    --> CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 1, 1, 0 )
    -->     , Point3d.fromCoordinates ( 3, 1, 0 )
    -->     , Point3d.fromCoordinates ( 3, 3, 0 )
    -->     , Point3d.fromCoordinates ( 3, 3, 0 )
    -->     )

-}
projectOnto : Plane3d -> CubicSpline3d -> CubicSpline3d
projectOnto plane =
    mapControlPoints (Point3d.projectOnto plane)


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    CubicSpline3d.relativeTo localFrame exampleSpline
    --> CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 0, -1, -2 )
    -->     , Point3d.fromCoordinates ( 2, -1, -2 )
    -->     , Point3d.fromCoordinates ( 2, 1, -2 )
    -->     , Point3d.fromCoordinates ( 2, 1, 0 )
    -->     )

-}
relativeTo : Frame3d -> CubicSpline3d -> CubicSpline3d
relativeTo frame =
    mapControlPoints (Point3d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    CubicSpline3d.placeIn localFrame exampleSpline
    --> CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 2, 3, 4 )
    -->     , Point3d.fromCoordinates ( 4, 3, 4 )
    -->     , Point3d.fromCoordinates ( 4, 5, 4 )
    -->     , Point3d.fromCoordinates ( 4, 5, 6 )
    -->     )

-}
placeIn : Frame3d -> CubicSpline3d -> CubicSpline3d
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| Project a spline into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the spline onto the plane and then expresses the projected spline in 2D
sketch coordinates.

    exampleSpline
        |> CubicSpline3d.projectInto SketchPlane3d.yz
    --> CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 1, 1 )
    -->     , Point2d.fromCoordinates ( 1, 1 )
    -->     , Point2d.fromCoordinates ( 3, 1 )
    -->     , Point2d.fromCoordinates ( 3, 3 )
    -->     )

-}
projectInto : SketchPlane3d -> CubicSpline3d -> CubicSpline2d
projectInto sketchPlane spline =
    CubicSpline2d.with
        { startPoint =
            Point3d.projectInto sketchPlane (startPoint spline)
        , startControlPoint =
            Point3d.projectInto sketchPlane (startControlPoint spline)
        , endControlPoint =
            Point3d.projectInto sketchPlane (endControlPoint spline)
        , endPoint =
            Point3d.projectInto sketchPlane (endPoint spline)
        }


mapControlPoints : (Point3d -> Point3d) -> CubicSpline3d -> CubicSpline3d
mapControlPoints function spline =
    with
        { startPoint = function (startPoint spline)
        , startControlPoint = function (startControlPoint spline)
        , endControlPoint = function (endControlPoint spline)
        , endPoint = function (endPoint spline)
        }


{-| Split a spline into two roughly equal halves.

    CubicSpline3d.bisect exampleSpline
    --> ( CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , Point3d.fromCoordinates ( 2, 1, 1 )
    -->     , Point3d.fromCoordinates ( 2.5, 1.5, 1 )
    -->     , Point3d.fromCoordinates ( 2.75, 2, 1.25 )
    -->     )
    --> , CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 2.75, 2, 1.25 )
    -->     , Point3d.fromCoordinates ( 3, 2.5, 1.5 )
    -->     , Point3d.fromCoordinates ( 3, 3, 2 )
    -->     , Point3d.fromCoordinates ( 3, 3, 3 )
    -->     )
    --> )

Equivalent to `CubicSpline3d.splitAt ParameterValue.oneHalf`.

-}
bisect : CubicSpline3d -> ( CubicSpline3d, CubicSpline3d )
bisect =
    splitAt ParameterValue.oneHalf


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.

    parameterValue =
        ParameterValue.clamped 0.75

    CubicSpline3d.splitAt parameterValue exampleSpline
    --> ( CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , Point3d.fromCoordinates ( 2.5, 1, 1 )
    -->     , Point3d.fromCoordinates ( 2.88, 2.13, 1 )
    -->     , Point3d.fromCoordinates ( 2.97, 2.69, 1.84 )
    -->     )
    --> , CubicSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 2.97, 2.69, 1.84 )
    -->     , Point3d.fromCoordinates ( 3, 2.88, 2.13 )
    -->     , Point3d.fromCoordinates ( 3, 3, 2.5 )
    -->     , Point3d.fromCoordinates ( 3, 3, 3 )
    -->     )
    --> )

-}
splitAt : ParameterValue -> CubicSpline3d -> ( CubicSpline3d, CubicSpline3d )
splitAt parameterValue spline =
    let
        t =
            ParameterValue.toFloat parameterValue

        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t

        q3 =
            Point3d.interpolateFrom p3 p4 t

        r1 =
            Point3d.interpolateFrom q1 q2 t

        r2 =
            Point3d.interpolateFrom q2 q3 t

        s =
            Point3d.interpolateFrom r1 r2 t
    in
    ( with
        { startPoint = p1
        , startControlPoint = q1
        , endControlPoint = r1
        , endPoint = s
        }
    , with
        { startPoint = s
        , startControlPoint = r2
        , endControlPoint = q3
        , endPoint = p4
        }
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized CubicSpline3d ArcLengthParameterization


{-| Build an arc length parameterization of the given spline:

    parameterizedSpline =
        CubicSpline3d.arcLengthParameterized
            (Accuracy.maxError 1.0e-4)
            exampleSpline

The accuracy of the parameterization is controlled by the first argument; this
affects the accuracy of results returned from functions such as `arcLength` and
`pointAlong`.

-}
arcLengthParameterized : Accuracy -> CubicSpline3d -> ArcLengthParameterized
arcLengthParameterized accuracy spline =
    let
        parameterization =
            ArcLengthParameterization.build
                { accuracy = accuracy
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude =
                    maxSecondDerivativeMagnitude spline
                }
    in
    ArcLengthParameterized spline parameterization


{-| Find the total arc length of a spline:

    arcLength =
        CubicSpline2d.arcLength parameterizedSpline

    arcLength
    --> 4.3303

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized -> Float
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


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
    --> Point3d.fromCoordinates ( 2.1563, 1.3125, 1.0313 )

If the given arc length is less than zero or greater than the arc length of the
spline, `Nothing` is returned.

-}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point3d
pointAlong (ArcLengthParameterized spline parameterization) distance =
    parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> Maybe.map (pointOn spline)


{-| Try to get the point and tangent direction along a spline at a given arc
length. To get the point and tangent direction a quarter of the way along
`exampleSpline`:

    CubicSpline3d.sampleAlong parameterizedSpline
        (0.25 * arcLength)

If the given arc length is less than zero or greater than the arc length of the
spline (or if the derivative of the spline happens to be exactly zero at the
given arc length), `Nothing` is returned.

-}
sampleAlong : ArcLengthParameterized -> Float -> Maybe ( Point3d, Direction3d )
sampleAlong (ArcLengthParameterized spline parameterization) =
    case sampler spline of
        Just toSample ->
            \distance ->
                parameterization
                    |> ArcLengthParameterization.arcLengthToParameterValue
                        distance
                    |> Maybe.map toSample

        Nothing ->
            always Nothing


{-| -}
arcLengthParameterization : ArcLengthParameterized -> ArcLengthParameterization
arcLengthParameterization (ArcLengthParameterized _ parameterization) =
    parameterization


{-| -}
underlyingSpline : ArcLengthParameterized -> CubicSpline3d
underlyingSpline (ArcLengthParameterized spline _) =
    spline


{-| Get the first derivative of a spline at a given parameter value.

    CubicSpline3d.derivative exampleSpline
        ParameterValue.zero
    --> Vector3d.fromComponents ( 6, 0, 0 )

    CubicSpline3d.derivative exampleSpline
        ParameterValue.oneHalf
    --> Vector3d.fromComponents ( 1.5, 3, 1.5 )

    CubicSpline3d.derivative exampleSpline
        ParameterValue.one
    --> Vector3d.fromComponents ( 0, 0, 6 )

-}
firstDerivative : CubicSpline3d -> ParameterValue -> Vector3d
firstDerivative spline parameterValue =
    let
        t =
            ParameterValue.toFloat parameterValue

        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        ( x1, y1, z1 ) =
            Point3d.coordinates p1

        ( x2, y2, z2 ) =
            Point3d.coordinates p2

        ( x3, y3, z3 ) =
            Point3d.coordinates p3

        ( x4, y4, z4 ) =
            Point3d.coordinates p4

        vx1 =
            x2 - x1

        vy1 =
            y2 - y1

        vz1 =
            z2 - z1

        vx2 =
            x3 - x2

        vy2 =
            y3 - y2

        vz2 =
            z3 - z2

        vx3 =
            x4 - x3

        vy3 =
            y4 - y3

        vz3 =
            z4 - z3
    in
    if t <= 0.5 then
        let
            wx1 =
                vx1 + t * (vx2 - vx1)

            wy1 =
                vy1 + t * (vy2 - vy1)

            wz1 =
                vz1 + t * (vz2 - vz1)

            wx2 =
                vx2 + t * (vx3 - vx2)

            wy2 =
                vy2 + t * (vy3 - vy2)

            wz2 =
                vz2 + t * (vz3 - vz2)
        in
        Vector3d.fromComponents
            ( 3 * (wx1 + t * (wx2 - wx1))
            , 3 * (wy1 + t * (wy2 - wy1))
            , 3 * (wz1 + t * (wz2 - wz1))
            )
    else
        let
            u =
                1 - t

            wx1 =
                vx2 + u * (vx1 - vx2)

            wy1 =
                vy2 + u * (vy1 - vy2)

            wz1 =
                vz2 + u * (vz1 - vz2)

            wx2 =
                vx3 + u * (vx2 - vx3)

            wy2 =
                vy3 + u * (vy2 - vy3)

            wz2 =
                vz3 + u * (vz2 - vz3)
        in
        Vector3d.fromComponents
            ( 3 * (wx2 + u * (wx1 - wx2))
            , 3 * (wy2 + u * (wy1 - wy2))
            , 3 * (wz2 + u * (wz1 - wz2))
            )


{-| Evaluate the first derivative of a spline at a range of parameter values.

    exampleSpline
        |> CubicSpline3d.firstDerivativesAt
            (ParameterValues.steps 2)
    --> [ Vector3d.fromComponents ( 6, 0, 0 )
    --> , Vector3d.fromComponents ( 1.5, 3, 1.5 )
    --> , Vector3d.fromComponents ( 0, 0, 6 )
    --> ]

-}
firstDerivativesAt : ParameterValues -> CubicSpline3d -> List Vector3d
firstDerivativesAt parameterValues spline =
    ParameterValues.map (firstDerivative spline) parameterValues


{-| Get the second derivative value at a point along a spline, based on a
parameter that ranges from 0 to 1. A parameter value of 0 corresponds to the
start of the spline and a value of 1 corresponds to the end.

    CubicSpline3d.secondDerivative exampleSpline
        ParameterValue.zero
    --> Vector3d.fromComponents ( -12, 12, 0 )

    CubicSpline3d.secondDerivative exampleSpline
        ParameterValue.oneHalf
    --> Vector3d.fromComponents ( -6, 0, 6 )

    CubicSpline3d.secondDerivative exampleSpline
        ParameterValue.one
    --> Vector3d.fromComponents ( 0, -12, 12 )

-}
secondDerivative : CubicSpline3d -> ParameterValue -> Vector3d
secondDerivative spline parameterValue =
    let
        t =
            ParameterValue.toFloat parameterValue

        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        u1 =
            Vector3d.from p1 p2

        u2 =
            Vector3d.from p2 p3

        u3 =
            Vector3d.from p3 p4

        v1 =
            Vector3d.difference u2 u1

        v2 =
            Vector3d.difference u3 u2
    in
    Vector3d.scaleBy 6 (Vector3d.interpolateFrom v1 v2 t)


{-| Evaluate the second derivative of a spline at a range of parameter values.

    exampleSpline
        |> CubicSpline3d.secondDerivativesAt
            (ParameterValues.steps 2)
    --> [ Vector3d.fromComponents ( -12, 12, 0 )
    --> , Vector3d.fromComponents ( -6, 0, 6 )
    --> , Vector3d.fromComponents ( 0, -12, 12 )
    --> ]

-}
secondDerivativesAt : ParameterValues -> CubicSpline3d -> List Vector3d
secondDerivativesAt parameterValues spline =
    ParameterValues.map (secondDerivative spline) parameterValues


{-| Get the third derivative of a spline (for a cubic spline, this is a
constant).
-}
thirdDerivative : CubicSpline3d -> Vector3d
thirdDerivative spline =
    let
        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        u1 =
            Vector3d.from p1 p2

        u2 =
            Vector3d.from p2 p3

        u3 =
            Vector3d.from p3 p4

        v1 =
            Vector3d.difference u2 u1

        v2 =
            Vector3d.difference u3 u2
    in
    Vector3d.scaleBy 6 (Vector3d.difference v2 v1)


{-| Find a conservative upper bound on the magnitude of the second derivative of
a spline. This can be useful when determining error bounds for various kinds of
linear approximations.
-}
maxSecondDerivativeMagnitude : CubicSpline3d -> Float
maxSecondDerivativeMagnitude spline =
    let
        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        u1 =
            Vector3d.from p1 p2

        u2 =
            Vector3d.from p2 p3

        u3 =
            Vector3d.from p3 p4

        v1 =
            Vector3d.difference u2 u1

        v2 =
            Vector3d.difference u3 u2
    in
    6 * max (Vector3d.length v1) (Vector3d.length v2)


derivativeMagnitude : CubicSpline3d -> Float -> Float
derivativeMagnitude spline =
    let
        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        ( x1, y1, z1 ) =
            Point3d.coordinates p1

        ( x2, y2, z2 ) =
            Point3d.coordinates p2

        ( x3, y3, z3 ) =
            Point3d.coordinates p3

        ( x4, y4, z4 ) =
            Point3d.coordinates p4

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
    \t ->
        let
            x13 =
                x12 + t * x123

            y13 =
                y12 + t * y123

            z13 =
                z12 + t * z123

            x24 =
                x23 + t * x234

            y24 =
                y23 + t * y234

            z24 =
                z23 + t * z234

            x14 =
                x13 + t * (x24 - x13)

            y14 =
                y13 + t * (y24 - y13)

            z14 =
                z13 + t * (z24 - z13)
        in
        3 * sqrt (x14 * x14 + y14 * y14 + z14 * z14)
