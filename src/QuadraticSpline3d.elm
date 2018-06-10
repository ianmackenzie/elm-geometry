module QuadraticSpline3d
    exposing
        ( ArcLengthParameterized
        , QuadraticSpline3d
        , arcLength
        , arcLengthParameterization
        , arcLengthParameterized
        , bisect
        , boundingBox
        , controlPoint
        , endDerivative
        , endPoint
        , firstDerivative
        , firstDerivativesAt
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
        , splitAt
        , startDerivative
        , startPoint
        , translateBy
        , translateIn
        , underlyingSpline
        , with
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/QuadraticSpline3d/icon.svg" alt="QuadraticSpline3d" width="160">

A `QuadraticSpline3d` is a quadratic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 3D defined by a start point, control point and end point. This module
contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs QuadraticSpline3d


# Constructors

@docs with, on


# Properties

@docs startPoint, endPoint, controlPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn, pointsAt, sampler, samplesAt


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn


# Sketch planes

@docs projectInto


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `QuadraticSpline3d`. If you need to do something fancy, you can
extract these two values separately.

@docs arcLengthParameterization, underlyingSpline


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, firstDerivativesAt, secondDerivative

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Geometry.ParameterValue as ParameterValue exposing (ParameterValue)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias QuadraticSpline3d =
    Types.QuadraticSpline3d


{-| Construct a spline from its start point, control point and end point:

    exampleSpline =
        QuadraticSpline3d.with
            { startPoint = Point3d.fromCoordinates ( 1, 1, 1 )
            , controlPoint = Point3d.fromCoordinates ( 3, 2, 1 )
            , endPoint = Point3d.fromCoordinates ( 3, 3, 3 )
            }

-}
with : { startPoint : Point3d, controlPoint : Point3d, endPoint : Point3d } -> QuadraticSpline3d
with =
    Types.QuadraticSpline3d


{-| Construct a 3D spline lying _on_ a sketch plane by providing a 2D spline
specified in XY coordinates _within_ the sketch plane.

    QuadraticSpline3d.on SketchPlane3d.xz <|
        QuadraticSpline2d.fromControlPoints
            ( Point2d.fromCoordinates ( 1, 1 )
            , Point2d.fromCoordinates ( 3, 4 )
            , Point2d.fromCoordinates ( 5, 1 )
            )
    --> QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 1, 0, 1 )
    -->     , Point3d.fromCoordinates ( 3, 0, 4 )
    -->     , Point3d.fromCoordinates ( 5, 0, 1 )
    -->     )

-}
on : SketchPlane3d -> QuadraticSpline2d -> QuadraticSpline3d
on sketchPlane spline2d =
    with
        { startPoint =
            Point3d.on sketchPlane (QuadraticSpline2d.startPoint spline2d)
        , controlPoint =
            Point3d.on sketchPlane (QuadraticSpline2d.controlPoint spline2d)
        , endPoint =
            Point3d.on sketchPlane (QuadraticSpline2d.endPoint spline2d)
        }


{-| Get the start point of a spline.

    QuadraticSpline3d.startPoint exampleSpline
    --> Point3d.fromCoordinates ( 1, 1, 1 )

-}
startPoint : QuadraticSpline3d -> Point3d
startPoint (Types.QuadraticSpline3d spline) =
    spline.startPoint


{-| Get the end point of a spline. This is equal to the spline's last control
point.

    QuadraticSpline3d.endPoint exampleSpline
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
endPoint : QuadraticSpline3d -> Point3d
endPoint (Types.QuadraticSpline3d spline) =
    spline.endPoint


{-| Get the control point of a spline.

    QuadraticSpline3d.controlPoint exampleSpline
    --> Point3d.fromCoordinates ( 3, 2, 1 )

-}
controlPoint : QuadraticSpline3d -> Point3d
controlPoint (Types.QuadraticSpline3d spline) =
    spline.controlPoint


{-| Get the start derivative of a spline. This is equal to twice the vector from
the spline's first control point to its second.

    QuadraticSpline3d.startDerivative exampleSpline
    --> Vector3d.fromComponents ( 4, 2, 0 )

-}
startDerivative : QuadraticSpline3d -> Vector3d
startDerivative spline =
    Vector3d.from (startPoint spline) (controlPoint spline)
        |> Vector3d.scaleBy 2


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline3d.endDerivative exampleSpline
    --> Vector3d.fromComponents ( 0, 2, 4 )

-}
endDerivative : QuadraticSpline3d -> Vector3d
endDerivative spline =
    Vector3d.from (controlPoint spline) (endPoint spline)
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
boundingBox : QuadraticSpline3d -> BoundingBox3d
boundingBox spline =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates (startPoint spline)

        ( x2, y2, z2 ) =
            Point3d.coordinates (controlPoint spline)

        ( x3, y3, z3 ) =
            Point3d.coordinates (endPoint spline)
    in
    BoundingBox3d.fromExtrema
        { minX = min x1 (min x2 x3)
        , maxX = max x1 (max x2 x3)
        , minY = min y1 (min y2 y3)
        , maxY = max y1 (max y2 y3)
        , minZ = min z1 (min z2 z3)
        , maxZ = max z1 (max z2 z3)
        }


{-| Get a point along a spline, based on a parameter that ranges from 0 to 1. A
parameter value of 0 corresponds to the start point of the spline and a value of
1 corresponds to the end point.

    QuadraticSpline3d.pointOn exampleSpline 0
    --> Point3d.fromCoordinates ( 1, 1, 1 )

    QuadraticSpline3d.pointOn exampleSpline 0.5
    --> Point3d.fromCoordinates ( 2.5, 2, 1.5 )

    QuadraticSpline3d.pointOn exampleSpline 1
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
pointOn : QuadraticSpline3d -> ParameterValue -> Point3d
pointOn spline parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t
    in
    Point3d.interpolateFrom q1 q2 t


{-| Convenient shorthand for evaluating multiple points;

    QuadraticSpline3d.pointsOn spline parameterValues

is equivalent to

    List.map (QuadraticSpline3d.pointOn spline) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
pointsAt : List ParameterValue -> QuadraticSpline3d -> List Point3d
pointsAt parameterValues spline =
    List.map (pointOn spline) parameterValues


{-| Get the first derivative of a spline at a given parameter value.

    QuadraticSpline3d.derivative exampleSpline
        ParameterValue.zero
    --> Vector3d.fromComponents ( 4, 2, 0 )

    QuadraticSpline3d.derivative exampleSpline
        ParameterValue.oneHalf
    --> Vector3d.fromComponents ( 2, 2, 2 )

    QuadraticSpline3d.derivative exampleSpline
        ParameterValue.one
    --> Vector3d.fromComponents ( 0, 2, 4 )

Note that the derivative interpolates linearly from end to end.

-}
firstDerivative : QuadraticSpline3d -> ParameterValue -> Vector3d
firstDerivative spline parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.interpolateFrom v1 v2 t |> Vector3d.scaleBy 2


{-| Convenient shorthand for evaluating multiple derivatives;

    QuadraticSpline3d.derivatives spline parameterValues

is equivalent to

    List.map (QuadraticSpline3d.derivative spline) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
firstDerivativesAt : List ParameterValue -> QuadraticSpline3d -> List Vector3d
firstDerivativesAt parameterValues spline =
    List.map (firstDerivative spline) parameterValues


{-| Find the magnitude of the derivative to a spline at a particular parameter
value;

    QuadraticSpline3d.derivativeMagnitude spline t

is equivalent to

    Vector3d.length (QuadraticSpline3d.derivative spline t)

but more efficient since it avoids any intermediate `Vector3d` allocation.

-}
derivativeMagnitude : QuadraticSpline3d -> Float -> Float
derivativeMagnitude spline =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates (startPoint spline)

        ( x2, y2, z2 ) =
            Point3d.coordinates (controlPoint spline)

        ( x3, y3, z3 ) =
            Point3d.coordinates (endPoint spline)

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
    \t ->
        let
            x13 =
                x12 + t * x123

            y13 =
                y12 + t * y123

            z13 =
                z12 + t * z123
        in
        2 * sqrt (x13 * x13 + y13 * y13 + z13 * z13)


{-| Sample a spline at a given parameter value to get both the position and
derivative vector at that parameter value;

    QuadraticSpline3d.sample spline t

is equivalent to

    ( QuadraticSpline3d.pointOn spline t
    , QuadraticSpline3d.derivative spline t
    )

but is more efficient.

-}
sampler : QuadraticSpline3d -> Maybe (ParameterValue -> ( Point3d, Direction3d ))
sampler spline =
    case Vector3d.direction (secondDerivative spline) of
        Just secondDerivativeDirection ->
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


nonZeroFirstDerivativeSampler : QuadraticSpline3d -> Direction3d -> ParameterValue -> ( Point3d, Direction3d )
nonZeroFirstDerivativeSampler spline firstDerivativeDirection =
    -- Tangent direction is always equal to the (constant) first derivative
    -- direction
    \parameterValue ->
        ( pointOn spline parameterValue
        , firstDerivativeDirection
        )


nonZeroSecondDerivativeSampler : QuadraticSpline3d -> Direction3d -> ParameterValue -> ( Point3d, Direction3d )
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


{-| -}
samplesAt : List ParameterValue -> QuadraticSpline3d -> List ( Point3d, Direction3d )
samplesAt parameterValues spline =
    case sampler spline of
        Just sampler_ ->
            List.map sampler_ parameterValues

        Nothing ->
            []


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    QuadraticSpline3d.reverse exampleSpline
    --> QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 3, 3, 3 )
    -->     , Point3d.fromCoordinates ( 3, 2, 1 )
    -->     , Point3d.fromCoordinates ( 1, 1, 1 )
    -->     )

-}
reverse : QuadraticSpline3d -> QuadraticSpline3d
reverse spline =
    with
        { startPoint = endPoint spline
        , controlPoint = controlPoint spline
        , endPoint = startPoint spline
        }


{-| Scale a spline about the given center point by the given scale.

    exampleSpline
        |> QuadraticSpline3d.scaleAbout Point3d.origin 2
    --> QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 2, 2, 2 )
    -->     , Point3d.fromCoordinates ( 6, 4, 2 )
    -->     , Point3d.fromCoordinates ( 6, 6, 6 )
    -->     )

-}
scaleAbout : Point3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
scaleAbout point scale =
    mapControlPoints (Point3d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given axis by a given angle (in
radians).

    exampleSpline
        |> QuadraticSpline3d.rotateAround Axis3d.z
            (degrees 90)
    --> QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( -1, 1, 1 )
    -->     , Point3d.fromCoordinates ( -2, 3, 1 )
    -->     , Point3d.fromCoordinates ( -3, 3, 3 )
    -->     )

-}
rotateAround : Axis3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
rotateAround axis angle =
    mapControlPoints (Point3d.rotateAround axis angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 3, 1 )

    exampleSpline
        |> QuadraticSpline3d.translateBy displacement
    --> QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 3, 4, 2 )
    -->     , Point3d.fromCoordinates ( 5, 5, 2 )
    -->     , Point3d.fromCoordinates ( 5, 6, 4 )
    -->     )

-}
translateBy : Vector3d -> QuadraticSpline3d -> QuadraticSpline3d
translateBy displacement =
    mapControlPoints (Point3d.translateBy displacement)


{-| Translate an arc in a given direction by a given distance;

    QuadraticSpline3d.translateIn direction distance

is equivalent to

    QuadraticSpline3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across a plane.

    QuadraticSpline3d.mirrorAcross Plane3d.xy exampleSpline
    --> QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 1, 1, -1 )
    -->     , Point3d.fromCoordinates ( 3, 2, -1 )
    -->     , Point3d.fromCoordinates ( 3, 3, -3 )
    -->     )

-}
mirrorAcross : Plane3d -> QuadraticSpline3d -> QuadraticSpline3d
mirrorAcross plane =
    mapControlPoints (Point3d.mirrorAcross plane)


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a spline onto a plane.

    QuadraticSpline3d.projectOnto Plane3d.xy exampleSpline
    --> QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 1, 1, 0 )
    -->     , Point3d.fromCoordinates ( 3, 2, 0 )
    -->     , Point3d.fromCoordinates ( 3, 3, 0 )
    -->     )

-}
projectOnto : Plane3d -> QuadraticSpline3d -> QuadraticSpline3d
projectOnto plane =
    mapControlPoints (Point3d.projectOnto plane)


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    QuadraticSpline3d.relativeTo localFrame exampleSpline
    --> QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 0, -1, -2 )
    -->     , Point3d.fromCoordinates ( 2, 0, -2 )
    -->     , Point3d.fromCoordinates ( 2, 1, 0 )
    -->     )

-}
relativeTo : Frame3d -> QuadraticSpline3d -> QuadraticSpline3d
relativeTo frame =
    mapControlPoints (Point3d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    QuadraticSpline3d.placeIn localFrame exampleSpline
    --> QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 2, 3, 4 )
    -->     , Point3d.fromCoordinates ( 4, 4, 4 )
    -->     , Point3d.fromCoordinates ( 4, 5, 6 )
    -->     )

-}
placeIn : Frame3d -> QuadraticSpline3d -> QuadraticSpline3d
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| Project a spline into a given sketch plane. Conceptually, finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the spline onto the plane and then expresses the projected spline in 2D
sketch coordinates.

    exampleSpline
        |> QuadraticSpline3d.projectInto SketchPlane3d.yz
    --> QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 1, 1 )
    -->     , Point2d.fromCoordinates ( 2, 1 )
    -->     , Point2d.fromCoordinates ( 3, 3 )
    -->     )

-}
projectInto : SketchPlane3d -> QuadraticSpline3d -> QuadraticSpline2d
projectInto sketchPlane spline =
    QuadraticSpline2d.with
        { startPoint = Point3d.projectInto sketchPlane (startPoint spline)
        , controlPoint = Point3d.projectInto sketchPlane (controlPoint spline)
        , endPoint = Point3d.projectInto sketchPlane (endPoint spline)
        }


mapControlPoints : (Point3d -> Point3d) -> QuadraticSpline3d -> QuadraticSpline3d
mapControlPoints function spline =
    with
        { startPoint = function (startPoint spline)
        , controlPoint = function (controlPoint spline)
        , endPoint = function (endPoint spline)
        }


{-| Split a spline into two roughly equal halves.

    QuadraticSpline3d.bisect exampleSpline
    --> ( QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , Point3d.fromCoordinates ( 2, 2.5 )
    -->     , Point3d.fromCoordinates ( 3, 2.5 )
    -->     )
    --> , QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 3, 2.5 )
    -->     , Point3d.fromCoordinates ( 4, 2.5 )
    -->     , Point3d.fromCoordinates ( 3, 3, 3 )
    -->     )
    --> )

Equivalent to `QuadraticSpline3d.splitAt ParameterValue.oneHalf`.

-}
bisect : QuadraticSpline3d -> ( QuadraticSpline3d, QuadraticSpline3d )
bisect =
    splitAt ParameterValue.oneHalf


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.

    parameterValue =
        ParameterValue.clamped 0.75

    QuadraticSpline3d.splitAt parameterValue exampleSpline
    --> ( QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , Point3d.fromCoordinates ( 2, 1.5, 1 )
    -->     , Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    -->     )
    --> , QuadraticSpline3d.fromControlPoints
    -->     ( Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    -->     , Point3d.fromCoordinates ( 3, 2.5, 2 )
    -->     , Point3d.fromCoordinates ( 3, 3, 3 )
    -->     )
    --> )

-}
splitAt : ParameterValue -> QuadraticSpline3d -> ( QuadraticSpline3d, QuadraticSpline3d )
splitAt parameterValue spline =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t

        r =
            Point3d.interpolateFrom q1 q2 t
    in
    ( with { startPoint = p1, controlPoint = q1, endPoint = r }
    , with { startPoint = r, controlPoint = q2, endPoint = p3 }
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized QuadraticSpline3d ArcLengthParameterization


{-| Build an arc length parameterization of the given spline, with a given
accuracy. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the specified maximum
error.

    parameterizedSpline =
        exampleSpline
            |> QuadraticSpline3d.arcLengthParameterized
                { maxError = 1.0e-4 }

-}
arcLengthParameterized : { maxError : Float } -> QuadraticSpline3d -> ArcLengthParameterized
arcLengthParameterized { maxError } spline =
    let
        maxSecondDerivativeMagnitude =
            Vector3d.length (secondDerivative spline)

        parameterization =
            ArcLengthParameterization.build
                { maxError = maxError
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude = maxSecondDerivativeMagnitude
                }
    in
    ArcLengthParameterized spline parameterization


{-| Find the total arc length of a spline.

    QuadraticSpline3d.arcLength parameterizedSpline
    --> 3.8175

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized -> Float
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
spline, `Nothing` is returned.

-}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point3d
pointAlong (ArcLengthParameterized spline parameterization) distance =
    parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> Maybe.map (pointOn spline)


{-| Try to get the tangent direction along a spline at a given arc length. To
get the tangent direction a quarter of the way along `exampleSpline`:

    QuadraticSpline3d.sampleAlong parameterizedSpline
        (arcLength / 4)
    --> Just <|
    -->     Direction3d.fromAzimuthAndElevation
    -->         (degrees 33.091)
    -->         (degrees 14.260)

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


{-| Get the original `QuadraticSpline3d` from which an `ArcLengthParameterized`
value was constructed.
-}
underlyingSpline : ArcLengthParameterized -> QuadraticSpline3d
underlyingSpline (ArcLengthParameterized spline _) =
    spline


{-| Get the second derivative of a spline (for a quadratic spline, this is a
constant).
-}
secondDerivative : QuadraticSpline3d -> Vector3d
secondDerivative spline =
    let
        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.difference v2 v1 |> Vector3d.scaleBy 2
