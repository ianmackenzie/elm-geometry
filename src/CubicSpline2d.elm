module CubicSpline2d
    exposing
        ( ArcLengthParameterized
        , CubicSpline2d
        , arcLength
        , arcLengthParameterized
        , arcLengthToParameterValue
        , bisect
        , boundingBox
        , derivative
        , derivativeMagnitude
        , derivatives
        , endControlPoint
        , endDerivative
        , endPoint
        , fromEndpoints
        , fromQuadraticSpline
        , maxSecondDerivativeMagnitude
        , mirrorAcross
        , parameterValueToArcLength
        , placeIn
        , pointAlong
        , pointOn
        , pointsOn
        , relativeTo
        , reverse
        , rotateAround
        , sample
        , samples
        , scaleAbout
        , secondDerivative
        , secondDerivatives
        , startControlPoint
        , startDerivative
        , startPoint
        , tangentAlong
        , translateBy
        , translateIn
        , underlyingSpline
        , with
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/CubicSpline2d/icon.svg" alt="CubicSpline2d" width="160">

A `CubicSpline2d` is a cubic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by four control points. This module contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs CubicSpline2d


# Constructors

@docs with, fromEndpoints, fromQuadraticSpline


# Properties

@docs startPoint, endPoint, startControlPoint, endControlPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn, pointsOn, derivative, derivatives, sample, samples


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Subdivision

@docs bisect


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentAlong, arcLengthToParameterValue, parameterValueToArcLength, underlyingSpline


# Low level

Low level functionality that you are unlikely to need to use directly.

@docs secondDerivative, secondDerivatives, derivativeMagnitude, maxSecondDerivativeMagnitude

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Accuracy exposing (Accuracy)
import Geometry.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias CubicSpline2d =
    Types.CubicSpline2d


{-| Construct a spline from its endpoints and control points:

    exampleSpline =
        CubicSpline2d.with
            { startPoint = Point2d.fromCoordinates ( 1, 1 )
            , startControlPoint = Point2d.fromCoordinates ( 3, 4 )
            , endControlPoint = Point2d.fromCoordinates ( 5, 1 )
            , endPoint = Point2d.fromCoordinates ( 7, 4 )
            }

-}
with : { startPoint : Point2d, startControlPoint : Point2d, endControlPoint : Point2d, endPoint : Point2d } -> CubicSpline2d
with =
    Types.CubicSpline2d


{-| Construct a spline from a given start point with a given start derivative,
to a given end point with a given end derivative, like so:

![Cubic spline from endpoints](https://ianmackenzie.github.io/elm-geometry/1.0.0/CubicSpline2d/from.svg)

The spline is based on a parameter that ranges from 0 to 1; as a result, in most
cases the length of each derivative vector should be roughly equal to the length
of the resulting spline.

-}
fromEndpoints : { startPoint : Point2d, startDerivative : Vector2d, endPoint : Point2d, endDerivative : Vector2d } -> CubicSpline2d
fromEndpoints arguments =
    let
        startControlPoint_ =
            arguments.startPoint
                |> Point2d.translateBy
                    (Vector2d.scaleBy (1 / 3) arguments.startDerivative)

        endControlPoint_ =
            arguments.endPoint
                |> Point2d.translateBy
                    (Vector2d.scaleBy (-1 / 3) arguments.endDerivative)
    in
    with
        { startPoint = arguments.startPoint
        , startControlPoint = startControlPoint_
        , endControlPoint = endControlPoint_
        , endPoint = arguments.endPoint
        }


{-| Convert a quadratic spline into the equivalent cubic spline (every quadratic
spline can be represented exactly as a cubic spline).

    quadraticSpline =
        QuadraticSpline2d.fromControlPoints
            ( Point2d.fromCoordinates ( 0, 0  )
            , Point2d.fromCoordinates ( 3, 0 )
            , Point2d.fromCoordinates ( 3, 3 )
            )

    CubicSpline2d.fromQuadraticSpline quadraticSpline
    --> CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 0, 0 )
    -->     , Point2d.fromCoordinates ( 2, 0 )
    -->     , Point2d.fromCoordinates ( 3, 1 )
    -->     , Point2d.fromCoordinates ( 3, 3 )
    -->     )

-}
fromQuadraticSpline : QuadraticSpline2d -> CubicSpline2d
fromQuadraticSpline quadraticSpline =
    let
        startPoint_ =
            QuadraticSpline2d.startPoint quadraticSpline

        controlPoint_ =
            QuadraticSpline2d.controlPoint quadraticSpline

        endPoint_ =
            QuadraticSpline2d.endPoint quadraticSpline

        startControlPoint_ =
            Point2d.interpolateFrom startPoint_ controlPoint_ (2 / 3)

        endControlPoint_ =
            Point2d.interpolateFrom endPoint_ controlPoint_ (2 / 3)
    in
    with
        { startPoint = startPoint_
        , startControlPoint = startControlPoint_
        , endControlPoint = endControlPoint_
        , endPoint = endPoint_
        }


{-| Get the start point of a spline.

    CubicSpline2d.startPoint exampleSpline
    --> Point2d.fromCoordinates ( 1, 1 )

-}
startPoint : CubicSpline2d -> Point2d
startPoint (Types.CubicSpline2d spline) =
    spline.startPoint


{-| Get the start control point of a spline (the control point next to the
start point).
-}
startControlPoint : CubicSpline2d -> Point2d
startControlPoint (Types.CubicSpline2d spline) =
    spline.startControlPoint


{-| Get the end control point of a spline (the control point next to the
end point).
-}
endControlPoint : CubicSpline2d -> Point2d
endControlPoint (Types.CubicSpline2d spline) =
    spline.endControlPoint


{-| Get the end point of a spline.

    CubicSpline2d.endPoint exampleSpline
    --> Point2d.fromCoordinates ( 7, 4 )

-}
endPoint : CubicSpline2d -> Point2d
endPoint (Types.CubicSpline2d spline) =
    spline.endPoint


{-| Get the start derivative of a spline. This is equal to three times the
vector from the spline's start point to its start control point.

    CubicSpline2d.startDerivative exampleSpline
    --> Vector2d.fromComponents ( 6, 9 )

-}
startDerivative : CubicSpline2d -> Vector2d
startDerivative spline =
    Vector2d.from (startPoint spline) (startControlPoint spline)
        |> Vector2d.scaleBy 3


{-| Get the end derivative of a spline. This is equal to three times the vector
from the spline's end control point to its end point.

    CubicSpline2d.endDerivative exampleSpline
    --> Vector2d.fromComponents ( 6, 9 )

-}
endDerivative : CubicSpline2d -> Vector2d
endDerivative spline =
    Vector2d.from (endControlPoint spline) (endPoint spline)
        |> Vector2d.scaleBy 3


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
area than the spline itself).

    CubicSpline2d.boundingBox exampleSpline
    --> BoundingBox2d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 7
    -->     , minY = 1
    -->     , maxY = 4
    -->     }

-}
boundingBox : CubicSpline2d -> BoundingBox2d
boundingBox spline =
    let
        ( x1, y1 ) =
            Point2d.coordinates (startPoint spline)

        ( x2, y2 ) =
            Point2d.coordinates (startControlPoint spline)

        ( x3, y3 ) =
            Point2d.coordinates (endControlPoint spline)

        ( x4, y4 ) =
            Point2d.coordinates (endPoint spline)
    in
    BoundingBox2d.fromExtrema
        { minX = min (min x1 x2) (min x3 x4)
        , maxX = max (max x1 x2) (max x3 x4)
        , minY = min (min y1 y2) (min y3 y4)
        , maxY = max (max y1 y2) (max y3 y4)
        }


{-| Get a point along a spline, based on a parameter that ranges from 0 to 1. A
parameter value of 0 corresponds to the start point of the spline and a value of
1 corresponds to the end point.

    CubicSpline2d.pointOn exampleSpline 0
    --> Point2d.fromCoordinates ( 1, 1 )

    CubicSpline2d.pointOn exampleSpline 0.5
    --> Point2d.fromCoordinates ( 4, 2.5 )

    CubicSpline2d.pointOn exampleSpline 1
    --> Point2d.fromCoordinates ( 7, 4 )

-}
pointOn : CubicSpline2d -> Float -> Maybe Point2d
pointOn spline t =
    if 0 <= t && t <= 1 then
        let
            p1 =
                startPoint spline

            p2 =
                startControlPoint spline

            p3 =
                endControlPoint spline

            p4 =
                endPoint spline

            q1 =
                Point2d.interpolateFrom p1 p2 t

            q2 =
                Point2d.interpolateFrom p2 p3 t

            q3 =
                Point2d.interpolateFrom p3 p4 t

            r1 =
                Point2d.interpolateFrom q1 q2 t

            r2 =
                Point2d.interpolateFrom q2 q3 t
        in
        Just <| Point2d.interpolateFrom r1 r2 t
    else
        Nothing


{-| Convenient shorthand for evaluating multiple points;

    CubicSpline2d.pointsOn spline parameterValues

is equivalent to

    List.map (CubicSpline2d.pointOn spline) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
pointsOn : CubicSpline2d -> List Float -> List Point2d
pointsOn spline parameterValues =
    List.filterMap (pointOn spline) parameterValues


{-| Get the derivative vector at a point along a spline, based on a parameter
that ranges from 0 to 1. A parameter value of 0 corresponds to the start
derivative of the spline and a value of 1 corresponds to the end derivative.

    CubicSpline2d.derivative exampleSpline 0
    --> Vector2d.fromComponents ( 6, 9 )

    CubicSpline2d.derivative exampleSpline 0.5
    --> Vector2d.fromComponents ( 6, 0 )

    CubicSpline2d.derivative exampleSpline 1
    --> Vector2d.fromComponents ( 6, 9 )

-}
derivative : CubicSpline2d -> Float -> Maybe Vector2d
derivative spline t =
    if 0 <= t && t <= 1 then
        let
            p1 =
                startPoint spline

            p2 =
                startControlPoint spline

            p3 =
                endControlPoint spline

            p4 =
                endPoint spline

            ( x1, y1 ) =
                Point2d.coordinates p1

            ( x2, y2 ) =
                Point2d.coordinates p2

            ( x3, y3 ) =
                Point2d.coordinates p3

            ( x4, y4 ) =
                Point2d.coordinates p4

            vx1 =
                x2 - x1

            vy1 =
                y2 - y1

            vx2 =
                x3 - x2

            vy2 =
                y3 - y2

            vx3 =
                x4 - x3

            vy3 =
                y4 - y3
        in
        if t <= 0.5 then
            let
                wx1 =
                    vx1 + t * (vx2 - vx1)

                wy1 =
                    vy1 + t * (vy2 - vy1)

                wx2 =
                    vx2 + t * (vx3 - vx2)

                wy2 =
                    vy2 + t * (vy3 - vy2)
            in
            Just <|
                Vector2d.fromComponents
                    ( 3 * (wx1 + t * (wx2 - wx1))
                    , 3 * (wy1 + t * (wy2 - wy1))
                    )
        else
            let
                u =
                    1 - t

                wx1 =
                    vx2 + u * (vx1 - vx2)

                wy1 =
                    vy2 + u * (vy1 - vy2)

                wx2 =
                    vx3 + u * (vx2 - vx3)

                wy2 =
                    vy3 + u * (vy2 - vy3)
            in
            Just <|
                Vector2d.fromComponents
                    ( 3 * (wx2 + u * (wx1 - wx2))
                    , 3 * (wy2 + u * (wy1 - wy2))
                    )
    else
        Nothing


{-| Convenient shorthand for evaluating multiple derivatives;

    CubicSpline2d.derivatives spline parameterValues

is equivalent to

    List.map (CubicSpline2d.derivative spline) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
derivatives : CubicSpline2d -> List Float -> List Vector2d
derivatives spline parameterValues =
    List.filterMap (derivative spline) parameterValues


{-| Find the magnitude of the derivative to a spline at a particular parameter
value;

    CubicSpline2d.derivativeMagnitude spline t

is equivalent to

    CubicSpline2d.derivative spline t
        |> Maybe.map Vector2d.length
        |> Maybe.withDefault 0

but more efficient since it avoids any intermediate `Vector2d` and `Maybe`
allocations.

-}
derivativeMagnitude : CubicSpline2d -> Float -> Float
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

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3

        ( x4, y4 ) =
            Point2d.coordinates p4

        x12 =
            x2 - x1

        y12 =
            y2 - y1

        x23 =
            x3 - x2

        y23 =
            y3 - y2

        x34 =
            x4 - x3

        y34 =
            y4 - y3

        x123 =
            x23 - x12

        y123 =
            y23 - y12

        x234 =
            x34 - x23

        y234 =
            y34 - y23
    in
    \t ->
        if 0 <= t && t <= 1 then
            let
                x13 =
                    x12 + t * x123

                y13 =
                    y12 + t * y123

                x24 =
                    x23 + t * x234

                y24 =
                    y23 + t * y234

                x14 =
                    x13 + t * (x24 - x13)

                y14 =
                    y13 + t * (y24 - y13)
            in
            3 * sqrt (x14 * x14 + y14 * y14)
        else
            0


{-| Sample a spline at a given parameter value to get both the position and
derivative vector at that parameter value;

    CubicSpline2d.sample spline t

is equivalent to

    ( CubicSpline2d.pointOn spline t
    , CubicSpline2d.derivative spline t
    )

but is more efficient.

-}
sample : CubicSpline2d -> Float -> Maybe ( Point2d, Vector2d )
sample spline t =
    if 0 <= t && t <= 1 then
        let
            p1 =
                startPoint spline

            p2 =
                startControlPoint spline

            p3 =
                endControlPoint spline

            p4 =
                endPoint spline

            q1 =
                Point2d.interpolateFrom p1 p2 t

            q2 =
                Point2d.interpolateFrom p2 p3 t

            q3 =
                Point2d.interpolateFrom p3 p4 t

            r1 =
                Point2d.interpolateFrom q1 q2 t

            r2 =
                Point2d.interpolateFrom q2 q3 t
        in
        Just
            ( Point2d.interpolateFrom r1 r2 t
            , Vector2d.from r1 r2 |> Vector2d.scaleBy 3
            )
    else
        Nothing


{-| Convenient shorthand for evaluating multiple samples;

    CubicSpline2d.samples spline parameterValues

is equivalent to

    List.map (CubicSpline2d.sample spline) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
samples : CubicSpline2d -> List Float -> List ( Point2d, Vector2d )
samples spline parameterValues =
    List.filterMap (sample spline) parameterValues


mapControlPoints : (Point2d -> Point2d) -> CubicSpline2d -> CubicSpline2d
mapControlPoints function spline =
    with
        { startPoint = function (startPoint spline)
        , startControlPoint = function (startControlPoint spline)
        , endControlPoint = function (endControlPoint spline)
        , endPoint = function (endPoint spline)
        }


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    CubicSpline2d.reverse exampleSpline
    --> CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 7, 4 )
    -->     , Point2d.fromCoordinates ( 5, 1 )
    -->     , Point2d.fromCoordinates ( 3, 4 )
    -->     , Point2d.fromCoordinates ( 1, 1 )
    -->     )

-}
reverse : CubicSpline2d -> CubicSpline2d
reverse spline =
    with
        { startPoint = endPoint spline
        , startControlPoint = endControlPoint spline
        , endControlPoint = startControlPoint spline
        , endPoint = startPoint spline
        }


{-| Scale a spline about the given center point by the given scale.

    CubicSpline2d.scaleAbout Point2d.origin 2 exampleSpline
    --> CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 2, 2 )
    -->     , Point2d.fromCoordinates ( 6, 8 )
    -->     , Point2d.fromCoordinates ( 10, 2 )
    -->     , Point2d.fromCoordinates ( 14, 8 )
    -->     )

-}
scaleAbout : Point2d -> Float -> CubicSpline2d -> CubicSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given center point by a given
angle (in radians).

    exampleSpline
        |> CubicSpline2d.rotateAround Point2d.origin
            (degrees 90)
    --> CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( -1, 1 )
    -->     , Point2d.fromCoordinates ( -4, 3 )
    -->     , Point2d.fromCoordinates ( -1, 5 )
    -->     , Point2d.fromCoordinates ( -4, 7 )
    -->     )

-}
rotateAround : Point2d -> Float -> CubicSpline2d -> CubicSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    CubicSpline2d.translateBy displacement exampleSpline
    --> CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 3, 4 )
    -->     , Point2d.fromCoordinates ( 5, 7 )
    -->     , Point2d.fromCoordinates ( 7, 4 )
    -->     , Point2d.fromCoordinates ( 9, 7 )
    -->     )

-}
translateBy : Vector2d -> CubicSpline2d -> CubicSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


{-| Translate a spline in a given direction by a given distance;

    CubicSpline2d.translateIn direction distance

is equivalent to

    CubicSpline2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> CubicSpline2d -> CubicSpline2d
translateIn direction distance spline =
    translateBy (Vector2d.withLength distance direction) spline


{-| Mirror a spline across an axis.

    CubicSpline2d.mirrorAcross Axis2d.x exampleSpline
    --> CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 1, -1 )
    -->     , Point2d.fromCoordinates ( 3, -4 )
    -->     , Point2d.fromCoordinates ( 5, -1 )
    -->     , Point2d.fromCoordinates ( 7, -4 )
    -->     )

-}
mirrorAcross : Axis2d -> CubicSpline2d -> CubicSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    CubicSpline2d.relativeTo localFrame exampleSpline
    --> CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 0, -1 )
    -->     , Point2d.fromCoordinates ( 2, 2 )
    -->     , Point2d.fromCoordinates ( 4, -1 )
    -->     , Point2d.fromCoordinates ( 6, 2 )
    -->     )

-}
relativeTo : Frame2d -> CubicSpline2d -> CubicSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    CubicSpline2d.placeIn localFrame exampleSpline
    --> CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 2, 3 )
    -->     , Point2d.fromCoordinates ( 4, 6 )
    -->     , Point2d.fromCoordinates ( 6, 3 )
    -->     , Point2d.fromCoordinates ( 8, 6 )
    -->     )

-}
placeIn : Frame2d -> CubicSpline2d -> CubicSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


{-| Split a spline into two roughly equal halves.

    CubicSpline2d.bisect exampleSpline
    --> ( CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 1, 1 )
    -->     , Point2d.fromCoordinates ( 2, 2.5 )
    -->     , Point2d.fromCoordinates ( 3, 2.5 )
    -->     , Point2d.fromCoordinates ( 4, 2.5 )
    -->     )
    --> , CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 4, 2.5 )
    -->     , Point2d.fromCoordinates ( 5, 2.5 )
    -->     , Point2d.fromCoordinates ( 6, 2.5 )
    -->     , Point2d.fromCoordinates ( 7, 4 )
    -->     )
    --> )

-}
bisect : CubicSpline2d -> ( CubicSpline2d, CubicSpline2d )
bisect =
    splitAt 0.5


{-| Split a spline at a particular parameter value (in the range 0 to 1),
resulting in two smaller splines.

    CubicSpline2d.splitAt 0.75 exampleSpline
    --> ( CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 1, 1 )
    -->     , Point2d.fromCoordinates ( 2.5, 3.25 )
    -->     , Point2d.fromCoordinates ( 4, 2.125 )
    -->     , Point2d.fromCoordinates ( 5.5, 2.6875 )
    -->     )
    --> , CubicSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 5.5, 2.6875 )
    -->     , Point2d.fromCoordinates ( 6, 2.875 )
    -->     , Point2d.fromCoordinates ( 6.5, 3.25 )
    -->     , Point2d.fromCoordinates ( 7, 4 )
    -->     )
    --> )

-}
splitAt : Float -> CubicSpline2d -> ( CubicSpline2d, CubicSpline2d )
splitAt t spline =
    let
        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        q1 =
            Point2d.interpolateFrom p1 p2 t

        q2 =
            Point2d.interpolateFrom p2 p3 t

        q3 =
            Point2d.interpolateFrom p3 p4 t

        r1 =
            Point2d.interpolateFrom q1 q2 t

        r2 =
            Point2d.interpolateFrom q2 q3 t

        s =
            Point2d.interpolateFrom r1 r2 t
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
    = ArcLengthParameterized CubicSpline2d ArcLengthParameterization


{-| Build an arc length parameterization of the given spline:

    parameterizedSpline =
        CubicSpline2d.arcLengthParameterized
            (Accuracy.maxError 1.0e-4)
            exampleSpline

The accuracy of the parameterization is controlled by the first argument; this
affects the accuracy of results returned from functions such as `arcLength` and
`pointAlong`.

-}
arcLengthParameterized : Accuracy -> CubicSpline2d -> ArcLengthParameterized
arcLengthParameterized (Types.MaxError tolerance) spline =
    let
        parameterization =
            ArcLengthParameterization.build
                { tolerance = tolerance
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude =
                    maxSecondDerivativeMagnitude spline
                }
    in
    ArcLengthParameterized spline parameterization


{-| Find the total arc length of a spline:

    CubicSpline2d.arcLength parameterizedSpline
    --> 7.0952

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized -> Float
arcLength (ArcLengthParameterized _ parameterization) =
    ArcLengthParameterization.totalArcLength parameterization


{-| Try to get the point along a spline at a given arc length. For example, to
get the point a quarter of the way along `exampleSpline`:

    CubicSpline2d.pointAlong parameterizedSpline
        (arcLength / 4)
    --> Just (Point2d.fromCoordinates ( 2.2681, 2.2114 ))

Note that this is not the same as evaulating at a parameter value of 1/4:

    CubicSpline2d.pointOn exampleSpline 0.25
    --> Point2d.fromCoordinates ( 2.5, 2.3125 )

If the given arc length is less than zero or greater than the arc length of the
spline, `Nothing` is returned.

-}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point2d
pointAlong (ArcLengthParameterized spline parameterization) s =
    parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue s
        |> Maybe.andThen (pointOn spline)


{-| Try to get the tangent direction along a spline at a given arc length. To
get the tangent direction a quarter of the way along `exampleSpline`:

    CubicSpline2d.tangentAlong parameterizedSpline
        (arcLength / 4)
    --> Just (Direction2d.fromAngle (degrees 26.5611))

If the given arc length is less than zero or greater than the arc length of the
spline (or if the derivative of the spline happens to be exactly zero at the
given arc length), `Nothing` is returned.

-}
tangentAlong : ArcLengthParameterized -> Float -> Maybe Direction2d
tangentAlong (ArcLengthParameterized spline parameterization) s =
    parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue s
        |> Maybe.andThen (derivative spline)
        |> Maybe.andThen Vector2d.direction


{-| Try to get the parameter value along a spline at a given arc length. If the
given arc length is less than zero or greater than the arc length of the spline,
returns `Nothing`.

    CubicSpline2d.arcLengthToParameterValue
        parameterizedSpline
        (arcLength / 4)
    --> Just 0.2113

-}
arcLengthToParameterValue : ArcLengthParameterized -> Float -> Maybe Float
arcLengthToParameterValue (ArcLengthParameterized _ parameterization) s =
    ArcLengthParameterization.arcLengthToParameterValue s parameterization


{-| Try to get the arc length along a spline at a given parameter value. If the
given parameter value is less than zero or greater than one, returns `Nothing`.

    CubicSpline2d.parameterValueToArcLength
        parameterizedSpline
        0.25
    --> 2.0269

-}
parameterValueToArcLength : ArcLengthParameterized -> Float -> Maybe Float
parameterValueToArcLength (ArcLengthParameterized _ parameterization) t =
    ArcLengthParameterization.parameterValueToArcLength t parameterization


{-| Get the original `CubicSpline2d` from which an `ArcLengthParameterized`
value was constructed.
-}
underlyingSpline : ArcLengthParameterized -> CubicSpline2d
underlyingSpline (ArcLengthParameterized spline _) =
    spline


{-| Find an upper bound on the magnitude of the second derivative of a spline.
-}
maxSecondDerivativeMagnitude : CubicSpline2d -> Float
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
            Vector2d.from p1 p2

        u2 =
            Vector2d.from p2 p3

        u3 =
            Vector2d.from p3 p4

        v1 =
            Vector2d.difference u2 u1

        v2 =
            Vector2d.difference u3 u2
    in
    6 * max (Vector2d.length v1) (Vector2d.length v2)


{-| Get the second derivative value at a point along a spline, based on a
parameter that ranges from 0 to 1. A parameter value of 0 corresponds to the
start of the spline and a value of 1 corresponds to the end.

    CubicSpline2d.secondDerivative exampleSpline 0
    --> Vector2d.fromComponents ( 0, -36 )

    CubicSpline2d.secondDerivative exampleSpline 0.5
    --> Vector2d.fromComponents ( 0, 0 )

    CubicSpline2d.secondDerivative exampleSpline 1
    --> Vector2d.fromComponents ( 0, 36 )

-}
secondDerivative : CubicSpline2d -> Float -> Maybe Vector2d
secondDerivative spline t =
    if 0 <= t && t <= 1 then
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
                Vector2d.from p1 p2

            u2 =
                Vector2d.from p2 p3

            u3 =
                Vector2d.from p3 p4

            v1 =
                Vector2d.difference u2 u1

            v2 =
                Vector2d.difference u3 u2
        in
        Just <| Vector2d.scaleBy 6 (Vector2d.interpolateFrom v1 v2 t)
    else
        Nothing


{-| Convenient shorthand for evaluating multiple second derivatives;

    CubicSpline2d.secondDerivatives spline parameterValues

is equivalent to

    List.map (CubicSpline2d.secondDerivative spline)
        parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
secondDerivatives : CubicSpline2d -> List Float -> List Vector2d
secondDerivatives spline parameterValues =
    List.filterMap (secondDerivative spline) parameterValues
