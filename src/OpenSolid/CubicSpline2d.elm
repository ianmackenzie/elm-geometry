module OpenSolid.CubicSpline2d
    exposing
        ( ArcLengthParameterized
        , CubicSpline2d
        , arcLength
        , arcLengthParameterized
        , arcLengthToParameterValue
        , bisect
        , controlPoints
        , derivative
        , derivativeMagnitude
        , endDerivative
        , endPoint
        , evaluate
        , fromControlPoints
        , fromQuadraticSpline
        , hermite
        , maxSecondDerivativeMagnitude
        , mirrorAcross
        , parameterValueToArcLength
        , placeIn
        , pointAlong
        , pointOn
        , relativeTo
        , reverse
        , rotateAround
        , scaleAbout
        , splitAt
        , startDerivative
        , startPoint
        , tangentAlong
        , translateBy
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/cubicSpline2d.svg" alt="CubicSpline2d" width="160">

A `CubicSpline2d` is a cubic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by four control points. This module contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs CubicSpline2d


# Constructors

@docs fromControlPoints, hermite, fromQuadraticSpline


# Properties

@docs controlPoints, startPoint, endPoint, startDerivative, endDerivative


# Evaluation

@docs pointOn, derivative, evaluate


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentAlong, arcLengthToParameterValue, parameterValueToArcLength


# Low level

Low level functionality that you are unlikely to need to use directly.

@docs derivativeMagnitude, maxSecondDerivativeMagnitude

-}

import OpenSolid.ArcLength as ArcLength
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


{-| -}
type alias CubicSpline2d =
    Internal.CubicSpline2d


{-| Construct a spline from its four control points:

    exampleSpline =
        CubicSpline2d.fromControlPoints
            ( Point2d.fromCoordinates ( 1, 1 )
            , Point2d.fromCoordinates ( 3, 4 )
            , Point2d.fromCoordinates ( 5, 1 )
            , Point2d.fromCoordinates ( 7, 4 )
            )

-}
fromControlPoints : ( Point2d, Point2d, Point2d, Point2d ) -> CubicSpline2d
fromControlPoints =
    Internal.CubicSpline2d


{-| Construct a spline in Hermite form, from the position and derivative values
at its start and end points, like so:

![Hermite cubic spline](https://opensolid.github.io/images/geometry/1.2/hermiteCubicSpline.svg)

The spline is based on a parameter that ranges from 0 to 1; as a result, in most
cases the length of each derivative vector should be roughly equal to the length
of the resulting spline.

-}
hermite : ( Point2d, Vector2d ) -> ( Point2d, Vector2d ) -> CubicSpline2d
hermite ( startPoint, startDerivative ) ( endPoint, endDerivative ) =
    let
        startControlPoint =
            startPoint
                |> Point2d.translateBy
                    (Vector2d.scaleBy (1 / 3) startDerivative)

        endControlPoint =
            endPoint
                |> Point2d.translateBy
                    (Vector2d.scaleBy (-1 / 3) endDerivative)
    in
    fromControlPoints
        ( startPoint
        , startControlPoint
        , endControlPoint
        , endPoint
        )


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
        ( p1, p2, p3 ) =
            QuadraticSpline2d.controlPoints quadraticSpline
    in
    fromControlPoints
        ( p1
        , Point2d.interpolateFrom p1 p2 (2 / 3)
        , Point2d.interpolateFrom p3 p2 (2 / 3)
        , p3
        )


{-| Get the control points of a spline as a tuple.

    ( p1, p2, p3, p4 ) =
        CubicSpline2d.controlPoints exampleSpline


    --> p1 = Point2d.fromCoordinates ( 1, 1 )
    --> p2 = Point2d.fromCoordinates ( 3, 4 )
    --> p3 = Point2d.fromCoordinates ( 5, 1 )
    --> p4 = Point2d.fromCoordinates ( 7, 4 )

-}
controlPoints : CubicSpline2d -> ( Point2d, Point2d, Point2d, Point2d )
controlPoints (Internal.CubicSpline2d controlPoints_) =
    controlPoints_


{-| Get the start point of a spline. This is equal to the spline's first control
point.

    CubicSpline2d.startPoint exampleSpline
    --> Point2d.fromCoordinates ( 1, 1 )

-}
startPoint : CubicSpline2d -> Point2d
startPoint (Internal.CubicSpline2d ( p1, _, _, _ )) =
    p1


{-| Get the end point of a spline. This is equal to the spline's last control
point.

    CubicSpline2d.endPoint exampleSpline
    --> Point2d.fromCoordinates ( 7, 4 )

-}
endPoint : CubicSpline2d -> Point2d
endPoint (Internal.CubicSpline2d ( _, _, _, p4 )) =
    p4


{-| Get the start derivative of a spline. This is equal to three times the
vector from the spline's first control point to its second.

    CubicSpline2d.startDerivative exampleSpline
    --> Vector2d.fromComponents ( 6, 9 )

-}
startDerivative : CubicSpline2d -> Vector2d
startDerivative spline =
    let
        ( p1, p2, _, _ ) =
            controlPoints spline
    in
    Vector2d.from p1 p2 |> Vector2d.scaleBy 3


{-| Get the end derivative of a spline. This is equal to three times the vector
from the spline's third control point to its fourth.

    CubicSpline2d.endDerivative exampleSpline
    --> Vector2d.fromComponents ( 6, 9 )

-}
endDerivative : CubicSpline2d -> Vector2d
endDerivative spline =
    let
        ( _, _, p3, p4 ) =
            controlPoints spline
    in
    Vector2d.from p3 p4 |> Vector2d.scaleBy 3


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
pointOn : CubicSpline2d -> Float -> Point2d
pointOn spline t =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

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
    Point2d.interpolateFrom r1 r2 t


{-| Get the deriative value at a point along a spline, based on a parameter that
ranges from 0 to 1. A parameter value of 0 corresponds to the start derivative
of the spline and a value of 1 corresponds to the end derivative.

    CubicSpline2d.derivative exampleSpline 0
    --> Vector2d.fromComponents ( 6, 9 )

    CubicSpline2d.derivative exampleSpline 0.5
    --> Vector2d.fromComponents ( 6, 0 )

    CubicSpline2d.derivative exampleSpline 1
    --> Vector2d.fromComponents ( 6, 9 )

-}
derivative : CubicSpline2d -> Float -> Vector2d
derivative spline t =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

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
        Vector2d.fromComponents
            ( 3 * (wx2 + u * (wx1 - wx2))
            , 3 * (wy2 + u * (wy1 - wy2))
            )


{-| Find the magnitude of the derivative to a spline at a particular parameter
value;

    CubicSpline2d.derivativeMagnitude spline t

is equivalent to

    Vector2d.length (CubicSpline2d.derivative spline t)

but more efficient since it avoids any intermediate `Vector2d` allocation.

-}
derivativeMagnitude : CubicSpline2d -> Float -> Float
derivativeMagnitude spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

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


{-| Evaluate a spline at a given parameter value, returning the point on the
spline at that parameter value and the derivative with respect to that parameter
value;

    CubicSpline2d.evaluate spline t

is equivalent to

    ( CubicSpline2d.pointOn spline t
    , CubicSpline2d.derivative spline t
    )

but is more efficient.

-}
evaluate : CubicSpline2d -> Float -> ( Point2d, Vector2d )
evaluate spline t =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

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
    ( Point2d.interpolateFrom r1 r2 t
    , Vector2d.from r1 r2 |> Vector2d.scaleBy 3
    )


mapControlPoints : (Point2d -> Point2d) -> CubicSpline2d -> CubicSpline2d
mapControlPoints function spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline
    in
    fromControlPoints ( function p1, function p2, function p3, function p4 )


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
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline
    in
    fromControlPoints ( p4, p3, p2, p1 )


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


{-| Split a spline into two roughly equal halves. Equivalent to `splitAt 0.5`.

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
        ( p1, p2, p3, p4 ) =
            controlPoints spline

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
    ( fromControlPoints ( p1, q1, r1, s )
    , fromControlPoints ( s, r2, q3, p4 )
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized CubicSpline2d ArcLength.Parameterization


{-| Build an arc length parameterization of the given spline, within
a given tolerance. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the given arc length
tolerance.

    tolerance =
        1.0e-4

    parameterizedSpline =
        CubicSpline2d.arcLengthParameterized
            tolerance
            exampleSpline

-}
arcLengthParameterized : Float -> CubicSpline2d -> ArcLengthParameterized
arcLengthParameterized tolerance spline =
    let
        parameterization =
            ArcLength.parameterization
                { tolerance = tolerance
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude =
                    maxSecondDerivativeMagnitude spline
                }
    in
    ArcLengthParameterized spline parameterization


{-| Find the total arc length of a spline. This will be accurate to within the
tolerance given when calling `arcLengthParameterized`.

    arcLength : Float
    arcLength =
        CubicSpline2d.arcLength parameterizedSpline

    arcLength
    --> 7.0952

-}
arcLength : ArcLengthParameterized -> Float
arcLength (ArcLengthParameterized _ parameterization) =
    ArcLength.fromParameterization parameterization


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
    ArcLength.toParameterValue parameterization s |> Maybe.map (pointOn spline)


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
    ArcLength.toParameterValue parameterization s
        |> Maybe.map (derivative spline)
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
    ArcLength.toParameterValue parameterization s


{-| Try to get the arc length along a spline at a given parameter value. If the
given parameter value is less than zero or greater than one, returns `Nothing`.

    CubicSpline2d.parameterValueToArcLength
        parameterizedSpline
        0.25
    --> 2.0269

-}
parameterValueToArcLength : ArcLengthParameterized -> Float -> Maybe Float
parameterValueToArcLength (ArcLengthParameterized _ parameterization) t =
    ArcLength.fromParameterValue parameterization t


{-| Find an upper bound on the magnitude of the second derivative of a spline.
-}
maxSecondDerivativeMagnitude : CubicSpline2d -> Float
maxSecondDerivativeMagnitude spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

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
