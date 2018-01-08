module OpenSolid.QuadraticSpline2d
    exposing
        ( ArcLengthParameterized
        , QuadraticSpline2d
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
        , mirrorAcross
        , parameterValueToArcLength
        , placeIn
        , pointAlong
        , pointOn
        , relativeTo
        , reverse
        , rotateAround
        , scaleAbout
        , secondDerivative
        , splitAt
        , startDerivative
        , startPoint
        , tangentAlong
        , translateBy
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/quadraticSpline2d.svg" alt="QuadraticSpline2d" width="160">

A `QuadraticSpline2d` is a quadratic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by three control points. This module contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs QuadraticSpline2d


# Constructors

@docs fromControlPoints


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

@docs derivativeMagnitude, secondDerivative

-}

import OpenSolid.ArcLength as ArcLength
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


{-| -}
type alias QuadraticSpline2d =
    Internal.QuadraticSpline2d


{-| Construct a spline from its three control points:

    exampleSpline =
        QuadraticSpline2d.fromControlPoints
            ( Point2d.fromCoordinates ( 1, 1 )
            , Point2d.fromCoordinates ( 3, 4 )
            , Point2d.fromCoordinates ( 5, 1 )
            )

-}
fromControlPoints : ( Point2d, Point2d, Point2d ) -> QuadraticSpline2d
fromControlPoints =
    Internal.QuadraticSpline2d


{-| Get the control points of a spline as a tuple.

    ( p1, p2, p3 ) =
        QuadraticSpline2d.controlPoints exampleSpline


    --> p1 = Point2d.fromCoordinates ( 1, 1 )
    --> p2 = Point2d.fromCoordinates ( 3, 4 )
    --> p3 = Point2d.fromCoordinates ( 5, 1 )

-}
controlPoints : QuadraticSpline2d -> ( Point2d, Point2d, Point2d )
controlPoints (Internal.QuadraticSpline2d controlPoints_) =
    controlPoints_


{-| Get the start point of a spline. This is equal to the spline's first control
point.

    QuadraticSpline2d.startPoint exampleSpline
    --> Point2d.fromCoordinates ( 1, 1 )

-}
startPoint : QuadraticSpline2d -> Point2d
startPoint (Internal.QuadraticSpline2d ( p1, _, _ )) =
    p1


{-| Get the end point of a spline. This is equal to the spline's last control
point.

    QuadraticSpline2d.endPoint exampleSpline
    --> Point2d.fromCoordinates ( 5, 1 )

-}
endPoint : QuadraticSpline2d -> Point2d
endPoint (Internal.QuadraticSpline2d ( _, _, p3 )) =
    p3


{-| Get the start derivative of a spline. This is equal to twice the vector from
the spline's first control point to its second.

    QuadraticSpline2d.startDerivative exampleSpline
    --> Vector2d.fromComponents ( 4, 6 )

-}
startDerivative : QuadraticSpline2d -> Vector2d
startDerivative spline =
    let
        ( p1, p2, _ ) =
            controlPoints spline
    in
    Vector2d.from p1 p2 |> Vector2d.scaleBy 2


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline2d.endDerivative exampleSpline
    --> Vector2d.fromComponents ( 4, -6 )

-}
endDerivative : QuadraticSpline2d -> Vector2d
endDerivative spline =
    let
        ( _, p2, p3 ) =
            controlPoints spline
    in
    Vector2d.from p2 p3 |> Vector2d.scaleBy 2


{-| Get a point along a spline, based on a parameter that ranges from 0 to 1. A
parameter value of 0 corresponds to the start point of the spline and a value of
1 corresponds to the end point.

    QuadraticSpline2d.pointOn exampleSpline 0
    --> Point2d.fromCoordinates ( 1, 1 )

    QuadraticSpline2d.pointOn exampleSpline 0.5
    --> Point2d.fromCoordinates ( 3, 2.5 )

    QuadraticSpline2d.pointOn exampleSpline 1
    --> Point2d.fromCoordinates ( 5, 1 )

-}
pointOn : QuadraticSpline2d -> Float -> Point2d
pointOn spline t =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        q1 =
            Point2d.interpolateFrom p1 p2 t

        q2 =
            Point2d.interpolateFrom p2 p3 t
    in
    Point2d.interpolateFrom q1 q2 t


{-| Get the deriative value at a point along a spline, based on a parameter that
ranges from 0 to 1. A parameter value of 0 corresponds to the start derivative
of the spline and a value of 1 corresponds to the end derivative.

    QuadraticSpline2d.derivative exampleSpline 0
    --> Vector2d.fromComponents ( 4, 6 )

    QuadraticSpline2d.derivative exampleSpline 0.5
    --> Vector2d.fromComponents ( 4, 0 )

    QuadraticSpline2d.derivative exampleSpline 1
    --> Vector2d.fromComponents ( 4, -6 )

Note that the derivative interpolates linearly from end to end.

-}
derivative : QuadraticSpline2d -> Float -> Vector2d
derivative spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        v1 =
            Vector2d.from p1 p2

        v2 =
            Vector2d.from p2 p3
    in
    \t -> Vector2d.interpolateFrom v1 v2 t |> Vector2d.scaleBy 2


{-| Find the magnitude of the derivative to a spline at a particular parameter
value;

    QuadraticSpline2d.derivativeMagnitude spline t

is equivalent to

    Vector2d.length (QuadraticSpline2d.derivative spline t)

but more efficient since it avoids any intermediate `Vector2d` allocation.

-}
derivativeMagnitude : QuadraticSpline2d -> Float -> Float
derivativeMagnitude spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3

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
    \t ->
        let
            x13 =
                x12 + t * x123

            y13 =
                y12 + t * y123
        in
        2 * sqrt (x13 * x13 + y13 * y13)


{-| Evaluate a spline at a given parameter value, returning the point on the
spline at that parameter value and the derivative with respect to that parameter
value;

    QuadraticSpline2d.evaluate spline t

is equivalent to

    ( QuadraticSpline2d.pointOn spline t
    , QuadraticSpline2d.derivative spline t
    )

but is more efficient.

-}
evaluate : QuadraticSpline2d -> Float -> ( Point2d, Vector2d )
evaluate spline t =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        q1 =
            Point2d.interpolateFrom p1 p2 t

        q2 =
            Point2d.interpolateFrom p2 p3 t
    in
    ( Point2d.interpolateFrom q1 q2 t
    , Vector2d.from q1 q2 |> Vector2d.scaleBy 2
    )


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    QuadraticSpline2d.reverse exampleSpline
    --> QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 5, 1 )
    -->     , Point2d.fromCoordinates ( 3, 4 )
    -->     , Point2d.fromCoordinates ( 1, 1 )
    -->     )

-}
reverse : QuadraticSpline2d -> QuadraticSpline2d
reverse spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline
    in
    fromControlPoints ( p3, p2, p1 )


{-| Scale a spline about the given center point by the given scale.

    examplePolyline
        |> QuadraticSpline2d.scaleAbout Point2d.origin 2
    --> QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 2, 2 )
    -->     , Point2d.fromCoordinates ( 6, 8 )
    -->     , Point2d.fromCoordinates ( 10, 2 )
    -->     )

-}
scaleAbout : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given center point by a given
angle (in radians).

    examplePolyline
        |> QuadraticSpline2d.rotateAround Point2d.origin
            (degrees 90)
    --> QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( -1, 1 )
    -->     , Point2d.fromCoordinates ( -4, 3 )
    -->     , Point2d.fromCoordinates ( -1, 5 )
    -->     )

-}
rotateAround : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    exampleSpline
        |> QuadraticSpline2d.translateBy displacement
    --> QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 3, 4 )
    -->     , Point2d.fromCoordinates ( 5, 7 )
    -->     , Point2d.fromCoordinates ( 7, 4 )
    -->     )

-}
translateBy : Vector2d -> QuadraticSpline2d -> QuadraticSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


{-| Mirror a spline across an axis.

    QuadraticSpline2d.mirrorAcross Axis2d.x exampleSpline
    --> QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 1, -1 )
    -->     , Point2d.fromCoordinates ( 3, -4 )
    -->     , Point2d.fromCoordinates ( 5, -1 )
    -->     )

-}
mirrorAcross : Axis2d -> QuadraticSpline2d -> QuadraticSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


mapControlPoints : (Point2d -> Point2d) -> QuadraticSpline2d -> QuadraticSpline2d
mapControlPoints function spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline
    in
    fromControlPoints ( function p1, function p2, function p3 )


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    QuadraticSpline2d.relativeTo localFrame exampleSpline
    --> QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 0, -1 )
    -->     , Point2d.fromCoordinates ( 2, 2 )
    -->     , Point2d.fromCoordinates ( 4, -1 )
    -->     )

-}
relativeTo : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    QuadraticSpline2d.placeIn localFrame exampleSpline
    --> QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 2, 3 )
    -->     , Point2d.fromCoordinates ( 4, 6 )
    -->     , Point2d.fromCoordinates ( 6, 3 )
    -->     )

-}
placeIn : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


{-| Split a spline into two roughly equal halves. Equivalent to `splitAt 0.5`.

    QuadraticSpline2d.bisect exampleSpline
    --> ( QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 1, 1 )
    -->     , Point2d.fromCoordinates ( 2, 2.5 )
    -->     , Point2d.fromCoordinates ( 3, 2.5 )
    -->     )
    --> , QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 3, 2.5 )
    -->     , Point2d.fromCoordinates ( 4, 2.5 )
    -->     , Point2d.fromCoordinates ( 5, 1 )
    -->     )
    --> )

-}
bisect : QuadraticSpline2d -> ( QuadraticSpline2d, QuadraticSpline2d )
bisect =
    splitAt 0.5


{-| Split a spline at a particular parameter value (in the range 0 to 1),
resulting in two smaller splines.

    QuadraticSpline2d.splitAt 0.75 exampleSpline
    --> ( QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 1, 1 )
    -->     , Point2d.fromCoordinates ( 2.5, 3.25 )
    -->     , Point2d.fromCoordinates ( 4, 2.125 )
    -->     )
    --> , QuadraticSpline2d.fromControlPoints
    -->     ( Point2d.fromCoordinates ( 4, 2.125 )
    -->     , Point2d.fromCoordinates ( 4.5, 1.75 )
    -->     , Point2d.fromCoordinates ( 5, 1 )
    -->     )
    --> )

-}
splitAt : Float -> QuadraticSpline2d -> ( QuadraticSpline2d, QuadraticSpline2d )
splitAt t spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        q1 =
            Point2d.interpolateFrom p1 p2 t

        q2 =
            Point2d.interpolateFrom p2 p3 t

        r =
            Point2d.interpolateFrom q1 q2 t
    in
    ( fromControlPoints ( p1, q1, r )
    , fromControlPoints ( r, q2, p3 )
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized QuadraticSpline2d ArcLength.Parameterization


{-| Build an arc length parameterization of the given spline, within
a given tolerance. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the given arc length
tolerance.

    tolerance =
        1.0e-4

    parameterizedSpline =
        QuadraticSpline2d.arcLengthParameterized
            tolerance
            exampleSpline

-}
arcLengthParameterized : Float -> QuadraticSpline2d -> ArcLengthParameterized
arcLengthParameterized tolerance spline =
    let
        maxSecondDerivativeMagnitude =
            Vector2d.length (secondDerivative spline)

        parameterization =
            ArcLength.parameterization
                { tolerance = tolerance
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude = maxSecondDerivativeMagnitude
                }
    in
    ArcLengthParameterized spline parameterization


{-| Find the total arc length of a spline. This will be accurate to within the
tolerance given when calling `arcLengthParameterized`.

    arcLength : Float
    arcLength =
        QuadraticSpline2d.arcLength parameterizedSpline

    arcLength
    --> 5.1986

-}
arcLength : ArcLengthParameterized -> Float
arcLength (ArcLengthParameterized _ parameterization) =
    ArcLength.fromParameterization parameterization


{-| Try to get the point along a spline at a given arc length. For example, to
get the point a quarter of the way along `exampleSpline`:

    QuadraticSpline2d.pointAlong parameterizedSpline
        (arcLength / 4)
    --> Just (Point2d.fromCoordinates ( 1.8350, 1.9911 ))

Note that this is not the same as evaulating at a parameter value of 1/4:

    QuadraticSpline2d.pointOn exampleSpline 0.25
    --> Point2d.fromCoordinates ( 2, 2.125 )

If the given arc length is less than zero or greater than the arc length of the
spline, `Nothing` is returned.

-}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point2d
pointAlong (ArcLengthParameterized spline parameterization) s =
    ArcLength.toParameterValue parameterization s |> Maybe.map (pointOn spline)


{-| Try to get the tangent direction along a spline at a given arc length. To
get the tangent direction a quarter of the way along `exampleSpline`:

    QuadraticSpline2d.tangentAlong parameterizedSpline
        (arcLength / 4)
    --> Just (Direction2d.fromAngle (degrees 41.145))

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

    QuadraticSpline2d.arcLengthToParameterValue
        parameterizedSpline
        (arcLength / 4)
    --> Just 0.2088

-}
arcLengthToParameterValue : ArcLengthParameterized -> Float -> Maybe Float
arcLengthToParameterValue (ArcLengthParameterized _ parameterization) s =
    ArcLength.toParameterValue parameterization s


{-| Try to get the arc length along a spline at a given parameter value. If the
given parameter value is less than zero or greater than one, returns `Nothing`.

    QuadraticSpline2d.parameterValueToArcLength
        parameterizedSpline
        0.25
    --> Just 1.5122

-}
parameterValueToArcLength : ArcLengthParameterized -> Float -> Maybe Float
parameterValueToArcLength (ArcLengthParameterized _ parameterization) t =
    ArcLength.fromParameterValue parameterization t


{-| Get the second derivative of a spline (for a quadratic spline, this is a
constant).
-}
secondDerivative : QuadraticSpline2d -> Vector2d
secondDerivative spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        v1 =
            Vector2d.from p1 p2

        v2 =
            Vector2d.from p2 p3
    in
    Vector2d.difference v2 v1 |> Vector2d.scaleBy 2
