module OpenSolid.CubicSpline2d
    exposing
        ( CubicSpline2d
        , bisect
        , controlPoints
        , derivative
        , endDerivative
        , endPoint
        , evaluate
        , hermite
        , mirrorAcross
        , placeIn
        , pointOn
        , relativeTo
        , reverse
        , rotateAround
        , scaleAbout
        , splitAt
        , startDerivative
        , startPoint
        , translateBy
        , withControlPoints
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

@docs withControlPoints, hermite


# Accessors

@docs controlPoints, startPoint, endPoint, startDerivative, endDerivative


# Evaluation

@docs pointOn, derivative, evaluate


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn


# Subdivision

@docs bisect, splitAt

-}

import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


{-| -}
type alias CubicSpline2d =
    Internal.CubicSpline2d


{-| Construct a spline from its four control points:

    exampleSpline =
        CubicSpline2d.withControlPoints
            ( Point2d.withCoordinates ( 1, 1 )
            , Point2d.withCoordinates ( 3, 4 )
            , Point2d.withCoordinates ( 5, 1 )
            , Point2d.withCoordinates ( 7, 4 )
            )

-}
withControlPoints : ( Point2d, Point2d, Point2d, Point2d ) -> CubicSpline2d
withControlPoints =
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
    withControlPoints
        ( startPoint
        , startControlPoint
        , endControlPoint
        , endPoint
        )


{-| Get the control points of a spline as a tuple.

    ( p1, p2, p3, p4 ) =
        CubicSpline2d.controlPoints exampleSpline


    --> p1 = Point2d.withCoordinates ( 1, 1 )
    --> p2 = Point2d.withCoordinates ( 3, 4 )
    --> p3 = Point2d.withCoordinates ( 5, 1 )
    --> p4 = Point2d.withCoordinates ( 7, 4 )

-}
controlPoints : CubicSpline2d -> ( Point2d, Point2d, Point2d, Point2d )
controlPoints (Internal.CubicSpline2d controlPoints_) =
    controlPoints_


{-| Get the start point of a spline. This is equal to the spline's first control
point.

    CubicSpline2d.startPoint exampleSpline
    --> Point2d.withCoordinates ( 1, 1 )

-}
startPoint : CubicSpline2d -> Point2d
startPoint (Internal.CubicSpline2d ( p1, _, _, _ )) =
    p1


{-| Get the end point of a spline. This is equal to the spline's last control
point.

    CubicSpline2d.endPoint exampleSpline
    --> Point2d.withCoordinates ( 7, 4 )

-}
endPoint : CubicSpline2d -> Point2d
endPoint (Internal.CubicSpline2d ( _, _, _, p4 )) =
    p4


{-| Get the start derivative of a spline. This is equal to three times the
vector from the spline's first control point to its second.

    CubicSpline2d.startDerivative exampleSpline
    --> Vector2d.withComponents ( 6, 9 )

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
    --> Vector2d.withComponents ( 6, 9 )

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
    --> Point2d.withCoordinates ( 1, 1 )

    CubicSpline2d.pointOn exampleSpline 0.5
    --> Point2d.withCoordinates ( 4, 2.5 )

    CubicSpline2d.pointOn exampleSpline 1
    --> Point2d.withCoordinates ( 7, 4 )

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
    --> Vector2d.withComponents ( 6, 9 )

    CubicSpline2d.derivative exampleSpline 0.5
    --> Vector2d.withComponents ( 6, 0 )

    CubicSpline2d.derivative exampleSpline 1
    --> Vector2d.withComponents ( 6, 9 )

-}
derivative : CubicSpline2d -> Float -> Vector2d
derivative spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        v1 =
            Vector2d.from p1 p2

        v2 =
            Vector2d.from p2 p3

        v3 =
            Vector2d.from p3 p4
    in
    \t ->
        let
            w1 =
                Vector2d.interpolateFrom v1 v2 t

            w2 =
                Vector2d.interpolateFrom v2 v3 t
        in
        Vector2d.interpolateFrom w1 w2 t |> Vector2d.scaleBy 3


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
    withControlPoints ( function p1, function p2, function p3, function p4 )


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    CubicSpline2d.reverse exampleSpline
    --> CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 7, 4 )
    -->     , Point2d.withCoordinates ( 5, 1 )
    -->     , Point2d.withCoordinates ( 3, 4 )
    -->     , Point2d.withCoordinates ( 1, 1 )
    -->     )

-}
reverse : CubicSpline2d -> CubicSpline2d
reverse spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline
    in
    withControlPoints ( p4, p3, p2, p1 )


{-| Scale a spline about the given center point by the given scale.

    CubicSpline2d.scaleAbout Point2d.origin 2 exampleSpline
    --> CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 2, 2 )
    -->     , Point2d.withCoordinates ( 6, 8 )
    -->     , Point2d.withCoordinates ( 10, 2 )
    -->     , Point2d.withCoordinates ( 14, 8 )
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
    --> CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( -1, 1 )
    -->     , Point2d.withCoordinates ( -4, 3 )
    -->     , Point2d.withCoordinates ( -1, 5 )
    -->     , Point2d.withCoordinates ( -4, 7 )
    -->     )

-}
rotateAround : Point2d -> Float -> CubicSpline2d -> CubicSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector2d.withComponents ( 2, 3 )

    CubicSpline2d.translateBy displacement exampleSpline
    --> CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 3, 4 )
    -->     , Point2d.withCoordinates ( 5, 7 )
    -->     , Point2d.withCoordinates ( 7, 4 )
    -->     , Point2d.withCoordinates ( 9, 7 )
    -->     )

-}
translateBy : Vector2d -> CubicSpline2d -> CubicSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


{-| Mirror a spline across an axis.

    CubicSpline2d.mirrorAcross Axis2d.x exampleSpline
    --> CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 1, -1 )
    -->     , Point2d.withCoordinates ( 3, -4 )
    -->     , Point2d.withCoordinates ( 5, -1 )
    -->     , Point2d.withCoordinates ( 7, -4 )
    -->     )

-}
mirrorAcross : Axis2d -> CubicSpline2d -> CubicSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d.withCoordinates ( 1, 2 ))

    CubicSpline2d.relativeTo localFrame exampleSpline
    --> CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 0, -1 )
    -->     , Point2d.withCoordinates ( 2, 2 )
    -->     , Point2d.withCoordinates ( 4, -1 )
    -->     , Point2d.withCoordinates ( 6, 2 )
    -->     )

-}
relativeTo : Frame2d -> CubicSpline2d -> CubicSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame2d.at (Point2d.withCoordinates ( 1, 2 ))

    CubicSpline2d.placeIn localFrame exampleSpline
    --> CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 2, 3 )
    -->     , Point2d.withCoordinates ( 4, 6 )
    -->     , Point2d.withCoordinates ( 6, 3 )
    -->     , Point2d.withCoordinates ( 8, 6 )
    -->     )

-}
placeIn : Frame2d -> CubicSpline2d -> CubicSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


{-| Split a spline into two roughly equal halves. Equivalent to `splitAt 0.5`.

    CubicSpline2d.bisect exampleSpline
    --> ( CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 1, 1 )
    -->     , Point2d.withCoordinates ( 2, 2.5 )
    -->     , Point2d.withCoordinates ( 3, 2.5 )
    -->     , Point2d.withCoordinates ( 4, 2.5 )
    -->     )
    --> , CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 4, 2.5 )
    -->     , Point2d.withCoordinates ( 5, 2.5 )
    -->     , Point2d.withCoordinates ( 6, 2.5 )
    -->     , Point2d.withCoordinates ( 7, 4 )
    -->     )
    --> )

-}
bisect : CubicSpline2d -> ( CubicSpline2d, CubicSpline2d )
bisect =
    splitAt 0.5


{-| Split a spline at a particular parameter value (in the range 0 to 1),
resulting in two smaller splines.

    CubicSpline2d.splitAt 0.75 exampleSpline
    --> ( CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 1, 1 )
    -->     , Point2d.withCoordinates ( 2.5, 3.25 )
    -->     , Point2d.withCoordinates ( 4, 2.125 )
    -->     , Point2d.withCoordinates ( 5.5, 2.6875 )
    -->     )
    --> , CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 5.5, 2.6875 )
    -->     , Point2d.withCoordinates ( 6, 2.875 )
    -->     , Point2d.withCoordinates ( 6.5, 3.25 )
    -->     , Point2d.withCoordinates ( 7, 4 )
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
    ( withControlPoints ( p1, q1, r1, s )
    , withControlPoints ( s, r2, q3, p4 )
    )
