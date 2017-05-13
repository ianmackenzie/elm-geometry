module OpenSolid.CubicSpline2d
    exposing
        ( bezier
        , hermite
        , controlPoints
        , startPoint
        , endPoint
        , startDerivative
        , endDerivative
        , point
        , derivative
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        , placeOnto
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/cubicSpline2d.svg" alt="CubicSpline2d" width="160">

A `CubicSpline2d` is a cubic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by four control points. This module contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

Splines can be constructed by passing a tuple of control points to the
`CubicSpline2d` constructor, for example

    exampleSpline =
        CubicSpline2d
            ( Point2d ( 1, 1 )
            , Point2d ( 3, 4 )
            , Point2d ( 5, 1 )
            , Point2d ( 7, 4 )
            )


# Constructors

@docs bezier, hermite


# Accessors

@docs controlPoints, startPoint, endPoint, startDerivative, endDerivative


# Evaluation

@docs point, derivative


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn


# Sketch planes

@docs placeOnto

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d


{-| Construct a spline from its four control points. This is the same as just
using the `CubicSpline2d` constructor directly;

    CubicSpline2d.bezier p1 p2 p3 p4

is equivalent to

    CubicSpline2d ( p1, p2, p3, p4 )

-}
bezier : Point2d -> Point2d -> Point2d -> Point2d -> CubicSpline2d
bezier firstPoint secondPoint thirdPoint fourthPoint =
    CubicSpline2d ( firstPoint, secondPoint, thirdPoint, fourthPoint )


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
        bezier startPoint startControlPoint endControlPoint endPoint


{-| Get the control points of a spline as a tuple.

    ( p1, p2, p3, p4 ) =
        CubicSpline2d.controlPoints exampleSpline


    --> p1 = Point2d ( 1, 1 )
    --> p2 = Point2d ( 3, 4 )
    --> p3 = Point2d ( 5, 1 )
    --> p4 = Point2d ( 7, 4 )

-}
controlPoints : CubicSpline2d -> ( Point2d, Point2d, Point2d, Point2d )
controlPoints (CubicSpline2d controlPoints_) =
    controlPoints_


{-| Get the start point of a spline. This is equal to the spline's first control
point.

    CubicSpline2d.startPoint exampleSpline
    --> Point2d ( 1, 1 )

-}
startPoint : CubicSpline2d -> Point2d
startPoint (CubicSpline2d ( p1, _, _, _ )) =
    p1


{-| Get the end point of a spline. This is equal to the spline's last control
point.

    CubicSpline2d.endPoint exampleSpline
    --> Point2d ( 7, 4 )

-}
endPoint : CubicSpline2d -> Point2d
endPoint (CubicSpline2d ( _, _, _, p4 )) =
    p4


{-| Get the start derivative of a spline. This is equal to three times the
vector from the spline's first control point to its second.

    CubicSpline2d.startDerivative exampleSpline
    --> Vector2d ( 6, 9 )

-}
startDerivative : CubicSpline2d -> Vector2d
startDerivative spline =
    let
        ( p1, p2, _, _ ) =
            controlPoints spline
    in
        Point2d.vectorFrom p1 p2 |> Vector2d.scaleBy 3


{-| Get the end derivative of a spline. This is equal to three times the vector
from the spline's third control point to its fourth.

    CubicSpline2d.endDerivative exampleSpline
    --> Vector2d ( 6, 9 )

-}
endDerivative : CubicSpline2d -> Vector2d
endDerivative spline =
    let
        ( _, _, p3, p4 ) =
            controlPoints spline
    in
        Point2d.vectorFrom p3 p4 |> Vector2d.scaleBy 3


{-| Get a point along a spline, based on a parameter that ranges from 0 to 1. A
parameter value of 0 corresponds to the start point of the spline and a value of
1 corresponds to the end point.

    CubicSpline2d.point exampleSpline 0
    --> Point2d ( 1, 1 )

    CubicSpline2d.point exampleSpline 0.5
    --> Point2d ( 4, 2.5 )

    CubicSpline2d.point exampleSpline 1
    --> Point2d ( 7, 4 )

-}
point : CubicSpline2d -> Float -> Point2d
point spline t =
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
    --> Vector2d ( 6, 9 )

    CubicSpline2d.derivative exampleSpline 0.5
    --> Vector2d ( 6, 0 )

    CubicSpline2d.derivative exampleSpline 1
    --> Vector2d ( 6, 9 )

-}
derivative : CubicSpline2d -> Float -> Vector2d
derivative spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        v1 =
            Point2d.vectorFrom p1 p2

        v2 =
            Point2d.vectorFrom p2 p3

        v3 =
            Point2d.vectorFrom p3 p4
    in
        \t ->
            let
                w1 =
                    Vector2d.interpolateFrom v1 v2 t

                w2 =
                    Vector2d.interpolateFrom v2 v3 t
            in
                Vector2d.interpolateFrom w1 w2 t |> Vector2d.scaleBy 3


mapControlPoints : (Point2d -> Point2d) -> CubicSpline2d -> CubicSpline2d
mapControlPoints function spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline
    in
        CubicSpline2d ( function p1, function p2, function p3, function p4 )


{-| Scale a spline about the given center point by the given scale.

    CubicSpline2d.scaleAbout Point2d.origin 2 exampleSpline
    --> CubicSpline2d
    -->     ( Point2d ( 2, 2 )
    -->     , Point2d ( 6, 8 )
    -->     , Point2d ( 10, 2 )
    -->     , Point2d ( 14, 8 )
    -->     )

-}
scaleAbout : Point2d -> Float -> CubicSpline2d -> CubicSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given center point by a given
angle (in radians).

    CubicSpline2d.rotateAround Point2d.origin (degrees 90) exampleSpline
    --> CubicSpline2d
    -->     ( Point2d ( -1, 1 )
    -->     , Point2d ( -4, 3 )
    -->     , Point2d ( -1, 5 )
    -->     , Point2d ( -4, 7 )
    -->     )

-}
rotateAround : Point2d -> Float -> CubicSpline2d -> CubicSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector2d ( 2, 3 )

    CubicSpline2d.translateBy displacement exampleSpline
    --> CubicSpline2d
    -->     ( Point2d ( 3, 4 )
    -->     , Point2d ( 5, 7 )
    -->     , Point2d ( 7, 4 )
    -->     , Point2d ( 9, 7 )
    -->     )

-}
translateBy : Vector2d -> CubicSpline2d -> CubicSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


{-| Mirror a spline across an axis.

    CubicSpline2d.mirrorAcross Axis2d.x exampleSpline
    --> CubicSpline2d
    -->     ( Point2d ( 1, -1 )
    -->     , Point2d ( 3, -4 )
    -->     , Point2d ( 5, -1 )
    -->     , Point2d ( 7, -4 )
    -->     )

-}
mirrorAcross : Axis2d -> CubicSpline2d -> CubicSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    CubicSpline2d.relativeTo localFrame exampleSpline
    --> CubicSpline2d
    -->     ( Point2d ( 0, -1 )
    -->     , Point2d ( 2, 2 )
    -->     , Point2d ( 4, -1 )
    -->     , Point2d ( 6, 2 )
    -->     )

-}
relativeTo : Frame2d -> CubicSpline2d -> CubicSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    CubicSpline2d.placeIn localFrame exampleSpline
    --> CubicSpline2d
    -->     ( Point2d ( 2, 3 )
    -->     , Point2d ( 4, 6 )
    -->     , Point2d ( 6, 3 )
    -->     , Point2d ( 8, 6 )
    -->     )

-}
placeIn : Frame2d -> CubicSpline2d -> CubicSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


{-| Take a spline defined in 2D coordinates within a particular sketch
plane and return the corresponding spline in 3D.

    CubicSpline2d.placeOnto SketchPlane3d.xz exampleSpline
    --> CubicSpline3d
    -->     ( Point3d ( 1, 0, 1 )
    -->     , Point3d ( 3, 0, 4 )
    -->     , Point3d ( 5, 0, 1 )
    -->     , Point3d ( 7, 0, 4 )
    -->     )

-}
placeOnto : SketchPlane3d -> CubicSpline2d -> CubicSpline3d
placeOnto sketchPlane spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        place =
            Point2d.placeOnto sketchPlane
    in
        CubicSpline3d ( place p1, place p2, place p3, place p4 )
