module OpenSolid.QuadraticSpline2d
    exposing
        ( bezier
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

{-| <img src="https://opensolid.github.io/images/geometry/icons/quadraticSpline2d.svg" alt="QuadraticSpline2d" width="160">

A `QuadraticSpline2d` is a quadratic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by three control points. This module contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

Splines can be constructed by passing a tuple of control points to the
`QuadraticSpline2d` constructor, for example

    exampleSpline =
        QuadraticSpline2d
            ( Point2d ( 1, 1 )
            , Point2d ( 3, 4 )
            , Point2d ( 5, 1 )
            )


# Constructors

@docs bezier


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


{-| Construct a spline from its three control points. This is the same as just
using the `QuadraticSpline2d` constructor directly;

    QuadraticSpline2d.bezier p1 p2 p3

is equivalent to

    QuadraticSpline2d ( p1, p2, p3 )

-}
bezier : Point2d -> Point2d -> Point2d -> QuadraticSpline2d
bezier firstPoint secondPoint thirdPoint =
    QuadraticSpline2d ( firstPoint, secondPoint, thirdPoint )


{-| Get the control points of a spline as a tuple.

    ( p1, p2, p3 ) =
        QuadraticSpline2d.controlPoints exampleSpline


    --> p1 = Point2d ( 1, 1 )
    --> p2 = Point2d ( 3, 4 )
    --> p3 = Point2d ( 5, 1 )

-}
controlPoints : QuadraticSpline2d -> ( Point2d, Point2d, Point2d )
controlPoints (QuadraticSpline2d controlPoints_) =
    controlPoints_


{-| Get the start point of a spline. This is equal to the spline's first control
point.

    QuadraticSpline2d.startPoint exampleSpline
    --> Point2d ( 1, 1 )

-}
startPoint : QuadraticSpline2d -> Point2d
startPoint (QuadraticSpline2d ( p1, _, _ )) =
    p1


{-| Get the end point of a spline. This is equal to the spline's last control
point.

    QuadraticSpline2d.endPoint exampleSpline
    --> Point2d ( 5, 1 )

-}
endPoint : QuadraticSpline2d -> Point2d
endPoint (QuadraticSpline2d ( _, _, p3 )) =
    p3


{-| Get the start derivative of a spline. This is equal to twice the vector from
the spline's first control point to its second.

    QuadraticSpline2d.startDerivative exampleSpline
    --> Vector2d ( 4, 6 )

-}
startDerivative : QuadraticSpline2d -> Vector2d
startDerivative spline =
    let
        ( p1, p2, _ ) =
            controlPoints spline
    in
        Point2d.vectorFrom p1 p2 |> Vector2d.scaleBy 2


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline2d.endDerivative exampleSpline
    --> Vector2d ( 4, -6 )

-}
endDerivative : QuadraticSpline2d -> Vector2d
endDerivative spline =
    let
        ( _, p2, p3 ) =
            controlPoints spline
    in
        Point2d.vectorFrom p2 p3 |> Vector2d.scaleBy 2


{-| Get a point along a spline, based on a parameter that ranges from 0 to 1. A
parameter value of 0 corresponds to the start point of the spline and a value of
1 corresponds to the end point.

    QuadraticSpline2d.point exampleSpline 0
    --> Point2d ( 1, 1 )

    QuadraticSpline2d.point exampleSpline 0.5
    --> Point2d ( 3, 2.5 )

    QuadraticSpline2d.point exampleSpline 1
    --> Point2d ( 5, 1 )

-}
point : QuadraticSpline2d -> Float -> Point2d
point spline t =
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
    --> Vector2d ( 4, 6 )

    QuadraticSpline2d.derivative exampleSpline 0.5
    --> Vector2d ( 4, 0 )

    QuadraticSpline2d.derivative exampleSpline 1
    --> Vector2d ( 4, -6 )

Note that the derivative interpolates linearly from end to end.

-}
derivative : QuadraticSpline2d -> Float -> Vector2d
derivative spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        v1 =
            Point2d.vectorFrom p1 p2

        v2 =
            Point2d.vectorFrom p2 p3
    in
        \t -> Vector2d.interpolateFrom v1 v2 t |> Vector2d.scaleBy 2


{-| Scale a spline about the given center point by the given scale.

    QuadraticSpline2d.scaleAbout Point2d.origin 2 exampleSpline
    --> QuadraticSpline2d
    -->     ( Point2d ( 2, 2 )
    -->     , Point2d ( 6, 8 )
    -->     , Point2d ( 10, 2 )
    -->     )

-}
scaleAbout : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given center point by a given
angle (in radians).

    QuadraticSpline2d.rotateAround Point2d.origin (degrees 90) exampleSpline
    --> QuadraticSpline2d
    -->     ( Point2d ( -1, 1 )
    -->     , Point2d ( -4, 3 )
    -->     , Point2d ( -1, 5 )
    -->     )

-}
rotateAround : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector2d ( 2, 3 )

    QuadraticSpline2d.translateBy displacement exampleSpline
    --> QuadraticSpline2d
    -->     ( Point2d ( 3, 4 )
    -->     , Point2d ( 5, 7 )
    -->     , Point2d ( 7, 4 )
    -->     )

-}
translateBy : Vector2d -> QuadraticSpline2d -> QuadraticSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


{-| Mirror a spline across an axis.

    QuadraticSpline2d.mirrorAcross Axis2d.x exampleSpline
    --> QuadraticSpline2d
    -->     ( Point2d ( 1, -1 )
    -->     , Point2d ( 3, -4 )
    -->     , Point2d ( 5, -1 )
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
        QuadraticSpline2d ( function p1, function p2, function p3 )


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    QuadraticSpline2d.relativeTo localFrame exampleSpline
    --> QuadraticSpline2d
    -->     ( Point2d ( 0, -1 )
    -->     , Point2d ( 2, 2 )
    -->     , Point2d ( 4, -1 )
    -->     )

-}
relativeTo : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    QuadraticSpline2d.placeIn localFrame exampleSpline
    --> QuadraticSpline2d
    -->     ( Point2d ( 2, 3 )
    -->     , Point2d ( 4, 6 )
    -->     , Point2d ( 6, 3 )
    -->     )

-}
placeIn : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


{-| Take a spline defined in 2D coordinates within a particular sketch
plane and return the corresponding spline in 3D.

    QuadraticSpline2d.placeOnto SketchPlane3d.xz exampleSpline
    --> QuadraticSpline3d
    -->     ( Point3d ( 1, 0, 1 )
    -->     , Point3d ( 3, 0, 4 )
    -->     , Point3d ( 5, 0, 1 )
    -->     )

-}
placeOnto : SketchPlane3d -> QuadraticSpline2d -> QuadraticSpline3d
placeOnto sketchPlane spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        place =
            Point2d.placeOnto sketchPlane
    in
        QuadraticSpline3d ( place p1, place p2, place p3 )
