module OpenSolid.CubicSpline3d
    exposing
        ( CubicSpline3d
        , bisect
        , controlPoints
        , derivative
        , endDerivative
        , endPoint
        , evaluate
        , hermite
        , mirrorAcross
        , on
        , placeIn
        , pointOn
        , projectInto
        , projectOnto
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

{-| <img src="https://opensolid.github.io/images/geometry/icons/cubicSpline3d.svg" alt="CubicSpline3d" width="160">

A `CubicSpline3d` is a cubic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 3D defined by four control points. This module contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs CubicSpline3d


# Constructors

@docs withControlPoints, hermite, on


# Accessors

@docs controlPoints, startPoint, endPoint, startDerivative, endDerivative


# Evaluation

@docs pointOn, derivative, evaluate


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto


# Coordinate frames

@docs relativeTo, placeIn


# Sketch planes

@docs projectInto


# Subdivision

@docs bisect, splitAt

-}

import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{-| -}
type alias CubicSpline3d =
    Internal.CubicSpline3d


{-| Construct a spline from its four control points:

    exampleSpline =
        CubicSpline3d.withControlPoints
            ( Point3d.withCoordinates ( 1, 1, 1 )
            , Point3d.withCoordinates ( 3, 1, 1 )
            , Point3d.withCoordinates ( 3, 3, 1 )
            , Point3d.withCoordinates ( 3, 3, 3 )
            )

-}
withControlPoints : ( Point3d, Point3d, Point3d, Point3d ) -> CubicSpline3d
withControlPoints =
    Internal.CubicSpline3d


{-| Construct a spline in Hermite form, from the position and derivative values
at its start and end points, like so:

![Hermite cubic spline](https://opensolid.github.io/images/geometry/1.2/hermiteCubicSpline.svg)

The spline is based on a parameter that ranges from 0 to 1; as a result, in most
cases the length of each derivative vector should be roughly equal to the length
of the resulting spline.

-}
hermite : ( Point3d, Vector3d ) -> ( Point3d, Vector3d ) -> CubicSpline3d
hermite start end =
    let
        ( startPoint, startDerivative ) =
            start

        ( endPoint, endDerivative ) =
            end

        startControlPoint =
            startPoint
                |> Point3d.translateBy
                    (Vector3d.scaleBy (1 / 3) startDerivative)

        endControlPoint =
            endPoint
                |> Point3d.translateBy
                    (Vector3d.scaleBy (-1 / 3) endDerivative)
    in
    withControlPoints
        ( startPoint
        , startControlPoint
        , endControlPoint
        , endPoint
        )


{-| Construct a 3D spline lying _on_ a sketch plane by providing a 2D spline
specified in XY coordinates _within_ the sketch plane.

    CubicSpline3d.on SketchPlane3d.xz <|
        CubicSpline2d.withControlPoints
            ( Point2d.withCoordinates ( 1, 1 )
            , Point2d.withCoordinates ( 3, 4 )
            , Point2d.withCoordinates ( 5, 1 )
            , Point2d.withCoordinates ( 7, 4 )
            )
    --> CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 1, 0, 1 )
    -->     , Point3d.withCoordinates ( 3, 0, 4 )
    -->     , Point3d.withCoordinates ( 5, 0, 1 )
    -->     , Point3d.withCoordinates ( 7, 0, 4 )
    -->     )

-}
on : SketchPlane3d -> CubicSpline2d -> CubicSpline3d
on sketchPlane spline =
    let
        ( p1, p2, p3, p4 ) =
            CubicSpline2d.controlPoints spline

        place =
            Point3d.on sketchPlane
    in
    withControlPoints ( place p1, place p2, place p3, place p4 )


{-| Get the control points of a spline as a tuple.

    ( p1, p2, p3, p4 ) =
        CubicSpline3d.controlPoints exampleSpline


    --> p1 = Point3d.withCoordinates ( 1, 1, 1 )
    --> p2 = Point3d.withCoordinates ( 3, 1, 1 )
    --> p3 = Point3d.withCoordinates ( 3, 3, 1 )
    --> p4 = Point3d.withCoordinates ( 3, 3, 3 )

-}
controlPoints : CubicSpline3d -> ( Point3d, Point3d, Point3d, Point3d )
controlPoints (Internal.CubicSpline3d controlPoints_) =
    controlPoints_


{-| Get the start point of a spline. This is equal to the spline's first control
point.

    CubicSpline3d.startPoint exampleSpline
    --> Point3d.withCoordinates ( 1, 1, 1 )

-}
startPoint : CubicSpline3d -> Point3d
startPoint (Internal.CubicSpline3d ( p1, _, _, _ )) =
    p1


{-| Get the end point of a spline. This is equal to the spline's last control
point.

    CubicSpline3d.endPoint exampleSpline
    --> Point3d.withCoordinates ( 3, 3, 3 )

-}
endPoint : CubicSpline3d -> Point3d
endPoint (Internal.CubicSpline3d ( _, _, _, p4 )) =
    p4


{-| Get the start derivative of a spline. This is equal to three times the
vector from the spline's first control point to its second.

    CubicSpline3d.startDerivative exampleSpline
    --> Vector3d.withComponents ( 6, 0, 0 )

-}
startDerivative : CubicSpline3d -> Vector3d
startDerivative spline =
    let
        ( p1, p2, _, _ ) =
            controlPoints spline
    in
    Vector3d.from p1 p2 |> Vector3d.scaleBy 3


{-| Get the end derivative of a spline. This is equal to three times the vector
from the spline's third control point to its fourth.

    CubicSpline3d.endDerivative exampleSpline
    --> Vector3d.withComponents ( 0, 0, 6 )

-}
endDerivative : CubicSpline3d -> Vector3d
endDerivative spline =
    let
        ( _, _, p3, p4 ) =
            controlPoints spline
    in
    Vector3d.from p3 p4 |> Vector3d.scaleBy 3


{-| Get a point along a spline, based on a parameter that ranges from 0 to 1. A
parameter value of 0 corresponds to the start point of the spline and a value of
1 corresponds to the end point.

    CubicSpline3d.pointOn exampleSpline 0
    --> Point3d.withCoordinates ( 1, 1, 1 )

    CubicSpline3d.pointOn exampleSpline 0.5
    --> Point3d.withCoordinates ( 2.75, 2, 1.25 )

    CubicSpline3d.pointOn exampleSpline 1
    --> Point3d.withCoordinates ( 3, 3, 3 )

-}
pointOn : CubicSpline3d -> Float -> Point3d
pointOn spline t =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

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


{-| Get the deriative value at a point along a spline, based on a parameter that
ranges from 0 to 1. A parameter value of 0 corresponds to the start derivative
of the spline and a value of 1 corresponds to the end derivative.

    CubicSpline3d.derivative exampleSpline 0
    --> Vector3d.withComponents ( 6, 0, 0 )

    CubicSpline3d.derivative exampleSpline 0.5
    --> Vector3d.withComponents ( 1.5, 3, 1.5 )

    CubicSpline3d.derivative exampleSpline 1
    --> Vector3d.withComponents ( 0, 0, 6 )

Note that the derivative interpolates linearly from end to end.

-}
derivative : CubicSpline3d -> Float -> Vector3d
derivative spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3

        v3 =
            Vector3d.from p3 p4
    in
    \t ->
        let
            w1 =
                Vector3d.interpolateFrom v1 v2 t

            w2 =
                Vector3d.interpolateFrom v2 v3 t
        in
        Vector3d.interpolateFrom w1 w2 t |> Vector3d.scaleBy 3


{-| Evaluate a spline at a given parameter value, returning the point on the
spline at that parameter value and the derivative with respect to that parameter
value;

    CubicSpline3d.evaluate spline t

is equivalent to

    ( CubicSpline3d.pointOn spline t
    , CubicSpline3d.derivative spline t
    )

but is more efficient.

-}
evaluate : CubicSpline3d -> Float -> ( Point3d, Vector3d )
evaluate spline t =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

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
    ( Point3d.interpolateFrom r1 r2 t
    , Vector3d.from r1 r2 |> Vector3d.scaleBy 3
    )


mapControlPoints : (Point3d -> Point3d) -> CubicSpline3d -> CubicSpline3d
mapControlPoints function spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline
    in
    withControlPoints ( function p1, function p2, function p3, function p4 )


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    CubicSpline3d.reverse exampleSpline
    --> CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 3, 3, 3 )
    -->     , Point3d.withCoordinates ( 3, 3, 1 )
    -->     , Point3d.withCoordinates ( 3, 1, 1 )
    -->     , Point3d.withCoordinates ( 1, 1, 1 )
    -->     )

-}
reverse : CubicSpline3d -> CubicSpline3d
reverse spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline
    in
    withControlPoints ( p4, p3, p2, p1 )


{-| Scale a spline about the given center point by the given scale.

    CubicSpline3d.scaleAbout Point3d.origin 2 exampleSpline
    --> CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 2, 2, 2 )
    -->     , Point3d.withCoordinates ( 6, 2, 2 )
    -->     , Point3d.withCoordinates ( 6, 6, 2 )
    -->     , Point3d.withCoordinates ( 6, 6, 6 )
    -->     )

-}
scaleAbout : Point3d -> Float -> CubicSpline3d -> CubicSpline3d
scaleAbout point scale =
    mapControlPoints (Point3d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given axis by a given angle (in
radians).

    exampleSpline
        |> CubicSpline3d.rotateAround Axis3d.z (degrees 90)
    --> CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( -1, 1, 1 )
    -->     , Point3d.withCoordinates ( -1, 3, 1 )
    -->     , Point3d.withCoordinates ( -3, 3, 1 )
    -->     , Point3d.withCoordinates ( -3, 3, 3 )
    -->     )

-}
rotateAround : Axis3d -> Float -> CubicSpline3d -> CubicSpline3d
rotateAround axis angle =
    mapControlPoints (Point3d.rotateAround axis angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector3d.withComponents ( 2, 3, 1 )

    CubicSpline3d.translateBy displacement exampleSpline
    --> CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 3, 4, 2 )
    -->     , Point3d.withCoordinates ( 5, 4, 2 )
    -->     , Point3d.withCoordinates ( 5, 6, 2 )
    -->     , Point3d.withCoordinates ( 5, 6, 4 )
    -->     )

-}
translateBy : Vector3d -> CubicSpline3d -> CubicSpline3d
translateBy displacement =
    mapControlPoints (Point3d.translateBy displacement)


{-| Mirror a spline across a plane.

    CubicSpline3d.mirrorAcross Plane3d.xy exampleSpline
    --> CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 1, 1, -1 )
    -->     , Point3d.withCoordinates ( 3, 1, -1 )
    -->     , Point3d.withCoordinates ( 3, 3, -1 )
    -->     , Point3d.withCoordinates ( 3, 3, -3 )
    -->     )

-}
mirrorAcross : Plane3d -> CubicSpline3d -> CubicSpline3d
mirrorAcross plane =
    mapControlPoints (Point3d.mirrorAcross plane)


{-| Project a spline onto a plane.

    CubicSpline3d.projectOnto Plane3d.xy exampleSpline
    --> CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 1, 1, 0 )
    -->     , Point3d.withCoordinates ( 3, 1, 0 )
    -->     , Point3d.withCoordinates ( 3, 3, 0 )
    -->     , Point3d.withCoordinates ( 3, 3, 0 )
    -->     )

-}
projectOnto : Plane3d -> CubicSpline3d -> CubicSpline3d
projectOnto plane =
    mapControlPoints (Point3d.projectOnto plane)


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.at (Point3d.withCoordinates ( 1, 2, 3 ))

    CubicSpline3d.relativeTo localFrame exampleSpline
    --> CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 0, -1, -2 )
    -->     , Point3d.withCoordinates ( 2, -1, -2 )
    -->     , Point3d.withCoordinates ( 2, 1, -2 )
    -->     , Point3d.withCoordinates ( 2, 1, 0 )
    -->     )

-}
relativeTo : Frame3d -> CubicSpline3d -> CubicSpline3d
relativeTo frame =
    mapControlPoints (Point3d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame3d.at (Point3d.withCoordinates ( 1, 2, 3 ))

    CubicSpline3d.placeIn localFrame exampleSpline
    --> CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 2, 3, 4 )
    -->     , Point3d.withCoordinates ( 4, 3, 4 )
    -->     , Point3d.withCoordinates ( 4, 5, 4 )
    -->     , Point3d.withCoordinates ( 4, 5, 6 )
    -->     )

-}
placeIn : Frame3d -> CubicSpline3d -> CubicSpline3d
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| Project a spline into a given sketch plane. Conceptually, this
projects the spline onto the plane and then expresses the projected
spline in 2D sketch coordinates.

    exampleSpline
        |> CubicSpline3d.projectInto SketchPlane3d.yz
    --> CubicSpline2d.withControlPoints
    -->     ( Point2d.withCoordinates ( 1, 1 )
    -->     , Point2d.withCoordinates ( 1, 1 )
    -->     , Point2d.withCoordinates ( 3, 1 )
    -->     , Point2d.withCoordinates ( 3, 3 )
    -->     )

-}
projectInto : SketchPlane3d -> CubicSpline3d -> CubicSpline2d
projectInto sketchPlane spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        project =
            Point3d.projectInto sketchPlane
    in
    CubicSpline2d.withControlPoints
        ( project p1
        , project p2
        , project p3
        , project p4
        )


{-| Split a spline into two roughly equal halves. Equivalent to `splitAt 0.5`.

    CubicSpline3d.bisect exampleSpline
    --> ( CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 1, 1, 1 )
    -->     , Point3d.withCoordinates ( 2, 1, 1 )
    -->     , Point3d.withCoordinates ( 2.5, 1.5, 1 )
    -->     , Point3d.withCoordinates ( 2.75, 2, 1.25 )
    -->     )
    --> , CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 2.75, 2, 1.25 )
    -->     , Point3d.withCoordinates ( 3, 2.5, 1.5 )
    -->     , Point3d.withCoordinates ( 3, 3, 2 )
    -->     , Point3d.withCoordinates ( 3, 3, 3 )
    -->     )
    --> )

-}
bisect : CubicSpline3d -> ( CubicSpline3d, CubicSpline3d )
bisect =
    splitAt 0.5


{-| Split a spline at a particular parameter value (in the range 0 to 1),
resulting in two smaller splines.

    CubicSpline3d.splitAt 0.75 exampleSpline
    --> ( CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 1, 1, 1 )
    -->     , Point3d.withCoordinates ( 2.5, 1, 1 )
    -->     , Point3d.withCoordinates ( 2.88, 2.13, 1 )
    -->     , Point3d.withCoordinates ( 2.97, 2.69, 1.84 )
    -->     )
    --> , CubicSpline3d.withControlPoints
    -->     ( Point3d.withCoordinates ( 2.97, 2.69, 1.84 )
    -->     , Point3d.withCoordinates ( 3, 2.88, 2.13 )
    -->     , Point3d.withCoordinates ( 3, 3, 2.5 )
    -->     , Point3d.withCoordinates ( 3, 3, 3 )
    -->     )
    --> )

-}
splitAt : Float -> CubicSpline3d -> ( CubicSpline3d, CubicSpline3d )
splitAt t spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

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
    ( withControlPoints ( p1, q1, r1, s )
    , withControlPoints ( s, r2, q3, p4 )
    )
