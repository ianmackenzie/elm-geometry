module OpenSolid.QuadraticSpline3d
    exposing
        ( ArcLengthParameterized
        , QuadraticSpline3d
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
        , on
        , parameterValueToArcLength
        , placeIn
        , pointAlong
        , pointOn
        , projectInto
        , projectOnto
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

{-| <img src="https://opensolid.github.io/images/geometry/icons/quadraticSpline3d.svg" alt="QuadraticSpline3d" width="160">

A `QuadraticSpline3d` is a quadratic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 3D defined by three control points. This module contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs QuadraticSpline3d


# Constructors

@docs fromControlPoints, on


# Properties

@docs controlPoints, startPoint, endPoint, startDerivative, endDerivative


# Evaluation

@docs pointOn, derivative, evaluate


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn


# Sketch planes

@docs projectInto


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentAlong, arcLengthToParameterValue, parameterValueToArcLength


# Low level

Low level functionality that you are unlikely to need to use directly.

@docs derivativeMagnitude, secondDerivative

-}

import OpenSolid.ArcLength as ArcLength
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{-| -}
type alias QuadraticSpline3d =
    Internal.QuadraticSpline3d


{-| Construct a spline from its three control points:

    exampleSpline =
        QuadraticSpline3d.fromControlPoints
            ( Point3d.fromCoordinates ( 1, 1, 1 )
            , Point3d.fromCoordinates ( 3, 2, 1 )
            , Point3d.fromCoordinates ( 3, 3, 3 )
            )

-}
fromControlPoints : ( Point3d, Point3d, Point3d ) -> QuadraticSpline3d
fromControlPoints =
    Internal.QuadraticSpline3d


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
    let
        ( p1, p2, p3 ) =
            QuadraticSpline2d.controlPoints spline2d

        place =
            Point3d.on sketchPlane
    in
    fromControlPoints ( place p1, place p2, place p3 )


{-| Get the control points of a spline as a tuple.

    ( p1, p2, p3 ) =
        QuadraticSpline3d.controlPoints exampleSpline


    --> p1 = Point3d.fromCoordinates ( 1, 1, 1 )
    --> p2 = Point3d.fromCoordinates ( 3, 2, 1 )
    --> p3 = Point3d.fromCoordinates ( 3, 3, 3 )

-}
controlPoints : QuadraticSpline3d -> ( Point3d, Point3d, Point3d )
controlPoints (Internal.QuadraticSpline3d controlPoints_) =
    controlPoints_


{-| Get the start point of a spline. This is equal to the spline's first control
point.

    QuadraticSpline3d.startPoint exampleSpline
    --> Point3d.fromCoordinates ( 1, 1, 1 )

-}
startPoint : QuadraticSpline3d -> Point3d
startPoint (Internal.QuadraticSpline3d ( p1, _, _ )) =
    p1


{-| Get the end point of a spline. This is equal to the spline's last control
point.

    QuadraticSpline3d.endPoint exampleSpline
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
endPoint : QuadraticSpline3d -> Point3d
endPoint (Internal.QuadraticSpline3d ( _, _, p3 )) =
    p3


{-| Get the start derivative of a spline. This is equal to twice the vector from
the spline's first control point to its second.

    QuadraticSpline3d.startDerivative exampleSpline
    --> Vector3d.fromComponents ( 4, 2, 0 )

-}
startDerivative : QuadraticSpline3d -> Vector3d
startDerivative spline =
    let
        ( p1, p2, _ ) =
            controlPoints spline
    in
    Vector3d.from p1 p2 |> Vector3d.scaleBy 2


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline3d.endDerivative exampleSpline
    --> Vector3d.fromComponents ( 0, 2, 4 )

-}
endDerivative : QuadraticSpline3d -> Vector3d
endDerivative spline =
    let
        ( _, p2, p3 ) =
            controlPoints spline
    in
    Vector3d.from p2 p3 |> Vector3d.scaleBy 2


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
pointOn : QuadraticSpline3d -> Float -> Point3d
pointOn spline t =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t
    in
    Point3d.interpolateFrom q1 q2 t


{-| Get the deriative value at a point along a spline, based on a parameter that
ranges from 0 to 1. A parameter value of 0 corresponds to the start derivative
of the spline and a value of 1 corresponds to the end derivative.

    QuadraticSpline3d.derivative exampleSpline 0
    --> Vector3d.fromComponents ( 4, 2, 0 )

    QuadraticSpline3d.derivative exampleSpline 0.5
    --> Vector3d.fromComponents ( 2, 2, 2 )

    QuadraticSpline3d.derivative exampleSpline 1
    --> Vector3d.fromComponents ( 0, 2, 4 )

Note that the derivative interpolates linearly from end to end.

-}
derivative : QuadraticSpline3d -> Float -> Vector3d
derivative spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    \t -> Vector3d.interpolateFrom v1 v2 t |> Vector3d.scaleBy 2


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
        ( p1, p2, p3 ) =
            controlPoints spline

        ( x1, y1, z1 ) =
            Point3d.coordinates p1

        ( x2, y2, z2 ) =
            Point3d.coordinates p2

        ( x3, y3, z3 ) =
            Point3d.coordinates p3

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


{-| Evaluate a spline at a given parameter value, returning the point on the
spline at that parameter value and the derivative with respect to that parameter
value;

    QuadraticSpline3d.evaluate spline t

is equivalent to

    ( QuadraticSpline3d.pointOn spline t
    , QuadraticSpline3d.derivative spline t
    )

but is more efficient.

-}
evaluate : QuadraticSpline3d -> Float -> ( Point3d, Vector3d )
evaluate spline t =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t
    in
    ( Point3d.interpolateFrom q1 q2 t
    , Vector3d.from q1 q2 |> Vector3d.scaleBy 2
    )


mapControlPoints : (Point3d -> Point3d) -> QuadraticSpline3d -> QuadraticSpline3d
mapControlPoints function spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline
    in
    fromControlPoints ( function p1, function p2, function p3 )


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
    let
        ( p1, p2, p3 ) =
            controlPoints spline
    in
    fromControlPoints ( p3, p2, p1 )


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
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        project =
            Point3d.projectInto sketchPlane
    in
    QuadraticSpline2d.fromControlPoints ( project p1, project p2, project p3 )


{-| Split a spline into two roughly equal halves. Equivalent to `splitAt 0.5`.

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

-}
bisect : QuadraticSpline3d -> ( QuadraticSpline3d, QuadraticSpline3d )
bisect =
    splitAt 0.5


{-| Split a spline at a particular parameter value (in the range 0 to 1),
resulting in two smaller splines.

    QuadraticSpline3d.splitAt 0.75 exampleSpline
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
splitAt : Float -> QuadraticSpline3d -> ( QuadraticSpline3d, QuadraticSpline3d )
splitAt t spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t

        r =
            Point3d.interpolateFrom q1 q2 t
    in
    ( fromControlPoints ( p1, q1, r )
    , fromControlPoints ( r, q2, p3 )
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized QuadraticSpline3d ArcLength.Parameterization


{-| Build an arc length parameterization of the given spline, within
a given tolerance. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the given arc length
tolerance.

    tolerance =
        1.0e-4

    parameterizedSpline =
        QuadraticSpline3d.arcLengthParameterized
            tolerance
            exampleSpline

-}
arcLengthParameterized : Float -> QuadraticSpline3d -> ArcLengthParameterized
arcLengthParameterized tolerance spline =
    let
        maxSecondDerivativeMagnitude =
            Vector3d.length (secondDerivative spline)

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
        QuadraticSpline3d.arcLength parameterizedSpline

    arcLength
    --> 3.8175

-}
arcLength : ArcLengthParameterized -> Float
arcLength (ArcLengthParameterized _ parameterization) =
    ArcLength.fromParameterization parameterization


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
pointAlong (ArcLengthParameterized spline parameterization) s =
    ArcLength.toParameterValue parameterization s |> Maybe.map (pointOn spline)


{-| Try to get the tangent direction along a spline at a given arc length. To
get the tangent direction a quarter of the way along `exampleSpline`:

    QuadraticSpline3d.tangentAlong parameterizedSpline
        (arcLength / 4)
    --> Just <|
    -->     Direction3d.with
    -->         { azimuth = degrees 33.091
    -->         , elevation = degrees 14.260
    -->         }

If the given arc length is less than zero or greater than the arc length of the
spline (or if the derivative of the spline happens to be exactly zero at the
given arc length), `Nothing` is returned.

-}
tangentAlong : ArcLengthParameterized -> Float -> Maybe Direction3d
tangentAlong (ArcLengthParameterized spline parameterization) s =
    ArcLength.toParameterValue parameterization s
        |> Maybe.map (derivative spline)
        |> Maybe.andThen Vector3d.direction


{-| Try to get the parameter value along a spline at a given arc length. If the
given arc length is less than zero or greater than the arc length of the spline,
returns `Nothing`.

    QuadraticSpline3d.arcLengthToParameterValue
        parameterizedSpline
        (arcLength / 4)
    --> Just 0.2328

-}
arcLengthToParameterValue : ArcLengthParameterized -> Float -> Maybe Float
arcLengthToParameterValue (ArcLengthParameterized _ parameterization) s =
    ArcLength.toParameterValue parameterization s


{-| Try to get the arc length along a spline at a given parameter value. If the
given parameter value is less than zero or greater than one, returns `Nothing`.

    QuadraticSpline3d.parameterValueToArcLength
        parameterizedSpline
        0.25
    --> Just 1.0192

-}
parameterValueToArcLength : ArcLengthParameterized -> Float -> Maybe Float
parameterValueToArcLength (ArcLengthParameterized _ parameterization) t =
    ArcLength.fromParameterValue parameterization t


{-| Get the second derivative of a spline (for a quadratic spline, this is a
constant).
-}
secondDerivative : QuadraticSpline3d -> Vector3d
secondDerivative spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.difference v2 v1 |> Vector3d.scaleBy 2
