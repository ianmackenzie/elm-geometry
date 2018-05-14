module QuadraticSpline2d
    exposing
        ( ArcLengthParameterized
        , QuadraticSpline2d
        , arcLength
        , arcLengthParameterized
        , arcLengthToParameterValue
        , bisect
        , boundingBox
        , controlPoint
        , derivative
        , derivativeMagnitude
        , derivatives
        , endDerivative
        , endPoint
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
        , startDerivative
        , startPoint
        , tangentAlong
        , translateBy
        , translateIn
        , underlyingSpline
        , with
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/QuadraticSpline2d/icon.svg" alt="QuadraticSpline2d" width="160">

A `QuadraticSpline2d` is a quadratic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by a start point, control point and end point. This module
contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs QuadraticSpline2d


# Constructors

@docs with


# Properties

@docs startPoint, endPoint, controlPoint, startDerivative, endDerivative, boundingBox


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

@docs derivativeMagnitude, secondDerivative

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Accuracy exposing (Accuracy)
import Geometry.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias QuadraticSpline2d =
    Types.QuadraticSpline2d


{-| Construct a spline from its start point, control point and end point:

    exampleSpline =
        QuadraticSpline2d.with
            { startPoint = Point2d.fromCoordinates ( 1, 1 )
            , controlPoint = Point2d.fromCoordinates ( 3, 4 )
            , endPoint = Point2d.fromCoordinates ( 5, 1 )
            }

-}
with : { startPoint : Point2d, controlPoint : Point2d, endPoint : Point2d } -> QuadraticSpline2d
with =
    Types.QuadraticSpline2d


{-| Get the start point of a spline.

    QuadraticSpline2d.startPoint exampleSpline
    --> Point2d.fromCoordinates ( 1, 1 )

-}
startPoint : QuadraticSpline2d -> Point2d
startPoint (Types.QuadraticSpline2d spline) =
    spline.startPoint


{-| Get the end point of a spline.

    QuadraticSpline2d.endPoint exampleSpline
    --> Point2d.fromCoordinates ( 5, 1 )

-}
endPoint : QuadraticSpline2d -> Point2d
endPoint (Types.QuadraticSpline2d spline) =
    spline.endPoint


{-| Get the control point of a spline.

    QuadraticSpline2d.controlPoint exampleSpline
    --> Point2d.fromCoordinates ( 3, 4 )

-}
controlPoint : QuadraticSpline2d -> Point2d
controlPoint (Types.QuadraticSpline2d spline) =
    spline.controlPoint


{-| Get the start derivative of a spline. This is equal to twice the vector from
the spline's first control point to its second.

    QuadraticSpline2d.startDerivative exampleSpline
    --> Vector2d.fromComponents ( 4, 6 )

-}
startDerivative : QuadraticSpline2d -> Vector2d
startDerivative spline =
    Vector2d.from (startPoint spline) (controlPoint spline)
        |> Vector2d.scaleBy 2


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline2d.endDerivative exampleSpline
    --> Vector2d.fromComponents ( 4, -6 )

-}
endDerivative : QuadraticSpline2d -> Vector2d
endDerivative spline =
    Vector2d.from (controlPoint spline) (endPoint spline)
        |> Vector2d.scaleBy 2


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
area than the spline itself).

    QuadraticSpline2d.boundingBox exampleSpline
    --> BoundingBox2d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 5
    -->     , minY = 1
    -->     , maxY = 4
    -->     }

-}
boundingBox : QuadraticSpline2d -> BoundingBox2d
boundingBox spline =
    let
        ( x1, y1 ) =
            Point2d.coordinates (startPoint spline)

        ( x2, y2 ) =
            Point2d.coordinates (controlPoint spline)

        ( x3, y3 ) =
            Point2d.coordinates (endPoint spline)
    in
    BoundingBox2d.fromExtrema
        { minX = min x1 (min x2 x3)
        , maxX = max x1 (max x2 x3)
        , minY = min y1 (min y2 y3)
        , maxY = max y1 (max y2 y3)
        }


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
pointOn : QuadraticSpline2d -> Float -> Maybe Point2d
pointOn spline t =
    if 0 <= t && t <= 1 then
        let
            p1 =
                startPoint spline

            p2 =
                controlPoint spline

            p3 =
                endPoint spline

            q1 =
                Point2d.interpolateFrom p1 p2 t

            q2 =
                Point2d.interpolateFrom p2 p3 t
        in
        Just <| Point2d.interpolateFrom q1 q2 t
    else
        Nothing


{-| Convenient shorthand for evaluating multiple points;

    QuadraticSpline2d.pointsOn arc parameterValues

is equivalent to

    List.map (QuadraticSpline2d.pointOn arc) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
pointsOn : QuadraticSpline2d -> List Float -> List Point2d
pointsOn arc parameterValues =
    List.filterMap (pointOn arc) parameterValues


{-| Get the derivative vector at a point along a spline, based on a parameter
that ranges from 0 to 1. A parameter value of 0 corresponds to the start
derivative of the spline and a value of 1 corresponds to the end derivative.

    QuadraticSpline2d.derivative exampleSpline 0
    --> Vector2d.fromComponents ( 4, 6 )

    QuadraticSpline2d.derivative exampleSpline 0.5
    --> Vector2d.fromComponents ( 4, 0 )

    QuadraticSpline2d.derivative exampleSpline 1
    --> Vector2d.fromComponents ( 4, -6 )

Note that the derivative interpolates linearly from end to end.

-}
derivative : QuadraticSpline2d -> Float -> Maybe Vector2d
derivative spline t =
    if 0 <= t && t <= 1 then
        let
            p1 =
                startPoint spline

            p2 =
                controlPoint spline

            p3 =
                endPoint spline

            v1 =
                Vector2d.from p1 p2

            v2 =
                Vector2d.from p2 p3
        in
        Just (Vector2d.interpolateFrom v1 v2 t |> Vector2d.scaleBy 2)
    else
        Nothing


{-| Convenient shorthand for evaluating multiple derivatives;

    QuadraticSpline2d.derivatives spline parameterValues

is equivalent to

    List.map (QuadraticSpline2d.derivative spline) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
derivatives : QuadraticSpline2d -> List Float -> List Vector2d
derivatives spline parameterValues =
    List.filterMap (derivative spline) parameterValues


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
        ( x1, y1 ) =
            Point2d.coordinates (startPoint spline)

        ( x2, y2 ) =
            Point2d.coordinates (controlPoint spline)

        ( x3, y3 ) =
            Point2d.coordinates (endPoint spline)

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
        if 0 <= t && t <= 1 then
            let
                x13 =
                    x12 + t * x123

                y13 =
                    y12 + t * y123
            in
            2 * sqrt (x13 * x13 + y13 * y13)
        else
            0


{-| Sample a spline at a given parameter value to get both the position and
derivative vector at that parameter value;

    QuadraticSpline2d.sample spline t

is equivalent to

    ( QuadraticSpline2d.pointOn spline t
    , QuadraticSpline2d.derivative spline t
    )

but is more efficient.

-}
sample : QuadraticSpline2d -> Float -> Maybe ( Point2d, Vector2d )
sample spline t =
    if 0 <= t && t <= 1 then
        let
            p1 =
                startPoint spline

            p2 =
                controlPoint spline

            p3 =
                endPoint spline

            q1 =
                Point2d.interpolateFrom p1 p2 t

            q2 =
                Point2d.interpolateFrom p2 p3 t
        in
        Just <|
            ( Point2d.interpolateFrom q1 q2 t
            , Vector2d.from q1 q2 |> Vector2d.scaleBy 2
            )
    else
        Nothing


{-| Convenient shorthand for evaluating multiple samples;

    QuadraticSpline2d.samples spline parameterValues

is equivalent to

    List.map (QuadraticSpline2d.sample spline) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
samples : QuadraticSpline2d -> List Float -> List ( Point2d, Vector2d )
samples spline parameterValues =
    List.filterMap (sample spline) parameterValues


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    QuadraticSpline2d.reverse exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 5, 1 )
    -->     , controlPoint = Point2d.fromCoordinates ( 3, 4 )
    -->     , endPoint = Point2d.fromCoordinates ( 1, 1 )
    -->     }

-}
reverse : QuadraticSpline2d -> QuadraticSpline2d
reverse spline =
    with
        { startPoint = endPoint spline
        , controlPoint = controlPoint spline
        , endPoint = startPoint spline
        }


{-| Scale a spline about the given center point by the given scale.

    examplePolyline
        |> QuadraticSpline2d.scaleAbout Point2d.origin 2
    --> QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 2, 2 )
    -->     , controlPoint = Point2d.fromCoordinates ( 6, 8 )
    -->     , endPoint = Point2d.fromCoordinates ( 10, 2 )
    -->     }

-}
scaleAbout : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given center point by a given
angle (in radians).

    examplePolyline
        |> QuadraticSpline2d.rotateAround Point2d.origin
            (degrees 90)
    --> QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( -1, 1 )
    -->     , controlPoint = Point2d.fromCoordinates ( -4, 3 )
    -->     , endPoint = Point2d.fromCoordinates ( -1, 5 )
    -->     }

-}
rotateAround : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    exampleSpline
        |> QuadraticSpline2d.translateBy displacement
    --> QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 3, 4 )
    -->     , controlPoint = Point2d.fromCoordinates ( 5, 7 )
    -->     , endPoint = Point2d.fromCoordinates ( 7, 4 )
    -->     )

-}
translateBy : Vector2d -> QuadraticSpline2d -> QuadraticSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


{-| Translate a spline in a given direction by a given distance;

    QuadraticSpline2d.translateIn direction distance

is equivalent to

    QuadraticSpline2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
translateIn direction distance spline =
    translateBy (Vector2d.withLength distance direction) spline


{-| Mirror a spline across an axis.

    QuadraticSpline2d.mirrorAcross Axis2d.x exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 1, -1 )
    -->     , controlPoint = Point2d.fromCoordinates ( 3, -4 )
    -->     , endPoint = Point2d.fromCoordinates ( 5, -1 )
    -->     }

-}
mirrorAcross : Axis2d -> QuadraticSpline2d -> QuadraticSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


mapControlPoints : (Point2d -> Point2d) -> QuadraticSpline2d -> QuadraticSpline2d
mapControlPoints function spline =
    with
        { startPoint = function (startPoint spline)
        , controlPoint = function (controlPoint spline)
        , endPoint = function (endPoint spline)
        }


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atCoordinates ( 1, 2 )

    QuadraticSpline2d.relativeTo localFrame exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 0, -1 )
    -->     , controlPoint = Point2d.fromCoordinates ( 2, 2 )
    -->     , endPoint = Point2d.fromCoordinates ( 4, -1 )
    -->     }

-}
relativeTo : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame2d.atCoordinates ( 1, 2 )

    QuadraticSpline2d.placeIn localFrame exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 2, 3 )
    -->     , controlPoint = Point2d.fromCoordinates ( 4, 6 )
    -->     , endPoint = Point2d.fromCoordinates ( 6, 3 )
    -->     }

-}
placeIn : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


{-| Split a spline into two roughly equal halves.

    QuadraticSpline2d.bisect exampleSpline
    --> ( QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 1, 1 )
    -->     , controlPoint = Point2d.fromCoordinates ( 2, 2.5 )
    -->     , endPoint = Point2d.fromCoordinates ( 3, 2.5 )
    -->     }
    --> , QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 3, 2.5 )
    -->     , controlPoint = Point2d.fromCoordinates ( 4, 2.5 )
    -->     , endPoint = Point2d.fromCoordinates ( 5, 1 )
    -->     }
    --> )

-}
bisect : QuadraticSpline2d -> ( QuadraticSpline2d, QuadraticSpline2d )
bisect =
    splitAt 0.5


{-| Split a spline at a particular parameter value (in the range 0 to 1),
resulting in two smaller splines.

    QuadraticSpline2d.splitAt 0.75 exampleSpline
    --> ( QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 1, 1 )
    -->     , controlPoint = Point2d.fromCoordinates ( 2.5, 3.25 )
    -->     , endPoint = Point2d.fromCoordinates ( 4, 2.125 )
    -->     }
    --> , QuadraticSpline2d.with
    -->     { startPoint = Point2d.fromCoordinates ( 4, 2.125 )
    -->     , controlPoint = Point2d.fromCoordinates ( 4.5, 1.75 )
    -->     , endPoint = Point2d.fromCoordinates ( 5, 1 )
    -->     }
    --> )

-}
splitAt : Float -> QuadraticSpline2d -> ( QuadraticSpline2d, QuadraticSpline2d )
splitAt t spline =
    let
        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        q1 =
            Point2d.interpolateFrom p1 p2 t

        q2 =
            Point2d.interpolateFrom p2 p3 t

        r =
            Point2d.interpolateFrom q1 q2 t
    in
    ( with { startPoint = p1, controlPoint = q1, endPoint = r }
    , with { startPoint = r, controlPoint = q2, endPoint = p3 }
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized QuadraticSpline2d ArcLengthParameterization


{-| Build an arc length parameterization of the given spline:

    parameterizedSpline =
        QuadraticSpline2d.arcLengthParameterized
            (Accuracy.maxError 1.0e-4)
            exampleSpline

The accuracy of the parameterization is controlled by the first argument; this
affects the accuracy of results returned from functions such as `arcLength` and
`pointAlong`.

-}
arcLengthParameterized : Accuracy -> QuadraticSpline2d -> ArcLengthParameterized
arcLengthParameterized (Types.MaxError tolerance) spline =
    let
        maxSecondDerivativeMagnitude =
            Vector2d.length (secondDerivative spline)

        parameterization =
            ArcLengthParameterization.build
                { tolerance = tolerance
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude = maxSecondDerivativeMagnitude
                }
    in
    ArcLengthParameterized spline parameterization


{-| Find the total arc length of a spline:

    QuadraticSpline2d.arcLength parameterizedSpline
    --> 5.1986

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized -> Float
arcLength (ArcLengthParameterized _ parameterization) =
    ArcLengthParameterization.totalArcLength parameterization


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
    parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue s
        |> Maybe.andThen (pointOn spline)


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
    parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue s
        |> Maybe.andThen (derivative spline)
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
    ArcLengthParameterization.arcLengthToParameterValue s parameterization


{-| Try to get the arc length along a spline at a given parameter value. If the
given parameter value is less than zero or greater than one, returns `Nothing`.

    QuadraticSpline2d.parameterValueToArcLength
        parameterizedSpline
        0.25
    --> Just 1.5122

-}
parameterValueToArcLength : ArcLengthParameterized -> Float -> Maybe Float
parameterValueToArcLength (ArcLengthParameterized _ parameterization) t =
    ArcLengthParameterization.parameterValueToArcLength t parameterization


{-| Get the original `QuadraticSpline2d` from which an `ArcLengthParameterized`
value was constructed.
-}
underlyingSpline : ArcLengthParameterized -> QuadraticSpline2d
underlyingSpline (ArcLengthParameterized spline _) =
    spline


{-| Get the second derivative of a spline (for a quadratic spline, this is a
constant).
-}
secondDerivative : QuadraticSpline2d -> Vector2d
secondDerivative spline =
    let
        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        v1 =
            Vector2d.from p1 p2

        v2 =
            Vector2d.from p2 p3
    in
    Vector2d.difference v2 v1 |> Vector2d.scaleBy 2
