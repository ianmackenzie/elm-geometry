--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module RationalQuadraticSpline3d exposing
    ( RationalQuadraticSpline3d
    , fromControlPoints
    , bSplineSegments, bSplineIntervals
    , startPoint, endPoint, startDerivative, endDerivative, boundingBox
    , firstControlPoint, secondControlPoint, thirdControlPoint, firstWeight, secondWeight, thirdWeight
    , pointOn, firstDerivative
    , segments
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    , bisect, splitAt
    )

{-| A `RationalQuadraticSpline3d` is a rational quadratic Bézier curve in 3D
defined by three control points and corresponding weights. This module contains
functionality for

  - Constructing splines
  - Evaluating points and tangent directions along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs RationalQuadraticSpline3d


# Constructors

@docs fromControlPoints


## B-splines

@docs bSplineSegments, bSplineIntervals


# Properties

@docs startPoint, endPoint, startDerivative, endDerivative, boundingBox


## Control points and weights

@docs firstControlPoint, secondControlPoint, thirdControlPoint, firstWeight, secondWeight, thirdWeight


# Evaluation

@docs pointOn, firstDerivative


# Linear approximation

@docs segments


# Transformations

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


# Subdivision

@docs bisect, splitAt

-}

import Angle exposing (Angle)
import ArcLengthParameterization exposing (ArcLengthParameterization)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Curve
import Direction3d exposing (Direction3d)
import Float.Extra as Float
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Interval exposing (Interval)
import LineSegment3d exposing (LineSegment3d)
import Parameter1d
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity(..), Rate)
import Vector3d exposing (Vector3d)


{-| -}
type alias RationalQuadraticSpline3d units coordinates =
    Types.RationalQuadraticSpline3d units coordinates


{-| Construct a spline from its four control points and associated weights.

    exampleSpline =
        RationalQuadraticSpline3d.fromControlPoints
            ( Point3d.meters 1 0 1, 1 )
            ( Point3d.meters 3 0 4, 2 )
            ( Point3d.meters 5 0 1, 1 )

All weights should be greater than zero. If any negative weights are passed,
their absolute values will be used. Larger weights will tend to 'pull' the curve
towards the corresponding control point.

-}
fromControlPoints :
    ( Point3d units coordinates, Float )
    -> ( Point3d units coordinates, Float )
    -> ( Point3d units coordinates, Float )
    -> RationalQuadraticSpline3d units coordinates
fromControlPoints ( p1, w1 ) ( p2, w2 ) ( p3, w3 ) =
    Types.RationalQuadraticSpline3d
        { firstControlPoint = p1
        , secondControlPoint = p2
        , thirdControlPoint = p3
        , firstWeight = abs w1
        , secondWeight = abs w2
        , thirdWeight = abs w3
        }


{-| Construct a [non-uniform rational B-spline](https://en.wikipedia.org/wiki/Non-uniform_rational_B-spline)
(also known as a NURBS curve) from a list of knot values and a list of control
points, and return the individual segments of that B-spline as a list.

The number of knots should be one greater than the number of control points; any
extra knots or control points will be dropped. In most cases the first and last
knots will be repeated two times; for example, the knots

    [ 0, 0, 1, 2, 3, 4, 4 ]

could be used along with 6 control points to form 4 spline segments.

Note that a popular alternate convention uses two extra 'dummy' knot values at
the start and end, so if you see an example where the number of knots is _three_
greater than the number of control points (especially if you also notice that
the first and last knots are repeated three times instead of two!) then you
should drop the first and last knot values.

Knot values should be given in ascending order but will be sorted if necessary.

-}
bSplineSegments : List Float -> List ( Point3d units coordinates, Float ) -> List (RationalQuadraticSpline3d units coordinates)
bSplineSegments givenKnots givenControlPoints =
    case ( List.sort givenKnots, givenControlPoints ) of
        ( u0 :: u1 :: u2 :: u3s, b01 :: b12 :: b23s ) ->
            bSplineSegmentsHelp u0 u1 u2 u3s b01 b12 b23s []

        _ ->
            []


bSplineSegmentsHelp :
    Float
    -> Float
    -> Float
    -> List Float
    -> ( Point3d units coordinates, Float )
    -> ( Point3d units coordinates, Float )
    -> List ( Point3d units coordinates, Float )
    -> List (RationalQuadraticSpline3d units coordinates)
    -> List (RationalQuadraticSpline3d units coordinates)
bSplineSegmentsHelp u0 u1 u2 u3s pair01 pair12 pair23s accumulated =
    case ( u3s, pair23s ) of
        ( u3 :: u4s, pair23 :: pair34s ) ->
            if u1 == u2 then
                bSplineSegmentsHelp u1 u2 u3 u4s pair12 pair23 pair34s accumulated

            else
                let
                    ( b01, w01 ) =
                        pair01

                    ( b12, w12 ) =
                        pair12

                    ( b23, w23 ) =
                        pair23

                    u11 =
                        (u1 - u0) / (u2 - u0)

                    w11 =
                        Float.interpolateFrom w01 w12 u11

                    b11 =
                        weightedInterpolation u11 w11 b01 w01 b12 w12

                    u22 =
                        (u2 - u1) / (u3 - u1)

                    w22 =
                        Float.interpolateFrom w12 w23 u22

                    b22 =
                        weightedInterpolation u22 w22 b12 w12 b23 w23

                    segment =
                        fromControlPoints ( b11, w11 ) ( b12, w12 ) ( b22, w22 )
                in
                bSplineSegmentsHelp u1 u2 u3 u4s pair12 pair23 pair34s (segment :: accumulated)

        _ ->
            List.reverse accumulated


{-| For a given set of B-spline knots, return the corresponding intervals
between knots that correspond to individual spline [segments](#bSplineSegments).
-}
bSplineIntervals : List Float -> List (Interval Float)
bSplineIntervals givenKnots =
    QuadraticSpline3d.bSplineIntervals givenKnots


{-| Convert a spline from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> RationalQuadraticSpline3d units1 coordinates -> RationalQuadraticSpline3d units2 coordinates
at rate (Types.RationalQuadraticSpline3d spline) =
    Types.RationalQuadraticSpline3d
        { firstControlPoint = Point3d.at rate spline.firstControlPoint
        , secondControlPoint = Point3d.at rate spline.secondControlPoint
        , thirdControlPoint = Point3d.at rate spline.thirdControlPoint
        , firstWeight = spline.firstWeight
        , secondWeight = spline.secondWeight
        , thirdWeight = spline.thirdWeight
        }


{-| Convert a spline from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> RationalQuadraticSpline3d units1 coordinates -> RationalQuadraticSpline3d units2 coordinates
at_ rate spline =
    at (Quantity.inverse rate) spline


{-| Get the start point of a spline. Equal to
[`firstControlPoint`](#firstControlPoint).
-}
startPoint : RationalQuadraticSpline3d units coordinates -> Point3d units coordinates
startPoint (Types.RationalQuadraticSpline3d spline) =
    spline.firstControlPoint


{-| Get the end point of a spline. Equal to
[`thirdControlPoint`](#thirdControlPoint).
-}
endPoint : RationalQuadraticSpline3d units coordinates -> Point3d units coordinates
endPoint (Types.RationalQuadraticSpline3d spline) =
    spline.thirdControlPoint


{-| Get the start derivative of a spline;

    RationalQuadraticSpline3d.startDerivative spline

is equivalent to (but more efficient than)

    RationalQuadraticSpline3d.firstDerivative spline 0

-}
startDerivative : RationalQuadraticSpline3d units coordinates -> Vector3d units coordinates
startDerivative (Types.RationalQuadraticSpline3d spline) =
    Vector3d.from spline.firstControlPoint spline.secondControlPoint
        |> Vector3d.scaleBy (2 * spline.secondWeight / spline.firstWeight)


{-| Get the end derivative of a spline;

    RationalQuadraticSpline3d.endDerivative spline

is equivalent to (but more efficient than)

    RationalQuadraticSpline3d.firstDerivative spline 1

-}
endDerivative : RationalQuadraticSpline3d units coordinates -> Vector3d units coordinates
endDerivative (Types.RationalQuadraticSpline3d spline) =
    Vector3d.from spline.secondControlPoint spline.thirdControlPoint
        |> Vector3d.scaleBy (2 * spline.secondWeight / spline.thirdWeight)


{-| Get the first control point of the spline. Equal to
[`startPoint`](#startPoint).
-}
firstControlPoint : RationalQuadraticSpline3d units coordinates -> Point3d units coordinates
firstControlPoint (Types.RationalQuadraticSpline3d spline) =
    spline.firstControlPoint


{-| Get the second control point of the spline.
-}
secondControlPoint : RationalQuadraticSpline3d units coordinates -> Point3d units coordinates
secondControlPoint (Types.RationalQuadraticSpline3d spline) =
    spline.secondControlPoint


{-| Get the third and last control point of the spline. Equal to [`endPoint`](#endPoint).
-}
thirdControlPoint : RationalQuadraticSpline3d units coordinates -> Point3d units coordinates
thirdControlPoint (Types.RationalQuadraticSpline3d spline) =
    spline.thirdControlPoint


{-| Get the weight corresponding to the first control point of the spline.
-}
firstWeight : RationalQuadraticSpline3d units coordinates -> Float
firstWeight (Types.RationalQuadraticSpline3d spline) =
    spline.firstWeight


{-| Get the weight corresponding to the second control point of the spline.
-}
secondWeight : RationalQuadraticSpline3d units coordinates -> Float
secondWeight (Types.RationalQuadraticSpline3d spline) =
    spline.secondWeight


{-| Get the weight corresponding to the third control point of the spline.
-}
thirdWeight : RationalQuadraticSpline3d units coordinates -> Float
thirdWeight (Types.RationalQuadraticSpline3d spline) =
    spline.thirdWeight


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
area than the spline itself).

    RationalQuadraticSpline3d.boundingBox exampleSpline
    --> BoundingBox3d.from
    -->     (Point3d.meters 1 0 1)
    -->     (Point3d.meters 5 0 4)

-}
boundingBox : RationalQuadraticSpline3d units coordinates -> BoundingBox3d units coordinates
boundingBox spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        x1 =
            Point3d.xCoordinate p1

        y1 =
            Point3d.yCoordinate p1

        z1 =
            Point3d.zCoordinate p1

        x2 =
            Point3d.xCoordinate p2

        y2 =
            Point3d.yCoordinate p2

        z2 =
            Point3d.zCoordinate p2

        x3 =
            Point3d.xCoordinate p3

        y3 =
            Point3d.yCoordinate p3

        z3 =
            Point3d.zCoordinate p3
    in
    BoundingBox3d.fromExtrema
        { minX = Quantity.min (Quantity.min x1 x2) x3
        , maxX = Quantity.max (Quantity.max x1 x2) x3
        , minY = Quantity.min (Quantity.min y1 y2) y3
        , maxY = Quantity.max (Quantity.max y1 y2) y3
        , minZ = Quantity.min (Quantity.min z1 z2) z3
        , maxZ = Quantity.max (Quantity.max z1 z2) z3
        }


weightedInterpolation :
    Float
    -> Float
    -> Point3d units coordinates
    -> Float
    -> Point3d units coordinates
    -> Float
    -> Point3d units coordinates
weightedInterpolation t w (Types.Point3d p1) w1 (Types.Point3d p2) w2 =
    let
        wx1 =
            w1 * p1.x

        wy1 =
            w1 * p1.y

        wz1 =
            w1 * p1.z

        wx2 =
            w2 * p2.x

        wy2 =
            w2 * p2.y

        wz2 =
            w2 * p2.z
    in
    if t <= 0.5 then
        Types.Point3d
            { x = (wx1 + t * (wx2 - wx1)) / w
            , y = (wy1 + t * (wy2 - wy1)) / w
            , z = (wz1 + t * (wz2 - wz1)) / w
            }

    else
        Types.Point3d
            { x = (wx2 + (1 - t) * (wx1 - wx2)) / w
            , y = (wy2 + (1 - t) * (wy1 - wy2)) / w
            , z = (wz2 + (1 - t) * (wz1 - wz2)) / w
            }


{-| Get the point along a spline at a given parameter value.
-}
pointOn : RationalQuadraticSpline3d units coordinates -> Float -> Point3d units coordinates
pointOn spline t =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        w1 =
            firstWeight spline

        w2 =
            secondWeight spline

        w3 =
            thirdWeight spline

        w12 =
            Float.interpolateFrom w1 w2 t

        w23 =
            Float.interpolateFrom w2 w3 t

        p12 =
            weightedInterpolation t w12 p1 w1 p2 w2

        p23 =
            weightedInterpolation t w23 p2 w2 p3 w3

        w123 =
            Float.interpolateFrom w12 w23 t
    in
    weightedInterpolation t w123 p12 w12 p23 w23


{-| Approximate a spline by a given number of line segments. Note that the
number of points in the polyline will be one more than the number of segments.
-}
segments : Int -> RationalQuadraticSpline3d units coordinates -> Polyline3d units coordinates
segments numSegments spline =
    Polyline3d.fromVertices (Parameter1d.steps numSegments (pointOn spline))



-- {-| Approximate a spline as a polyline, within a given tolerance. Every point on
-- the returned polyline will be within the given tolerance of the spline.
-- -}
-- approximate : Quantity Float units -> RationalQuadraticSpline3d units coordinates -> Polyline3d units coordinates
-- approximate maxError spline =
--     segments (numApproximationSegments maxError spline) spline


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.
-}
reverse : RationalQuadraticSpline3d units coordinates -> RationalQuadraticSpline3d units coordinates
reverse spline =
    fromControlPoints
        ( thirdControlPoint spline, thirdWeight spline )
        ( secondControlPoint spline, secondWeight spline )
        ( firstControlPoint spline, firstWeight spline )


{-| Scale a spline about the given center point by the given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> RationalQuadraticSpline3d units coordinates -> RationalQuadraticSpline3d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point3d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given center point by a given
angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> RationalQuadraticSpline3d units coordinates -> RationalQuadraticSpline3d units coordinates
rotateAround axis angle spline =
    mapControlPoints (Point3d.rotateAround axis angle) spline


{-| Translate a spline by a given displacement.
-}
translateBy : Vector3d units coordinates -> RationalQuadraticSpline3d units coordinates -> RationalQuadraticSpline3d units coordinates
translateBy displacement spline =
    mapControlPoints (Point3d.translateBy displacement) spline


{-| Translate a spline in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> RationalQuadraticSpline3d units coordinates -> RationalQuadraticSpline3d units coordinates
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across an axis.
-}
mirrorAcross : Types.Plane3d units coordinates -> RationalQuadraticSpline3d units coordinates -> RationalQuadraticSpline3d units coordinates
mirrorAcross plane spline =
    mapControlPoints (Point3d.mirrorAcross plane) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> RationalQuadraticSpline3d units globalCoordinates -> RationalQuadraticSpline3d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point3d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> RationalQuadraticSpline3d units localCoordinates -> RationalQuadraticSpline3d units globalCoordinates
placeIn frame spline =
    mapControlPoints (Point3d.placeIn frame) spline


mapControlPoints : (Point3d units1 coordinates1 -> Point3d units2 coordinates2) -> RationalQuadraticSpline3d units1 coordinates1 -> RationalQuadraticSpline3d units2 coordinates2
mapControlPoints function spline =
    fromControlPoints
        ( function (firstControlPoint spline), firstWeight spline )
        ( function (secondControlPoint spline), secondWeight spline )
        ( function (thirdControlPoint spline), thirdWeight spline )


{-| Split a spline into two roughly equal halves. Equivalent to
`RationalQuadraticSpline3d.splitAt 0.5`.
-}
bisect : RationalQuadraticSpline3d units coordinates -> ( RationalQuadraticSpline3d units coordinates, RationalQuadraticSpline3d units coordinates )
bisect spline =
    splitAt 0.5 spline


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.
-}
splitAt : Float -> RationalQuadraticSpline3d units coordinates -> ( RationalQuadraticSpline3d units coordinates, RationalQuadraticSpline3d units coordinates )
splitAt t spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        w1 =
            firstWeight spline

        w2 =
            secondWeight spline

        w3 =
            thirdWeight spline

        w12 =
            Float.interpolateFrom w1 w2 t

        w23 =
            Float.interpolateFrom w2 w3 t

        p12 =
            weightedInterpolation t w12 p1 w1 p2 w2

        p23 =
            weightedInterpolation t w23 p2 w2 p3 w3

        w123 =
            Float.interpolateFrom w12 w23 t

        p123 =
            weightedInterpolation t w123 p12 w12 p23 w23
    in
    ( fromControlPoints ( p1, w1 ) ( p12, w12 ) ( p123, w123 )
    , fromControlPoints ( p123, w123 ) ( p23, w23 ) ( p3, w3 )
    )


{-| Get the first derivative of a spline at a given parameter value.
-}
firstDerivative : RationalQuadraticSpline3d units coordinates -> Float -> Vector3d units coordinates
firstDerivative spline t =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        w1 =
            firstWeight spline

        w2 =
            secondWeight spline

        w3 =
            thirdWeight spline

        w12 =
            Float.interpolateFrom w1 w2 t

        w23 =
            Float.interpolateFrom w2 w3 t

        p12 =
            weightedInterpolation t w12 p1 w1 p2 w2

        p23 =
            weightedInterpolation t w23 p2 w2 p3 w3

        w123 =
            Float.interpolateFrom w12 w23 t
    in
    Vector3d.from p12 p23
        |> Vector3d.scaleBy (2 * w12 * w23 / (w123 * w123))



-- scaledPoint : Point2d units coordinates -> Float -> Point3d units coordinates
-- scaledPoint (Types.Point2d p) w =
--     Types.Point3d { x = p.x * w, y = p.y * w, z = w }
-- {-| Determine the number of linear segments needed to approximate a cubic
-- spline to within a given tolerance.
-- -}
-- numApproximationSegments : Quantity Float units -> RationalQuadraticSpline2d units coordinats -> Int
-- numApproximationSegments maxError spline =
--     let
--         p1 =
--             firstControlPoint spline
--         p2 =
--             secondControlPoint spline
--         p3 =
--             thirdControlPoint spline
--         w1 =
--             firstWeight spline
--         w2 =
--             secondWeight spline
--         w3 =
--             thirdWeight spline
--         wMin =
--             min (min w1 w2) w3
--         s1 =
--             w1 / wMin
--         s2 =
--             w2 / wMin
--         s3 =
--             w3 / wMin
--         q1 =
--             scaledPoint p1 s1
--         q2 =
--             scaledPoint p2 s2
--         q3 =
--             scaledPoint p3 s3
--         spline3d =
--             QuadraticSpline3d.fromControlPoints q1 q2 q3
--     in
--     Curve.numApproximationSegments
--         { maxError = maxError
--         , maxSecondDerivativeMagnitude =
--             Vector3d.length (QuadraticSpline3d.secondDerivative spline3d)
--         }
