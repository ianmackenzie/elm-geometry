--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module RationalCubicSpline3d exposing
    ( RationalCubicSpline3d
    , fromControlPoints
    , bSplineSegments, bSplineIntervals
    , startPoint, endPoint, startDerivative, endDerivative, boundingBox
    , firstControlPoint, secondControlPoint, thirdControlPoint, fourthControlPoint, firstWeight, secondWeight, thirdWeight, fourthWeight
    , pointOn, firstDerivative
    , segments
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    , bisect, splitAt
    )

{-| A `RationalCubicSpline3d` is a rational cubic Bézier curve in 3D defined by
four control points and corresponding weights. This module contains
functionality for

  - Constructing splines
  - Evaluating points and tangent directions along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs RationalCubicSpline3d


# Constructors

@docs fromControlPoints


## B-splines

@docs bSplineSegments, bSplineIntervals


# Properties

@docs startPoint, endPoint, startDerivative, endDerivative, boundingBox


## Control points and weights

@docs firstControlPoint, secondControlPoint, thirdControlPoint, fourthControlPoint, firstWeight, secondWeight, thirdWeight, fourthWeight


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
import CubicSpline3d exposing (CubicSpline3d)
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
import Quantity exposing (Quantity(..), Rate)
import Vector3d exposing (Vector3d)


{-| -}
type alias RationalCubicSpline3d units coordinates =
    Types.RationalCubicSpline3d units coordinates


{-| Construct a spline from its four control points and associated weights.

    exampleSpline =
        RationalCubicSpline3d.fromControlPoints
            ( Point3d.meters 1 0 1, 1 )
            ( Point3d.meters 3 0 4, 2 )
            ( Point3d.meters 5 0 1, 2 )
            ( Point3d.meters 7 0 4, 1 )

All weights should be greater than zero. If any negative weights are passed,
their absolute values will be used. Larger weights will tend to 'pull' the curve
towards the corresponding control point.

-}
fromControlPoints :
    ( Point3d units coordinates, Float )
    -> ( Point3d units coordinates, Float )
    -> ( Point3d units coordinates, Float )
    -> ( Point3d units coordinates, Float )
    -> RationalCubicSpline3d units coordinates
fromControlPoints ( p1, w1 ) ( p2, w2 ) ( p3, w3 ) ( p4, w4 ) =
    Types.RationalCubicSpline3d
        { firstControlPoint = p1
        , secondControlPoint = p2
        , thirdControlPoint = p3
        , fourthControlPoint = p4
        , firstWeight = abs w1
        , secondWeight = abs w2
        , thirdWeight = abs w3
        , fourthWeight = abs w4
        }


{-| Construct a [non-uniform rational B-spline](https://en.wikipedia.org/wiki/Non-uniform_rational_B-spline)
(also known as a NURBS curve) from a list of knot values and a list of control
points, and return the individual segments of that B-spline as a list.

The number of knots should be two greater than the number of control points; any
extra knots or control points will be dropped. In most cases the first and last
knots will be repeated three times; for example, the knots

    [ 0, 0, 0, 1, 2, 3, 4, 4, 4 ]

could be used along with 7 control points to form 4 spline segments.

Note that a popular alternate convention uses two extra 'dummy' knot values at
the start and end, so if you see an example where the number of knots is _four_
greater than the number of control points (especially if you also notice that
the first and last knots are repeated four times instead of three!) then you
should drop the first and last knot values.

Knot values should be given in ascending order but will be sorted if necessary.

-}
bSplineSegments : List Float -> List ( Point3d units coordinates, Float ) -> List (RationalCubicSpline3d units coordinates)
bSplineSegments givenKnots givenControlPoints =
    case ( List.sort givenKnots, givenControlPoints ) of
        ( u0 :: u1 :: u2 :: u3 :: u4 :: u5s, b012 :: b123 :: b234 :: b345s ) ->
            bSplineSegmentsHelp u0 u1 u2 u3 u4 u5s b012 b123 b234 b345s []

        _ ->
            []


bSplineSegmentsHelp :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> List Float
    -> ( Point3d units coordinates, Float )
    -> ( Point3d units coordinates, Float )
    -> ( Point3d units coordinates, Float )
    -> List ( Point3d units coordinates, Float )
    -> List (RationalCubicSpline3d units coordinates)
    -> List (RationalCubicSpline3d units coordinates)
bSplineSegmentsHelp u0 u1 u2 u3 u4 u5s pair012 pair123 pair234 pair345s accumulated =
    case ( u5s, pair345s ) of
        ( u5 :: u6s, pair345 :: pair456s ) ->
            if u2 == u3 then
                bSplineSegmentsHelp u1 u2 u3 u4 u5 u6s pair123 pair234 pair345 pair456s accumulated

            else
                let
                    ( b012, w012 ) =
                        pair012

                    ( b123, w123 ) =
                        pair123

                    ( b234, w234 ) =
                        pair234

                    ( b345, w345 ) =
                        pair345

                    u122 =
                        (u2 - u0) / (u3 - u0)

                    w122 =
                        Float.interpolateFrom w012 w123 u122

                    b122 =
                        weightedInterpolation u122 w122 b012 w012 b123 w123

                    u223 =
                        (u2 - u1) / (u4 - u1)

                    w223 =
                        Float.interpolateFrom w123 w234 u223

                    b223 =
                        weightedInterpolation u223 w223 b123 w123 b234 w234

                    u233 =
                        (u3 - u1) / (u4 - u1)

                    w233 =
                        Float.interpolateFrom w123 w234 u233

                    b233 =
                        weightedInterpolation u233 w233 b123 w123 b234 w234

                    u334 =
                        (u3 - u2) / (u5 - u2)

                    w334 =
                        Float.interpolateFrom w234 w345 u334

                    b334 =
                        weightedInterpolation u334 w334 b234 w234 b345 w345

                    u222 =
                        (u2 - u1) / (u3 - u1)

                    w222 =
                        Float.interpolateFrom w122 w223 u222

                    b222 =
                        weightedInterpolation u222 w222 b122 w122 b223 w223

                    u333 =
                        (u3 - u2) / (u4 - u2)

                    w333 =
                        Float.interpolateFrom w233 w334 u333

                    b333 =
                        weightedInterpolation u333 w333 b233 w233 b334 w334

                    segment =
                        fromControlPoints ( b222, w222 ) ( b223, w223 ) ( b233, w233 ) ( b333, w333 )
                in
                bSplineSegmentsHelp u1 u2 u3 u4 u5 u6s pair123 pair234 pair345 pair456s (segment :: accumulated)

        _ ->
            List.reverse accumulated


{-| For a given set of B-spline knots, return the corresponding intervals
between knots that correspond to individual spline [segments](#bSplineSegments).
-}
bSplineIntervals : List Float -> List (Interval Float)
bSplineIntervals givenKnots =
    CubicSpline3d.bSplineIntervals givenKnots


{-| Convert a spline from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> RationalCubicSpline3d units1 coordinates -> RationalCubicSpline3d units2 coordinates
at rate (Types.RationalCubicSpline3d spline) =
    Types.RationalCubicSpline3d
        { firstControlPoint = Point3d.at rate spline.firstControlPoint
        , secondControlPoint = Point3d.at rate spline.secondControlPoint
        , thirdControlPoint = Point3d.at rate spline.thirdControlPoint
        , fourthControlPoint = Point3d.at rate spline.fourthControlPoint
        , firstWeight = spline.firstWeight
        , secondWeight = spline.secondWeight
        , thirdWeight = spline.thirdWeight
        , fourthWeight = spline.fourthWeight
        }


{-| Convert a spline from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> RationalCubicSpline3d units1 coordinates -> RationalCubicSpline3d units2 coordinates
at_ rate spline =
    at (Quantity.inverse rate) spline


{-| Get the start point of a spline. Equal to
[`firstControlPoint`](#firstControlPoint).
-}
startPoint : RationalCubicSpline3d units coordinates -> Point3d units coordinates
startPoint (Types.RationalCubicSpline3d spline) =
    spline.firstControlPoint


{-| Get the end point of a spline. Equal to
[`fourthControlPoint`](#fourthControlPoint).
-}
endPoint : RationalCubicSpline3d units coordinates -> Point3d units coordinates
endPoint (Types.RationalCubicSpline3d spline) =
    spline.fourthControlPoint


{-| Get the start derivative of a spline;

    RationalCubicSpline3d.startDerivative spline

is equivalent to (but more efficient than)

    RationalCubicSpline3d.firstDerivative spline 0

-}
startDerivative : RationalCubicSpline3d units coordinates -> Vector3d units coordinates
startDerivative (Types.RationalCubicSpline3d spline) =
    Vector3d.from spline.firstControlPoint spline.secondControlPoint
        |> Vector3d.scaleBy (3 * spline.secondWeight / spline.firstWeight)


{-| Get the end derivative of a spline;

    RationalCubicSpline3d.endDerivative spline

is equivalent to (but more efficient than)

    RationalCubicSpline3d.firstDerivative spline 1

-}
endDerivative : RationalCubicSpline3d units coordinates -> Vector3d units coordinates
endDerivative (Types.RationalCubicSpline3d spline) =
    Vector3d.from spline.thirdControlPoint spline.fourthControlPoint
        |> Vector3d.scaleBy (3 * spline.thirdWeight / spline.fourthWeight)


{-| Get the first control point of the spline. Equal to
[`startPoint`](#startPoint).
-}
firstControlPoint : RationalCubicSpline3d units coordinates -> Point3d units coordinates
firstControlPoint (Types.RationalCubicSpline3d spline) =
    spline.firstControlPoint


{-| Get the second control point of the spline.
-}
secondControlPoint : RationalCubicSpline3d units coordinates -> Point3d units coordinates
secondControlPoint (Types.RationalCubicSpline3d spline) =
    spline.secondControlPoint


{-| Get the third control point of the spline.
-}
thirdControlPoint : RationalCubicSpline3d units coordinates -> Point3d units coordinates
thirdControlPoint (Types.RationalCubicSpline3d spline) =
    spline.thirdControlPoint


{-| Get the fourth and last control point of the spline.
Equal to [`endPoint`](#endPoint).
-}
fourthControlPoint : RationalCubicSpline3d units coordinates -> Point3d units coordinates
fourthControlPoint (Types.RationalCubicSpline3d spline) =
    spline.fourthControlPoint


{-| Get the weight corresponding to the first control point of the spline.
-}
firstWeight : RationalCubicSpline3d units coordinates -> Float
firstWeight (Types.RationalCubicSpline3d spline) =
    spline.firstWeight


{-| Get the weight corresponding to the second control point of the spline.
-}
secondWeight : RationalCubicSpline3d units coordinates -> Float
secondWeight (Types.RationalCubicSpline3d spline) =
    spline.secondWeight


{-| Get the weight corresponding to the third control point of the spline.
-}
thirdWeight : RationalCubicSpline3d units coordinates -> Float
thirdWeight (Types.RationalCubicSpline3d spline) =
    spline.thirdWeight


{-| Get the weight corresponding to the fourth control point of the spline.
-}
fourthWeight : RationalCubicSpline3d units coordinates -> Float
fourthWeight (Types.RationalCubicSpline3d spline) =
    spline.fourthWeight


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
area than the spline itself).

    RationalCubicSpline3d.boundingBox exampleSpline
    --> BoundingBox3d.from
    -->     (Point3d.meters 1 0 1)
    -->     (Point3d.meters 7 0 4)

-}
boundingBox : RationalCubicSpline3d units coordinates -> BoundingBox3d units coordinates
boundingBox spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

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

        x4 =
            Point3d.xCoordinate p4

        y4 =
            Point3d.yCoordinate p4

        z4 =
            Point3d.zCoordinate p4
    in
    BoundingBox3d.fromExtrema
        { minX = Quantity.min (Quantity.min x1 x2) (Quantity.min x3 x4)
        , maxX = Quantity.max (Quantity.max x1 x2) (Quantity.max x3 x4)
        , minY = Quantity.min (Quantity.min y1 y2) (Quantity.min y3 y4)
        , maxY = Quantity.max (Quantity.max y1 y2) (Quantity.max y3 y4)
        , minZ = Quantity.min (Quantity.min z1 z2) (Quantity.min z3 z4)
        , maxZ = Quantity.max (Quantity.max z1 z2) (Quantity.max z3 z4)
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
pointOn : RationalCubicSpline3d units coordinates -> Float -> Point3d units coordinates
pointOn spline t =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        w1 =
            firstWeight spline

        w2 =
            secondWeight spline

        w3 =
            thirdWeight spline

        w4 =
            fourthWeight spline

        w12 =
            Float.interpolateFrom w1 w2 t

        w23 =
            Float.interpolateFrom w2 w3 t

        w34 =
            Float.interpolateFrom w3 w4 t

        p12 =
            weightedInterpolation t w12 p1 w1 p2 w2

        p23 =
            weightedInterpolation t w23 p2 w2 p3 w3

        p34 =
            weightedInterpolation t w34 p3 w3 p4 w4

        w123 =
            Float.interpolateFrom w12 w23 t

        w234 =
            Float.interpolateFrom w23 w34 t

        p123 =
            weightedInterpolation t w123 p12 w12 p23 w23

        p234 =
            weightedInterpolation t w234 p23 w23 p34 w34

        w1234 =
            Float.interpolateFrom w123 w234 t
    in
    weightedInterpolation t w1234 p123 w123 p234 w234


{-| Approximate a spline by a given number of line segments. Note that the
number of points in the polyline will be one more than the number of segments.
-}
segments : Int -> RationalCubicSpline3d units coordinates -> Polyline3d units coordinates
segments numSegments spline =
    Polyline3d.fromVertices (Parameter1d.steps numSegments (pointOn spline))



-- {-| Approximate a spline as a polyline, within a given tolerance. Every point on
-- the returned polyline will be within the given tolerance of the spline.
-- -}
-- approximate : Quantity Float units -> RationalCubicSpline3d units coordinates -> Polyline3d units coordinates
-- approximate maxError spline =
--     segments (numApproximationSegments maxError spline) spline


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.
-}
reverse : RationalCubicSpline3d units coordinates -> RationalCubicSpline3d units coordinates
reverse spline =
    fromControlPoints
        ( fourthControlPoint spline, fourthWeight spline )
        ( thirdControlPoint spline, thirdWeight spline )
        ( secondControlPoint spline, secondWeight spline )
        ( firstControlPoint spline, firstWeight spline )


{-| Scale a spline about the given center point by the given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> RationalCubicSpline3d units coordinates -> RationalCubicSpline3d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point3d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given center point by a given
angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> RationalCubicSpline3d units coordinates -> RationalCubicSpline3d units coordinates
rotateAround axis angle spline =
    mapControlPoints (Point3d.rotateAround axis angle) spline


{-| Translate a spline by a given displacement.
-}
translateBy : Vector3d units coordinates -> RationalCubicSpline3d units coordinates -> RationalCubicSpline3d units coordinates
translateBy displacement spline =
    mapControlPoints (Point3d.translateBy displacement) spline


{-| Translate a spline in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> RationalCubicSpline3d units coordinates -> RationalCubicSpline3d units coordinates
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across an axis.
-}
mirrorAcross : Types.Plane3d units coordinates -> RationalCubicSpline3d units coordinates -> RationalCubicSpline3d units coordinates
mirrorAcross plane spline =
    mapControlPoints (Point3d.mirrorAcross plane) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> RationalCubicSpline3d units globalCoordinates -> RationalCubicSpline3d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point3d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> RationalCubicSpline3d units localCoordinates -> RationalCubicSpline3d units globalCoordinates
placeIn frame spline =
    mapControlPoints (Point3d.placeIn frame) spline


mapControlPoints : (Point3d units1 coordinates1 -> Point3d units2 coordinates2) -> RationalCubicSpline3d units1 coordinates1 -> RationalCubicSpline3d units2 coordinates2
mapControlPoints function spline =
    fromControlPoints
        ( function (firstControlPoint spline), firstWeight spline )
        ( function (secondControlPoint spline), secondWeight spline )
        ( function (thirdControlPoint spline), thirdWeight spline )
        ( function (fourthControlPoint spline), fourthWeight spline )


{-| Split a spline into two roughly equal halves. Equivalent to
`RationalCubicSpline3d.splitAt 0.5`.
-}
bisect : RationalCubicSpline3d units coordinates -> ( RationalCubicSpline3d units coordinates, RationalCubicSpline3d units coordinates )
bisect spline =
    splitAt 0.5 spline


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.
-}
splitAt : Float -> RationalCubicSpline3d units coordinates -> ( RationalCubicSpline3d units coordinates, RationalCubicSpline3d units coordinates )
splitAt t spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        w1 =
            firstWeight spline

        w2 =
            secondWeight spline

        w3 =
            thirdWeight spline

        w4 =
            fourthWeight spline

        w12 =
            Float.interpolateFrom w1 w2 t

        w23 =
            Float.interpolateFrom w2 w3 t

        w34 =
            Float.interpolateFrom w3 w4 t

        p12 =
            weightedInterpolation t w12 p1 w1 p2 w2

        p23 =
            weightedInterpolation t w23 p2 w2 p3 w3

        p34 =
            weightedInterpolation t w34 p3 w3 p4 w4

        w123 =
            Float.interpolateFrom w12 w23 t

        w234 =
            Float.interpolateFrom w23 w34 t

        p123 =
            weightedInterpolation t w123 p12 w12 p23 w23

        p234 =
            weightedInterpolation t w234 p23 w23 p34 w34

        w1234 =
            Float.interpolateFrom w123 w234 t

        p1234 =
            weightedInterpolation t w1234 p123 w123 p234 w234
    in
    ( fromControlPoints ( p1, w1 ) ( p12, w12 ) ( p123, w123 ) ( p1234, w1234 )
    , fromControlPoints ( p1234, w1234 ) ( p234, w234 ) ( p34, w34 ) ( p4, w4 )
    )


{-| Get the first derivative of a spline at a given parameter value.
-}
firstDerivative : RationalCubicSpline3d units coordinates -> Float -> Vector3d units coordinates
firstDerivative spline t =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        w1 =
            firstWeight spline

        w2 =
            secondWeight spline

        w3 =
            thirdWeight spline

        w4 =
            fourthWeight spline

        w12 =
            Float.interpolateFrom w1 w2 t

        w23 =
            Float.interpolateFrom w2 w3 t

        w34 =
            Float.interpolateFrom w3 w4 t

        p12 =
            weightedInterpolation t w12 p1 w1 p2 w2

        p23 =
            weightedInterpolation t w23 p2 w2 p3 w3

        p34 =
            weightedInterpolation t w34 p3 w3 p4 w4

        w123 =
            Float.interpolateFrom w12 w23 t

        w234 =
            Float.interpolateFrom w23 w34 t

        p123 =
            weightedInterpolation t w123 p12 w12 p23 w23

        p234 =
            weightedInterpolation t w234 p23 w23 p34 w34

        w1234 =
            Float.interpolateFrom w123 w234 t
    in
    Vector3d.from p123 p234
        |> Vector3d.scaleBy (3 * w123 * w234 / (w1234 * w1234))



-- scaledPoint : Point2d units coordinates -> Float -> Point3d units coordinates
-- scaledPoint (Types.Point2d p) w =
--     Types.Point3d { x = p.x * w, y = p.y * w, z = w }
-- {-| Determine the number of linear segments needed to approximate a cubic
-- spline to within a given tolerance.
-- -}
-- numApproximationSegments : Quantity Float units -> RationalCubicSpline2d units coordinats -> Int
-- numApproximationSegments maxError spline =
--     let
--         p1 =
--             firstControlPoint spline
--         p2 =
--             secondControlPoint spline
--         p3 =
--             thirdControlPoint spline
--         p4 =
--             fourthControlPoint spline
--         w1 =
--             firstWeight spline
--         w2 =
--             secondWeight spline
--         w3 =
--             thirdWeight spline
--         w4 =
--             fourthWeight spline
--         wMin =
--             min (min w1 w2) (min w3 w4)
--         s1 =
--             w1 / wMin
--         s2 =
--             w2 / wMin
--         s3 =
--             w3 / wMin
--         s4 =
--             w4 / wMin
--         q1 =
--             scaledPoint p1 s1
--         q2 =
--             scaledPoint p2 s2
--         q3 =
--             scaledPoint p3 s3
--         q4 =
--             scaledPoint p4 s4
--         spline3d =
--             CubicSpline3d.fromControlPoints q1 q2 q3 q4
--     in
--     Curve.numApproximationSegments
--         { maxError = maxError
--         , maxSecondDerivativeMagnitude = CubicSpline3d.maxSecondDerivativeMagnitude spline3d
--         }
