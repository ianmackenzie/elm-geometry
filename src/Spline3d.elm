--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Spline3d exposing
    ( Spline3d
    , fromControlPoints, fromQuadraticSpline, fromCubicSpline, on
    , bSplineSegments, bSplineIntervals
    , controlPoints, degree, startPoint, endPoint, startDerivative, endDerivative, boundingBox
    , pointOn
    , segments, approximate
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto
    , at, at_
    , relativeTo, placeIn, projectInto
    , bisect, splitAt
    , firstDerivative, secondDerivative, firstDerivativeBoundingBox, secondDerivativeBoundingBox, maxSecondDerivativeMagnitude, numApproximationSegments
    )

{-| A `Spline3d` is a [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 3D defined by a list of control points. This module contains functionality
for

  - Constructing splines
  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

In general you will want to use a [`QuadraticSpline3d`](#QuadraticSpline3d) or
[`CubicSpline3d`](#CubicSpline3d) instead, but a `Spline3d` can be useful if you
need to support arbitrary spline degrees (for example quartic or quintic
splines) or if the spline degree is not known at compile time.

@docs Spline3d


# Constructors

@docs fromControlPoints, fromQuadraticSpline, fromCubicSpline, on


## B-splines

@docs bSplineSegments, bSplineIntervals


# Properties

@docs controlPoints, degree, startPoint, endPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn


# Linear approximation

@docs segments, approximate


# Transformations

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn, projectInto


# Subdivision

@docs bisect, splitAt


# Advanced

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, secondDerivative, firstDerivativeBoundingBox, secondDerivativeBoundingBox, maxSecondDerivativeMagnitude, numApproximationSegments

-}

import Angle exposing (Angle)
import ArcLengthParameterization exposing (ArcLengthParameterization)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import CubicSpline3d exposing (CubicSpline3d)
import Curve
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types exposing (QuadraticSpline3d, VectorBoundingBox3d)
import Interval exposing (Interval)
import Parameter1d
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity(..), Rate)
import Quantity.Interval
import SketchPlane3d exposing (SketchPlane3d)
import Spline2d exposing (Spline2d)
import Vector3d exposing (Vector3d)
import VectorBoundingBox3d exposing (VectorBoundingBox3d)


{-| -}
type alias Spline3d units coordinates =
    Types.Spline3d units coordinates


{-| Construct a spline from its control points. In order to guarantee that a
spline has at least one control point, you must pass the first and remaining
control points separately.
-}
fromControlPoints : Point3d units coordinates -> List (Point3d units coordinates) -> Spline3d units coordinates
fromControlPoints firstControlPoint remainingControlPoints =
    Types.Spline3d firstControlPoint remainingControlPoints


{-| Convert a `QuadraticSpline3d` to a generic `Spline3d`.
-}
fromQuadraticSpline : QuadraticSpline3d units coordinates -> Spline3d units coordinates
fromQuadraticSpline quadraticSpline =
    fromControlPoints
        (QuadraticSpline3d.firstControlPoint quadraticSpline)
        [ QuadraticSpline3d.secondControlPoint quadraticSpline
        , QuadraticSpline3d.thirdControlPoint quadraticSpline
        ]


{-| Convert a `CubicSpline3d` to a generic `Spline3d`.
-}
fromCubicSpline : CubicSpline3d units coordinates -> Spline3d units coordinates
fromCubicSpline cubicSpline =
    fromControlPoints
        (CubicSpline3d.firstControlPoint cubicSpline)
        [ CubicSpline3d.secondControlPoint cubicSpline
        , CubicSpline3d.thirdControlPoint cubicSpline
        , CubicSpline3d.fourthControlPoint cubicSpline
        ]


{-| Project a spline into a given sketch plane.
-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Spline2d units coordinates2d -> Spline3d units coordinates3d
on sketchPlane spline2d =
    let
        (Types.Spline2d first rest) =
            spline2d

        pointOnPlane =
            Point3d.on sketchPlane
    in
    Types.Spline3d (pointOnPlane first) (List.map pointOnPlane rest)


{-| Get the control points of a spline as a list.
-}
controlPoints : Spline3d units coordinates -> List (Point3d units coordinates)
controlPoints spline =
    let
        (Types.Spline3d first rest) =
            spline
    in
    first :: rest


{-| Get the polynomial degree of a spline; this is equal to the number of
control points minus one. A single point has degree 0, a line segment has
degree 1, a quadratic spline has degree 2, a cubic spline has degree 3, etc.
-}
degree : Spline3d units coordinates -> Int
degree spline =
    let
        (Types.Spline3d _ rest) =
            spline
    in
    List.length rest


{-| Construct a [B-spline](https://mathworld.wolfram.com/B-Spline.html) from a
given polynomial degree (2 for a quadratic spline, 3 for a cubic spline etc.),
a list of knot values and a list of control points, and return the individual
segments of that B-spline as a list.

The number of knots should be equal to the number of control points plus the
given degree, minus 1; for example, for a cubic spline (degree 3) the number of
knots should be 2 greater than the number of control points. A popular alternate
convention uses two extra 'dummy' knot values at the start and end, so the
number of knots is equal to the given degree _plus_ one. The `bSplineSegments`
function supports both conventions - if the number of knots is equal to the
degree plus one then the first and last knots will be dropped.

In most cases the first and last knots will be repeated a number of times equal
to the spline degree (or the degree plus one, for the alternate convention
described above); for example for a quadratic spline the first and last knots
will be repeated twice and for a cubic spline they will be repeated three times.

Knot values should be given in ascending order but will be sorted if necessary.
If the number of knots does not follow either of the conventions described
above, an empty list will be returned.

-}
bSplineSegments : Int -> List Float -> List (Point3d units coordinates) -> List (Spline3d units coordinates)
bSplineSegments givenDegree givenKnots givenControlPoints =
    let
        sortedKnots =
            List.sort givenKnots

        numKnots =
            List.length sortedKnots

        numControlPoints =
            List.length givenControlPoints

        numSegments =
            numControlPoints - givenDegree
    in
    if numKnots == numControlPoints + givenDegree - 1 then
        bSplineSegmentsHelp numSegments givenDegree sortedKnots givenControlPoints

    else if numKnots == numControlPoints + givenDegree + 1 then
        let
            trimmedKnots =
                sortedKnots
                    |> List.drop 1
                    |> List.take (numControlPoints + givenDegree - 1)
        in
        bSplineSegmentsHelp numSegments givenDegree trimmedKnots givenControlPoints

    else
        []


bSplineSegmentsHelp : Int -> Int -> List Float -> List (Point3d units coordinates) -> List (Spline3d units coordinates)
bSplineSegmentsHelp numSegments givenDegree givenKnots givenControlPoints =
    if numSegments == 0 then
        []

    else
        let
            segmentKnots =
                List.take (2 * givenDegree) givenKnots

            segmentControlPoints =
                List.take (givenDegree + 1) givenControlPoints

            segment =
                bSplineSegment givenDegree segmentKnots segmentControlPoints
        in
        segment ++ bSplineSegmentsHelp (numSegments - 1) givenDegree (List.drop 1 givenKnots) (List.drop 1 givenControlPoints)


bSplineSegment : Int -> List Float -> List (Point3d units coordinates) -> List (Spline3d units coordinates)
bSplineSegment givenDegree segmentKnots segmentControlPoints =
    let
        knotArray =
            Array.fromList segmentKnots
    in
    if Array.get (givenDegree - 1) knotArray == Array.get givenDegree knotArray then
        -- Knots corresponding to this segment are equal, no segment exists
        []

    else if givenDegree == 1 then
        -- Special case for piecewise linear B-splines since bSplineSegmentHelp
        -- assumes at least one level of generated control points
        case segmentControlPoints of
            [ start, end ] ->
                [ Types.Spline3d start [ end ] ]

            _ ->
                []

    else
        let
            initialDict =
                segmentControlPoints
                    |> List.indexedMap (initialControlPoint givenDegree)
                    |> Dict.fromList
        in
        case bSplineSegmentHelp givenDegree givenDegree knotArray initialDict of
            first :: rest ->
                [ Types.Spline3d first rest ]

            [] ->
                []


initialControlPoint : Int -> Int -> Point3d units coordinates -> ( List Int, Point3d units coordinates )
initialControlPoint givenDegree index point =
    ( List.range index (index + givenDegree - 1)
    , point
    )


bSplineSegmentHelp :
    Int
    -> Int
    -> Array Float
    -> Dict (List Int) (Point3d units coordinates)
    -> List (Point3d units coordinates)
bSplineSegmentHelp givenDegree stride knotArray dict =
    let
        updated =
            Dict.foldl (processControlPoint givenDegree stride knotArray dict)
                Dict.empty
                dict
    in
    if stride <= 2 then
        Dict.union dict updated
            |> Dict.values
            |> List.drop 1
            |> List.take (givenDegree + 1)

    else
        bSplineSegmentHelp givenDegree (stride - 1) knotArray updated


processControlPoint :
    Int
    -> Int
    -> Array Float
    -> Dict (List Int) (Point3d units coordinates)
    -> List Int
    -> Point3d units coordinates
    -> Dict (List Int) (Point3d units coordinates)
    -> Dict (List Int) (Point3d units coordinates)
processControlPoint givenDegree stride knotArray current key point accumulated =
    case key of
        first :: rest ->
            let
                last =
                    first + stride

                neighborKey =
                    rest ++ [ last ]
            in
            case Dict.get neighborKey current of
                Just neighbor ->
                    accumulated
                        |> interpolateControlPoints knotArray rest point neighbor first last (givenDegree - 1)
                        |> interpolateControlPoints knotArray rest point neighbor first last givenDegree

                Nothing ->
                    accumulated

        [] ->
            -- Shouldn't happen
            accumulated


{-| Insert a new value into a sorted list.
-}
insert : Int -> List Int -> List Int
insert value sortedValues =
    case sortedValues of
        first :: rest ->
            if value <= first then
                value :: sortedValues

            else
                first :: insert value rest

        [] ->
            [ value ]


interpolateControlPoints :
    Array Float
    -> List Int
    -> Point3d units coordinates
    -> Point3d units coordinates
    -> Int
    -> Int
    -> Int
    -> Dict (List Int) (Point3d units coordinates)
    -> Dict (List Int) (Point3d units coordinates)
interpolateControlPoints knotArray partialKey p1 p2 startIndex endIndex targetIndex accumulated =
    if targetIndex == startIndex || targetIndex == endIndex then
        accumulated

    else
        case interpolationParameter knotArray startIndex endIndex targetIndex of
            Just t ->
                Dict.insert (insert targetIndex partialKey) (Point3d.interpolateFrom p1 p2 t) accumulated

            Nothing ->
                accumulated


interpolationParameter : Array Float -> Int -> Int -> Int -> Maybe Float
interpolationParameter knotArray startIndex endIndex targetIndex =
    Maybe.map3
        (\uStart uEnd uTarget ->
            (uTarget - uStart) / (uEnd - uStart)
        )
        (Array.get startIndex knotArray)
        (Array.get endIndex knotArray)
        (Array.get targetIndex knotArray)


{-| For a given degree and set of B-spline knots, return the corresponding
intervals between knots that correspond to individual spline [segments](#bSplineSegments).
-}
bSplineIntervals : Int -> List Float -> List (Interval Float)
bSplineIntervals givenDegree givenKnots =
    if givenDegree < 1 then
        []

    else
        let
            trimmedKnots =
                List.sort givenKnots
                    |> List.drop (givenDegree - 1)
                    |> List.take (List.length givenKnots - 2 * (givenDegree - 1))
        in
        bSplineIntervalsHelp trimmedKnots []


bSplineIntervalsHelp : List Float -> List (Interval Float) -> List (Interval Float)
bSplineIntervalsHelp givenKnots accumulated =
    case givenKnots of
        first :: ((second :: remaining) as rest) ->
            if first < second then
                bSplineIntervalsHelp rest (Interval.from first second :: accumulated)

            else
                bSplineIntervalsHelp rest accumulated

        _ ->
            List.reverse accumulated


{-| Convert a spline from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Spline3d units1 coordinates -> Spline3d units2 coordinates
at rate (Types.Spline3d first rest) =
    Types.Spline3d (Point3d.at rate first) (List.map (Point3d.at rate) rest)


{-| Convert a spline from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Spline3d units1 coordinates -> Spline3d units2 coordinates
at_ rate spline =
    at (Quantity.inverse rate) spline


{-| Get the start point of a spline.
-}
startPoint : Spline3d units coordinates -> Point3d units coordinates
startPoint spline =
    pointOn spline 0


{-| Get the end point of a spline.
-}
endPoint : Spline3d units coordinates -> Point3d units coordinates
endPoint spline =
    pointOn spline 1


{-| Get the start derivative of a spline.
-}
startDerivative : Spline3d units coordinates -> Vector3d units coordinates
startDerivative spline =
    firstDerivative spline 0


{-| Get the end derivative of a spline.
-}
endDerivative : Spline3d units coordinates -> Vector3d units coordinates
endDerivative spline =
    firstDerivative spline 1


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
volume than the spline itself).
-}
boundingBox : Spline3d units coordinates -> BoundingBox3d units coordinates
boundingBox spline =
    let
        (Types.Spline3d first rest) =
            spline
    in
    BoundingBox3d.hull first rest


{-| Get a point on a spline at a given parameter value.
-}
pointOn : Spline3d units coordinates -> Float -> Point3d units coordinates
pointOn spline parameterValue =
    let
        (Types.Spline3d first rest) =
            spline
    in
    evaluatePoint first rest parameterValue


evaluatePoint : Point3d units coordinates -> List (Point3d units coordinates) -> Float -> Point3d units coordinates
evaluatePoint first rest t =
    case collapsePoints first rest t of
        newFirst :: newRest ->
            evaluatePoint newFirst newRest t

        [] ->
            first


collapsePoints :
    Point3d units coordinates
    -> List (Point3d units coordinates)
    -> Float
    -> List (Point3d units coordinates)
collapsePoints first rest t =
    case rest of
        second :: remaining ->
            Point3d.interpolateFrom first second t :: collapsePoints second remaining t

        [] ->
            []


{-| Approximate a spline by a given number of line segments. Note that the
number of points in the polyline will be one more than the number of segments.
-}
segments : Int -> Spline3d units coordinates -> Polyline3d units coordinates
segments numSegments spline =
    Polyline3d.fromVertices (Parameter1d.steps numSegments (pointOn spline))


{-| Approximate a spline as a polyline, within a given tolerance. Every point on
the returned polyline will be within the given tolerance of the spline.
-}
approximate : Quantity Float units -> Spline3d units coordinates -> Polyline3d units coordinates
approximate maxError spline =
    segments (numApproximationSegments maxError spline) spline


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.
-}
reverse : Spline3d units coordinates -> Spline3d units coordinates
reverse spline =
    let
        (Types.Spline3d first rest) =
            spline
    in
    reverseImpl first rest []


reverseImpl :
    Point3d units coordinates
    -> List (Point3d units coordinates)
    -> List (Point3d units coordinates)
    -> Spline3d units coordinates
reverseImpl current rest accumulated =
    case rest of
        next :: remaining ->
            reverseImpl next remaining (current :: accumulated)

        [] ->
            Types.Spline3d current accumulated


{-| Scale a spline about the given center point by the given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> Spline3d units coordinates -> Spline3d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point3d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> Spline3d units coordinates -> Spline3d units coordinates
rotateAround axis angle spline =
    mapControlPoints (Point3d.rotateAround axis angle) spline


{-| Translate a spline by a given displacement.
-}
translateBy : Vector3d units coordinates -> Spline3d units coordinates -> Spline3d units coordinates
translateBy displacement spline =
    mapControlPoints (Point3d.translateBy displacement) spline


{-| Translate a spline in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> Spline3d units coordinates -> Spline3d units coordinates
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across a plane.
-}
mirrorAcross : Plane3d units coordinates -> Spline3d units coordinates -> Spline3d units coordinates
mirrorAcross plane spline =
    mapControlPoints (Point3d.mirrorAcross plane) spline


{-| Find the orthographic projection of a spline onto a plane.
-}
projectOnto : Plane3d units coordinates -> Spline3d units coordinates -> Spline3d units coordinates
projectOnto plane spline =
    mapControlPoints (Point3d.projectOnto plane) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Spline3d units globalCoordinates
    -> Spline3d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point3d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.
-}
placeIn :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Spline3d units localCoordinates
    -> Spline3d units globalCoordinates
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| Project a spline into a given sketch plane.
-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Spline3d units coordinates3d -> Spline2d units coordinates2d
projectInto sketchPlane spline =
    let
        (Types.Spline3d first rest) =
            spline

        project =
            Point3d.projectInto sketchPlane
    in
    Types.Spline2d (project first) (List.map project rest)


mapControlPoints :
    (Point3d units1 coordinates1 -> Point3d units2 coordinates2)
    -> Spline3d units1 coordinates1
    -> Spline3d units2 coordinates2
mapControlPoints function spline =
    let
        (Types.Spline3d first rest) =
            spline
    in
    Types.Spline3d (function first) (List.map function rest)


{-| Split a spline into two roughly equal halves. Equivalent to
`Spline3d.splitAt 0.5`.
-}
bisect : Spline3d units coordinates -> ( Spline3d units coordinates, Spline3d units coordinates )
bisect spline =
    splitAt 0.5 spline


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.
-}
splitAt : Float -> Spline3d units coordinates -> ( Spline3d units coordinates, Spline3d units coordinates )
splitAt parameterValue spline =
    let
        (Types.Spline3d first rest) =
            spline
    in
    splitHelp parameterValue first first rest [] [] []


splitHelp :
    Float
    -> Point3d units coordinates
    -> Point3d units coordinates
    -> List (Point3d units coordinates)
    -> List (Point3d units coordinates)
    -> List (Point3d units coordinates)
    -> List (Point3d units coordinates)
    -> ( Spline3d units coordinates, Spline3d units coordinates )
splitHelp parameterValue left right toProcess interpolated leftAccumulated rightAccumulated =
    case toProcess of
        next :: remaining ->
            splitHelp parameterValue
                left
                next
                remaining
                (Point3d.interpolateFrom right next parameterValue :: interpolated)
                leftAccumulated
                rightAccumulated

        [] ->
            case List.reverse interpolated of
                first :: rest ->
                    splitHelp parameterValue
                        first
                        first
                        rest
                        []
                        (left :: leftAccumulated)
                        (right :: rightAccumulated)

                [] ->
                    ( reverseImpl left leftAccumulated []
                    , Types.Spline3d right rightAccumulated
                    )


{-| Get the first derivative of a spline at a given parameter value.
-}
firstDerivative : Spline3d units coordinates -> Float -> Vector3d units coordinates
firstDerivative spline parameterValue =
    let
        (Types.Spline3d first rest) =
            spline
    in
    case displacements first rest [] of
        firstVector :: remainingVectors ->
            evaluateVector firstVector remainingVectors parameterValue
                |> Vector3d.scaleBy (toFloat (degree spline))

        [] ->
            -- Spline is a single point
            Vector3d.zero


displacements : Point3d units coordinates -> List (Point3d units coordinates) -> List (Vector3d units coordinates) -> List (Vector3d units coordinates)
displacements first rest accumulated =
    case rest of
        next :: remaining ->
            displacements next remaining (Vector3d.from first next :: accumulated)

        [] ->
            List.reverse accumulated


evaluateVector : Vector3d units coordinates -> List (Vector3d units coordinates) -> Float -> Vector3d units coordinates
evaluateVector first rest t =
    case collapseVectors first rest t of
        newFirst :: newRest ->
            evaluateVector newFirst newRest t

        [] ->
            first


collapseVectors :
    Vector3d units coordinates
    -> List (Vector3d units coordinates)
    -> Float
    -> List (Vector3d units coordinates)
collapseVectors first rest t =
    case rest of
        second :: remaining ->
            Vector3d.interpolateFrom first second t :: collapseVectors second remaining t

        [] ->
            []


{-| Get the second derivative value at a point along a spline.
-}
secondDerivative : Spline3d units coordinates -> Float -> Vector3d units coordinates
secondDerivative spline parameterValue =
    let
        (Types.Spline3d firstControlPoint remainingControlPoints) =
            spline

        n =
            degree spline
    in
    case differentiate (displacements firstControlPoint remainingControlPoints []) of
        first :: rest ->
            evaluateVector first rest parameterValue
                |> Vector3d.scaleBy (toFloat (n * (n - 1)))

        [] ->
            Vector3d.zero


differentiate :
    List (Vector3d units coordinates)
    -> List (Vector3d units coordinates)
differentiate vectors =
    case vectors of
        first :: rest ->
            differentiateHelp first rest []

        [] ->
            []


differentiateHelp :
    Vector3d units coordinates
    -> List (Vector3d units coordinates)
    -> List (Vector3d units coordinates)
    -> List (Vector3d units coordinates)
differentiateHelp first rest accumulated =
    case rest of
        second :: remaining ->
            differentiateHelp second remaining <|
                ((second |> Vector3d.minus first) :: accumulated)

        [] ->
            List.reverse accumulated


{-| Get the bounds on the first derivative of a spline.
-}
firstDerivativeBoundingBox : Spline3d units coordinates -> VectorBoundingBox3d units coordinates
firstDerivativeBoundingBox spline =
    let
        (Types.Spline3d firstControlPoint remainingControlPoints) =
            spline
    in
    case displacements firstControlPoint remainingControlPoints [] of
        first :: rest ->
            VectorBoundingBox3d.hull first rest
                |> VectorBoundingBox3d.multiplyBy (toFloat (degree spline))

        [] ->
            VectorBoundingBox3d.singleton Vector3d.zero


{-| Get the bounds on the first derivative of a spline.
-}
secondDerivativeBoundingBox : Spline3d units coordinates -> VectorBoundingBox3d units coordinates
secondDerivativeBoundingBox spline =
    let
        (Types.Spline3d firstControlPoint remainingControlPoints) =
            spline
    in
    case differentiate (displacements firstControlPoint remainingControlPoints []) of
        first :: rest ->
            let
                n =
                    degree spline
            in
            VectorBoundingBox3d.hull first rest
                |> VectorBoundingBox3d.multiplyBy (toFloat (n * (n - 1)))

        [] ->
            VectorBoundingBox3d.singleton Vector3d.zero


{-| Find a conservative upper bound on the magnitude of the second derivative of
a spline. This can be useful when determining error bounds for various kinds of
linear approximations.
-}
maxSecondDerivativeMagnitude : Spline3d units coordinates -> Quantity Float units
maxSecondDerivativeMagnitude spline =
    Quantity.Interval.maxValue (VectorBoundingBox3d.length (secondDerivativeBoundingBox spline))


{-| Determine the number of linear segments needed to approximate a spline to
within a given tolerance.
-}
numApproximationSegments : Quantity Float units -> Spline3d units coordinates -> Int
numApproximationSegments maxError spline =
    Curve.numApproximationSegments
        { maxError = maxError
        , maxSecondDerivativeMagnitude = maxSecondDerivativeMagnitude spline
        }
