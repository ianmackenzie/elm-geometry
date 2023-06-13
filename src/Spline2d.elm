--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Spline2d exposing
    ( Spline2d
    , fromControlPoints, fromQuadraticSpline, fromCubicSpline
    , bSplineSegments, bSplineIntervals
    , controlPoints, degree, startPoint, endPoint, startDerivative, endDerivative, boundingBox
    , pointOn
    , segments, approximate
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    , bisect, splitAt
    , firstDerivative, secondDerivative, firstDerivativeBoundingBox, secondDerivativeBoundingBox, maxSecondDerivativeMagnitude, numApproximationSegments
    )

{-| A `Spline2d` is a [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by a list of control points. This module contains functionality
for

  - Constructing splines
  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

In general you will want to use a [`QuadraticSpline2d`](#QuadraticSpline2d) or
[`CubicSpline2d`](#CubicSpline2d) instead, but a `Spline2d` can be useful if you
need to support arbitrary spline degrees (for example quartic or quintic
splines) or if the spline degree is not known at compile time.

@docs Spline2d


# Constructors

@docs fromControlPoints, fromQuadraticSpline, fromCubicSpline


## B-splines

@docs bSplineSegments, bSplineIntervals


# Properties

@docs controlPoints, degree, startPoint, endPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn


# Linear approximation

@docs segments, approximate


# Transformations

These transformations generally behave just like [the ones in the `Point2d`
module](Point2d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


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
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import CubicSpline2d exposing (CubicSpline2d)
import Curve
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types exposing (QuadraticSpline2d, VectorBoundingBox2d)
import Interval exposing (Interval)
import Parameter1d
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity(..), Rate)
import Quantity.Interval
import Vector2d exposing (Vector2d)
import VectorBoundingBox2d exposing (VectorBoundingBox2d)


{-| -}
type alias Spline2d units coordinates =
    Types.Spline2d units coordinates


{-| Construct a spline from its control points. In order to guarantee that a
spline has at least one control point, you must pass the first and remaining
control points separately.
-}
fromControlPoints : Point2d units coordinates -> List (Point2d units coordinates) -> Spline2d units coordinates
fromControlPoints firstControlPoint remainingControlPoints =
    Types.Spline2d firstControlPoint remainingControlPoints


{-| Convert a `QuadraticSpline2d` to a generic `Spline2d`.
-}
fromQuadraticSpline : QuadraticSpline2d units coordinates -> Spline2d units coordinates
fromQuadraticSpline quadraticSpline =
    fromControlPoints
        (QuadraticSpline2d.firstControlPoint quadraticSpline)
        [ QuadraticSpline2d.secondControlPoint quadraticSpline
        , QuadraticSpline2d.thirdControlPoint quadraticSpline
        ]


{-| Convert a `CubicSpline2d` to a generic `Spline2d`.
-}
fromCubicSpline : CubicSpline2d units coordinates -> Spline2d units coordinates
fromCubicSpline cubicSpline =
    fromControlPoints
        (CubicSpline2d.firstControlPoint cubicSpline)
        [ CubicSpline2d.secondControlPoint cubicSpline
        , CubicSpline2d.thirdControlPoint cubicSpline
        , CubicSpline2d.fourthControlPoint cubicSpline
        ]


{-| Get the control points of a spline as a list.
-}
controlPoints : Spline2d units coordinates -> List (Point2d units coordinates)
controlPoints spline =
    let
        (Types.Spline2d first rest) =
            spline
    in
    first :: rest


{-| Get the polynomial degree of a spline; this is equal to the number of
control points minus one. A single point has degree 0, a line segment has
degree 1, a quadratic spline has degree 2, a cubic spline has degree 3, etc.
-}
degree : Spline2d units coordinates -> Int
degree spline =
    let
        (Types.Spline2d _ rest) =
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
bSplineSegments : Int -> List Float -> List (Point2d units coordinates) -> List (Spline2d units coordinates)
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


bSplineSegmentsHelp : Int -> Int -> List Float -> List (Point2d units coordinates) -> List (Spline2d units coordinates)
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


bSplineSegment : Int -> List Float -> List (Point2d units coordinates) -> List (Spline2d units coordinates)
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
                [ Types.Spline2d start [ end ] ]

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
                [ Types.Spline2d first rest ]

            [] ->
                []


initialControlPoint : Int -> Int -> Point2d units coordinates -> ( List Int, Point2d units coordinates )
initialControlPoint givenDegree index point =
    ( List.range index (index + givenDegree - 1)
    , point
    )


bSplineSegmentHelp :
    Int
    -> Int
    -> Array Float
    -> Dict (List Int) (Point2d units coordinates)
    -> List (Point2d units coordinates)
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
    -> Dict (List Int) (Point2d units coordinates)
    -> List Int
    -> Point2d units coordinates
    -> Dict (List Int) (Point2d units coordinates)
    -> Dict (List Int) (Point2d units coordinates)
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
    -> Point2d units coordinates
    -> Point2d units coordinates
    -> Int
    -> Int
    -> Int
    -> Dict (List Int) (Point2d units coordinates)
    -> Dict (List Int) (Point2d units coordinates)
interpolateControlPoints knotArray partialKey p1 p2 startIndex endIndex targetIndex accumulated =
    if targetIndex == startIndex || targetIndex == endIndex then
        accumulated

    else
        case interpolationParameter knotArray startIndex endIndex targetIndex of
            Just t ->
                Dict.insert (insert targetIndex partialKey) (Point2d.interpolateFrom p1 p2 t) accumulated

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
at : Quantity Float (Rate units2 units1) -> Spline2d units1 coordinates -> Spline2d units2 coordinates
at rate (Types.Spline2d first rest) =
    Types.Spline2d (Point2d.at rate first) (List.map (Point2d.at rate) rest)


{-| Convert a spline from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Spline2d units1 coordinates -> Spline2d units2 coordinates
at_ rate spline =
    at (Quantity.inverse rate) spline


{-| Get the start point of a spline.
-}
startPoint : Spline2d units coordinates -> Point2d units coordinates
startPoint spline =
    pointOn spline 0


{-| Get the end point of a spline.
-}
endPoint : Spline2d units coordinates -> Point2d units coordinates
endPoint spline =
    pointOn spline 1


{-| Get the start derivative of a spline.
-}
startDerivative : Spline2d units coordinates -> Vector2d units coordinates
startDerivative spline =
    firstDerivative spline 0


{-| Get the end derivative of a spline.
-}
endDerivative : Spline2d units coordinates -> Vector2d units coordinates
endDerivative spline =
    firstDerivative spline 1


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
volume than the spline itself).
-}
boundingBox : Spline2d units coordinates -> BoundingBox2d units coordinates
boundingBox spline =
    let
        (Types.Spline2d first rest) =
            spline
    in
    BoundingBox2d.hull first rest


{-| Get a point on a spline at a given parameter value.
-}
pointOn : Spline2d units coordinates -> Float -> Point2d units coordinates
pointOn spline parameterValue =
    let
        (Types.Spline2d first rest) =
            spline
    in
    evaluatePoint first rest parameterValue


evaluatePoint : Point2d units coordinates -> List (Point2d units coordinates) -> Float -> Point2d units coordinates
evaluatePoint first rest t =
    case collapsePoints first rest t of
        newFirst :: newRest ->
            evaluatePoint newFirst newRest t

        [] ->
            first


collapsePoints :
    Point2d units coordinates
    -> List (Point2d units coordinates)
    -> Float
    -> List (Point2d units coordinates)
collapsePoints first rest t =
    case rest of
        second :: remaining ->
            Point2d.interpolateFrom first second t :: collapsePoints second remaining t

        [] ->
            []


{-| Approximate a spline by a given number of line segments. Note that the
number of points in the polyline will be one more than the number of segments.
-}
segments : Int -> Spline2d units coordinates -> Polyline2d units coordinates
segments numSegments spline =
    Polyline2d.fromVertices (Parameter1d.steps numSegments (pointOn spline))


{-| Approximate a spline as a polyline, within a given tolerance. Every point on
the returned polyline will be within the given tolerance of the spline.
-}
approximate : Quantity Float units -> Spline2d units coordinates -> Polyline2d units coordinates
approximate maxError spline =
    segments (numApproximationSegments maxError spline) spline


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.
-}
reverse : Spline2d units coordinates -> Spline2d units coordinates
reverse spline =
    let
        (Types.Spline2d first rest) =
            spline
    in
    reverseImpl first rest []


reverseImpl :
    Point2d units coordinates
    -> List (Point2d units coordinates)
    -> List (Point2d units coordinates)
    -> Spline2d units coordinates
reverseImpl current rest accumulated =
    case rest of
        next :: remaining ->
            reverseImpl next remaining (current :: accumulated)

        [] ->
            Types.Spline2d current accumulated


{-| Scale a spline about the given center point by the given scale.
-}
scaleAbout : Point2d units coordinates -> Float -> Spline2d units coordinates -> Spline2d units coordinates
scaleAbout point scale spline =
    mapControlPoints (Point2d.scaleAbout point scale) spline


{-| Rotate a spline counterclockwise around a given axis by a given angle.
-}
rotateAround : Point2d units coordinates -> Angle -> Spline2d units coordinates -> Spline2d units coordinates
rotateAround point angle spline =
    mapControlPoints (Point2d.rotateAround point angle) spline


{-| Translate a spline by a given displacement.
-}
translateBy : Vector2d units coordinates -> Spline2d units coordinates -> Spline2d units coordinates
translateBy displacement spline =
    mapControlPoints (Point2d.translateBy displacement) spline


{-| Translate a spline in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Spline2d units coordinates -> Spline2d units coordinates
translateIn direction distance spline =
    translateBy (Vector2d.withLength distance direction) spline


{-| Mirror a spline across a plane.
-}
mirrorAcross : Axis2d units coordinates -> Spline2d units coordinates -> Spline2d units coordinates
mirrorAcross axis spline =
    mapControlPoints (Point2d.mirrorAcross axis) spline


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Spline2d units globalCoordinates
    -> Spline2d units localCoordinates
relativeTo frame spline =
    mapControlPoints (Point2d.relativeTo frame) spline


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.
-}
placeIn :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Spline2d units localCoordinates
    -> Spline2d units globalCoordinates
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


mapControlPoints :
    (Point2d units1 coordinates1 -> Point2d units2 coordinates2)
    -> Spline2d units1 coordinates1
    -> Spline2d units2 coordinates2
mapControlPoints function spline =
    let
        (Types.Spline2d first rest) =
            spline
    in
    Types.Spline2d (function first) (List.map function rest)


{-| Split a spline into two roughly equal halves. Equivalent to
`Spline2d.splitAt 0.5`.
-}
bisect : Spline2d units coordinates -> ( Spline2d units coordinates, Spline2d units coordinates )
bisect spline =
    splitAt 0.5 spline


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.
-}
splitAt : Float -> Spline2d units coordinates -> ( Spline2d units coordinates, Spline2d units coordinates )
splitAt parameterValue spline =
    let
        (Types.Spline2d first rest) =
            spline
    in
    splitHelp parameterValue first first rest [] [] []


splitHelp :
    Float
    -> Point2d units coordinates
    -> Point2d units coordinates
    -> List (Point2d units coordinates)
    -> List (Point2d units coordinates)
    -> List (Point2d units coordinates)
    -> List (Point2d units coordinates)
    -> ( Spline2d units coordinates, Spline2d units coordinates )
splitHelp parameterValue left right toProcess interpolated leftAccumulated rightAccumulated =
    case toProcess of
        next :: remaining ->
            splitHelp parameterValue
                left
                next
                remaining
                (Point2d.interpolateFrom right next parameterValue :: interpolated)
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
                    , Types.Spline2d right rightAccumulated
                    )


{-| Get the first derivative of a spline at a given parameter value.
-}
firstDerivative : Spline2d units coordinates -> Float -> Vector2d units coordinates
firstDerivative spline parameterValue =
    let
        (Types.Spline2d first rest) =
            spline
    in
    case displacements first rest [] of
        firstVector :: remainingVectors ->
            evaluateVector firstVector remainingVectors parameterValue
                |> Vector2d.scaleBy (toFloat (degree spline))

        [] ->
            -- Spline is a single point
            Vector2d.zero


displacements : Point2d units coordinates -> List (Point2d units coordinates) -> List (Vector2d units coordinates) -> List (Vector2d units coordinates)
displacements first rest accumulated =
    case rest of
        next :: remaining ->
            displacements next remaining (Vector2d.from first next :: accumulated)

        [] ->
            List.reverse accumulated


evaluateVector : Vector2d units coordinates -> List (Vector2d units coordinates) -> Float -> Vector2d units coordinates
evaluateVector first rest t =
    case collapseVectors first rest t of
        newFirst :: newRest ->
            evaluateVector newFirst newRest t

        [] ->
            first


collapseVectors :
    Vector2d units coordinates
    -> List (Vector2d units coordinates)
    -> Float
    -> List (Vector2d units coordinates)
collapseVectors first rest t =
    case rest of
        second :: remaining ->
            Vector2d.interpolateFrom first second t :: collapseVectors second remaining t

        [] ->
            []


{-| Get the second derivative value at a point along a spline.
-}
secondDerivative : Spline2d units coordinates -> Float -> Vector2d units coordinates
secondDerivative spline parameterValue =
    let
        (Types.Spline2d firstControlPoint remainingControlPoints) =
            spline

        n =
            degree spline
    in
    case differentiate (displacements firstControlPoint remainingControlPoints []) of
        first :: rest ->
            evaluateVector first rest parameterValue
                |> Vector2d.scaleBy (toFloat (n * (n - 1)))

        [] ->
            Vector2d.zero


differentiate :
    List (Vector2d units coordinates)
    -> List (Vector2d units coordinates)
differentiate vectors =
    case vectors of
        first :: rest ->
            differentiateHelp first rest []

        [] ->
            []


differentiateHelp :
    Vector2d units coordinates
    -> List (Vector2d units coordinates)
    -> List (Vector2d units coordinates)
    -> List (Vector2d units coordinates)
differentiateHelp first rest accumulated =
    case rest of
        second :: remaining ->
            differentiateHelp second remaining <|
                ((second |> Vector2d.minus first) :: accumulated)

        [] ->
            List.reverse accumulated


{-| Get the bounds on the first derivative of a spline.
-}
firstDerivativeBoundingBox : Spline2d units coordinates -> VectorBoundingBox2d units coordinates
firstDerivativeBoundingBox spline =
    let
        (Types.Spline2d firstControlPoint remainingControlPoints) =
            spline
    in
    case displacements firstControlPoint remainingControlPoints [] of
        first :: rest ->
            VectorBoundingBox2d.hull first rest
                |> VectorBoundingBox2d.multiplyBy (toFloat (degree spline))

        [] ->
            VectorBoundingBox2d.singleton Vector2d.zero


{-| Get the bounds on the first derivative of a spline.
-}
secondDerivativeBoundingBox : Spline2d units coordinates -> VectorBoundingBox2d units coordinates
secondDerivativeBoundingBox spline =
    let
        (Types.Spline2d firstControlPoint remainingControlPoints) =
            spline
    in
    case differentiate (displacements firstControlPoint remainingControlPoints []) of
        first :: rest ->
            let
                n =
                    degree spline
            in
            VectorBoundingBox2d.hull first rest
                |> VectorBoundingBox2d.multiplyBy (toFloat (n * (n - 1)))

        [] ->
            VectorBoundingBox2d.singleton Vector2d.zero


{-| Find a conservative upper bound on the magnitude of the second derivative of
a spline. This can be useful when determining error bounds for various kinds of
linear approximations.
-}
maxSecondDerivativeMagnitude : Spline2d units coordinates -> Quantity Float units
maxSecondDerivativeMagnitude spline =
    Quantity.Interval.maxValue (VectorBoundingBox2d.length (secondDerivativeBoundingBox spline))


{-| Determine the number of linear segments needed to approximate a spline to
within a given tolerance.
-}
numApproximationSegments : Quantity Float units -> Spline2d units coordinates -> Int
numApproximationSegments maxError spline =
    Curve.numApproximationSegments
        { maxError = maxError
        , maxSecondDerivativeMagnitude = maxSecondDerivativeMagnitude spline
        }
