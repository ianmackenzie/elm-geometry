module OpenSolid.ArcLength
    exposing
        ( ArcLengthParameterization
        , ArcLengthParameterized(..)
        , Config
        , buildParameterization
        , fromParameterValue
        , fromParameterization
        , parameterization
        , toParameterValue
        )

{-| _You will likely never need to use this module directly._ In the vast
majority of cases the individual curve modules such as `QuadraticSpline2d`
should contain all the functionality you need to construct an arc length
parameterization and use it to do things like evaluate a curve at evenly-spaced
points. This module is primarily for use internally by those curve modules.

@docs ArcLengthParameterized, ArcLengthParameterization


# Constructing

@docs buildParameterization, Config


# Evaluating

@docs parameterization, fromParameterization, toParameterValue, fromParameterValue

-}

import OpenSolid.Interval as Interval exposing (Interval)
import OpenSolid.Scalar as Scalar


{-| An arc length parameterized curve is simply a tuple containing the curve
itself along with its associated arc length parameterization. Different curve
modules contain functions for creating `ArcLengthParameterized` values and then
evaluating them.
-}
type ArcLengthParameterized c
    = ArcLengthParameterized c ArcLengthParameterization


{-| Contains a mapping from curve parameter value to arc length, and vice versa.
-}
type ArcLengthParameterization
    = ArcLengthParameterization Float SegmentTree


type SegmentTree
    = Node
        { lengthAtStart : Float
        , paramAtStart : Float
        , leftBranch : SegmentTree
        , rightBranch : SegmentTree
        }
    | Leaf
        { lengthAtStart : Float
        , paramAtStart : Float
        , lengthAtEnd : Float
        , paramAtEnd : Float
        }


{-| Build an arc length parameterization for a particular curve, within a given
tolerance. A `Config` value must be supplied that defines how to bisect the
curve and how to determine bounds on its 'speed' (the magnitude of its first
derivative).
-}
buildParameterization : Config c -> Float -> c -> ArcLengthParameterization
buildParameterization config tolerance curve =
    buildSegment config tolerance curve 0 0 1


{-| Type used as argument to `parameterization`.
-}
type alias Config c =
    { bisect : c -> ( c, c )
    , speed : c -> Interval
    }


buildSegment : Config c -> Float -> c -> Float -> Float -> Float -> ArcLengthParameterization
buildSegment config tolerance curve lengthAtStart paramAtStart paramAtEnd =
    let
        speed =
            config.speed curve

        paramDelta =
            paramAtEnd - paramAtStart

        lowerBound =
            Interval.minValue speed

        upperBound =
            Interval.maxValue speed

        maxError =
            (upperBound - lowerBound) / 2
    in
    if maxError <= tolerance || tolerance <= 0 || paramAtStart == paramAtEnd then
        let
            segmentLength =
                lowerBound + maxError

            lengthAtEnd =
                lengthAtStart + segmentLength

            leaf =
                Leaf
                    { lengthAtStart = lengthAtStart
                    , paramAtStart = paramAtStart
                    , lengthAtEnd = lengthAtEnd
                    , paramAtEnd = paramAtEnd
                    }
        in
        ArcLengthParameterization lengthAtEnd leaf
    else
        let
            ( leftCurve, rightCurve ) =
                config.bisect curve

            paramAtSplit =
                paramAtStart + 0.5 * paramDelta

            (ArcLengthParameterization lengthAtLeftEnd leftBranch) =
                buildSegment config
                    (0.5 * tolerance)
                    leftCurve
                    lengthAtStart
                    paramAtStart
                    paramAtSplit

            (ArcLengthParameterization lengthAtEnd rightBranch) =
                buildSegment config
                    (0.5 * tolerance)
                    rightCurve
                    lengthAtLeftEnd
                    paramAtSplit
                    paramAtEnd

            node =
                Node
                    { lengthAtStart = lengthAtStart
                    , paramAtStart = paramAtStart
                    , leftBranch = leftBranch
                    , rightBranch = rightBranch
                    }
        in
        ArcLengthParameterization lengthAtEnd node


{-| Get the arc length parameterization of a parameterized curve.
-}
parameterization : ArcLengthParameterized c -> ArcLengthParameterization
parameterization (ArcLengthParameterized _ parameterization) =
    parameterization


{-| Get the total arc length of a curve given its arc length parameterization.
-}
fromParameterization : ArcLengthParameterization -> Float
fromParameterization (ArcLengthParameterization arcLength _) =
    arcLength


{-| Convert an arc length to the corresponding parameter value. If the given
arc length is less than zero or greater than the total arc length of the curve,
returns `Nothing`.
-}
toParameterValue : ArcLengthParameterization -> Float -> Maybe Float
toParameterValue (ArcLengthParameterization arcLength segmentTree) s =
    if s >= 0 && s <= arcLength then
        Just (unsafeToParameterValue segmentTree s)
    else
        Nothing


unsafeToParameterValue : SegmentTree -> Float -> Float
unsafeToParameterValue tree s =
    case tree of
        Leaf { lengthAtStart, paramAtStart, lengthAtEnd, paramAtEnd } ->
            let
                lengthFraction =
                    (s - lengthAtStart) / (lengthAtEnd - lengthAtStart)
            in
            Scalar.interpolateFrom paramAtStart paramAtEnd lengthFraction

        Node { leftBranch, rightBranch } ->
            if s < lengthAtStart rightBranch then
                unsafeToParameterValue leftBranch s
            else
                unsafeToParameterValue rightBranch s


lengthAtStart : SegmentTree -> Float
lengthAtStart tree =
    case tree of
        Node { lengthAtStart } ->
            lengthAtStart

        Leaf { lengthAtStart } ->
            lengthAtStart


{-| Convert a parameter value to the corresponding arc length. If the given
parameter value is less than zero or greater than one, returns `Nothing`.
-}
fromParameterValue : ArcLengthParameterization -> Float -> Maybe Float
fromParameterValue (ArcLengthParameterization arcLength segmentTree) t =
    if t >= 0 && t <= 1 then
        Just (unsafeFromParameterValue segmentTree t)
    else
        Nothing


unsafeFromParameterValue : SegmentTree -> Float -> Float
unsafeFromParameterValue tree t =
    case tree of
        Leaf { lengthAtStart, paramAtStart, lengthAtEnd, paramAtEnd } ->
            let
                paramFraction =
                    (t - paramAtStart) / (paramAtEnd - paramAtStart)
            in
            Scalar.interpolateFrom lengthAtStart lengthAtEnd paramFraction

        Node { leftBranch, rightBranch } ->
            if t < paramAtStart rightBranch then
                unsafeFromParameterValue leftBranch t
            else
                unsafeFromParameterValue rightBranch t


paramAtStart : SegmentTree -> Float
paramAtStart tree =
    case tree of
        Node { paramAtStart } ->
            paramAtStart

        Leaf { paramAtStart } ->
            paramAtStart
