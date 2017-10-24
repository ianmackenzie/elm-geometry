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
    = ArcLengthParameterization SegmentTree


type SegmentTree
    = Node
        { lengthAtStart : Float
        , lengthAtEnd : Float
        , paramAtStart : Float
        , leftBranch : SegmentTree
        , rightBranch : SegmentTree
        }
    | Leaf
        { lengthAtStart : Float
        , lengthAtEnd : Float
        , paramAtStart : Float
        , lengthAtEnd : Float
        , paramAtEnd : Float
        }


{-| Build an arc length parameterization for a particular curve, within a given
tolerance. A `Config` value must be supplied that defines how to bisect the
curve and how to determine bounds on the magnitude of its first derivative.
-}
buildParameterization : Config -> ArcLengthParameterization
buildParameterization config =
    let
        { tolerance, derivativeMagnitude, maxSecondDerivativeMagnitude } =
            config

        height =
            if tolerance <= 0 then
                0
            else
                let
                    numSegments =
                        maxSecondDerivativeMagnitude / (8 * tolerance)
                in
                max 0 (ceiling (logBase 2 numSegments))
    in
    ArcLengthParameterization (buildTree derivativeMagnitude 0 0 1 height)


{-| Type used as argument to `buildParameterization`.
-}
type alias Config =
    { tolerance : Float
    , derivativeMagnitude : Float -> Float
    , maxSecondDerivativeMagnitude : Float
    }


buildTree : (Float -> Float) -> Float -> Float -> Float -> Int -> SegmentTree
buildTree derivativeMagnitude lengthAtStart paramAtStart paramAtEnd height =
    let
        paramDelta =
            paramAtEnd - paramAtStart

        paramAtMid =
            paramAtStart + 0.5 * paramDelta
    in
    if height == 0 then
        let
            segmentLength =
                derivativeMagnitude paramAtMid * paramDelta

            lengthAtEnd =
                lengthAtStart + segmentLength
        in
        Leaf
            { lengthAtStart = lengthAtStart
            , lengthAtEnd = lengthAtEnd
            , paramAtStart = paramAtStart
            , paramAtEnd = paramAtEnd
            }
    else
        let
            branchHeight =
                height - 1

            leftBranch =
                buildTree derivativeMagnitude
                    lengthAtStart
                    paramAtStart
                    paramAtMid
                    branchHeight

            lengthAtLeftEnd =
                lengthAtEnd leftBranch

            rightBranch =
                buildTree derivativeMagnitude
                    lengthAtLeftEnd
                    paramAtMid
                    paramAtEnd
                    branchHeight
        in
        Node
            { lengthAtStart = lengthAtStart
            , lengthAtEnd = lengthAtEnd rightBranch
            , paramAtStart = paramAtStart
            , leftBranch = leftBranch
            , rightBranch = rightBranch
            }


{-| Get the arc length parameterization of a parameterized curve.
-}
parameterization : ArcLengthParameterized c -> ArcLengthParameterization
parameterization (ArcLengthParameterized _ parameterization) =
    parameterization


{-| Get the total arc length of a curve given its arc length parameterization.
-}
fromParameterization : ArcLengthParameterization -> Float
fromParameterization (ArcLengthParameterization tree) =
    lengthAtEnd tree


{-| Convert an arc length to the corresponding parameter value. If the given
arc length is less than zero or greater than the total arc length of the curve,
returns `Nothing`.
-}
toParameterValue : ArcLengthParameterization -> Float -> Maybe Float
toParameterValue (ArcLengthParameterization tree) s =
    if s >= 0 && s <= lengthAtEnd tree then
        Just (unsafeToParameterValue tree s)
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


lengthAtEnd : SegmentTree -> Float
lengthAtEnd tree =
    case tree of
        Node { lengthAtEnd } ->
            lengthAtEnd

        Leaf { lengthAtEnd } ->
            lengthAtEnd


{-| Convert a parameter value to the corresponding arc length. If the given
parameter value is less than zero or greater than one, returns `Nothing`.
-}
fromParameterValue : ArcLengthParameterization -> Float -> Maybe Float
fromParameterValue (ArcLengthParameterization tree) t =
    if t >= 0 && t <= 1 then
        Just (unsafeFromParameterValue tree t)
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
