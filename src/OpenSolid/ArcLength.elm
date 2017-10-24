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
        { length0 : Float
        , length1 : Float
        , length2 : Float
        , length3 : Float
        , length4 : Float
        , param0 : Float
        , param1 : Float
        , param2 : Float
        , param3 : Float
        , param4 : Float
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
                max 0 (ceiling (logBase 2 (numSegments / 4)))
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
    in
    if height == 0 then
        let
            param0 =
                paramAtStart

            param1 =
                paramAtStart + 0.25 * paramDelta

            param2 =
                paramAtStart + 0.5 * paramDelta

            param3 =
                paramAtEnd - 0.25 * paramDelta

            param4 =
                paramAtEnd

            offset =
                0.125 * paramDelta

            paramStep =
                0.25 * paramDelta

            length0 =
                lengthAtStart

            length1 =
                length0 + derivativeMagnitude (param0 + offset) * paramStep

            length2 =
                length1 + derivativeMagnitude (param1 + offset) * paramStep

            length3 =
                length2 + derivativeMagnitude (param2 + offset) * paramStep

            length4 =
                length3 + derivativeMagnitude (param3 + offset) * paramStep
        in
        Leaf
            { param0 = param0
            , param1 = param1
            , param2 = param2
            , param3 = param3
            , param4 = param4
            , length0 = length0
            , length1 = length1
            , length2 = length2
            , length3 = length3
            , length4 = length4
            }
    else
        let
            branchHeight =
                height - 1

            paramAtMid =
                paramAtStart + 0.5 * paramDelta

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
        Leaf { length0, length1, length2, length3, length4, param0, param1, param2, param3, param4 } ->
            if s <= length2 then
                if s <= length1 then
                    let
                        lengthFraction =
                            (s - length0) / (length1 - length0)
                    in
                    Scalar.interpolateFrom param0 param1 lengthFraction
                else
                    let
                        lengthFraction =
                            (s - length1) / (length2 - length1)
                    in
                    Scalar.interpolateFrom param1 param2 lengthFraction
            else if s <= length3 then
                let
                    lengthFraction =
                        (s - length2) / (length3 - length2)
                in
                Scalar.interpolateFrom param2 param3 lengthFraction
            else
                let
                    lengthFraction =
                        (s - length3) / (length4 - length3)
                in
                Scalar.interpolateFrom param3 param4 lengthFraction

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

        Leaf { length0 } ->
            length0


lengthAtEnd : SegmentTree -> Float
lengthAtEnd tree =
    case tree of
        Node { lengthAtEnd } ->
            lengthAtEnd

        Leaf { length4 } ->
            length4


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
        Leaf { length0, length1, length2, length3, length4, param0, param1, param2, param3, param4 } ->
            if t <= param2 then
                if t <= param1 then
                    let
                        paramFraction =
                            (t - param0) / (param1 - param0)
                    in
                    Scalar.interpolateFrom length0 length1 paramFraction
                else
                    let
                        paramFraction =
                            (t - param1) / (param2 - param1)
                    in
                    Scalar.interpolateFrom length1 length2 paramFraction
            else if t <= param3 then
                let
                    paramFraction =
                        (t - param2) / (param3 - param2)
                in
                Scalar.interpolateFrom length2 length3 paramFraction
            else
                let
                    paramFraction =
                        (t - param3) / (param4 - param3)
                in
                Scalar.interpolateFrom length3 length4 paramFraction

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

        Leaf { param0 } ->
            param0
