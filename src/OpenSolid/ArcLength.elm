module OpenSolid.ArcLength
    exposing
        ( Parameterization
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

@docs Parameterization


# Constructing

@docs parameterization


# Evaluating

@docs fromParameterization, toParameterValue, fromParameterValue

-}

import OpenSolid.Scalar as Scalar


{-| Contains a mapping from curve parameter value to arc length, and vice versa.
-}
type Parameterization
    = Parameterization SegmentTree


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
        , length5 : Float
        , length6 : Float
        , length7 : Float
        , length8 : Float
        , param0 : Float
        , param1 : Float
        , param2 : Float
        , param3 : Float
        , param4 : Float
        , param5 : Float
        , param6 : Float
        , param7 : Float
        , param8 : Float
        }


segmentsPerLeaf : Int
segmentsPerLeaf =
    8


{-| Build an arc length parameterization for some curve. You must supply:

  - A `derivativeMagnitude` function that returns the magnitude of the first
    derivative of the curve at a given parameter value
  - The maximum magnitude of the second derivative of the curve
  - A tolerance specifying the maximum error of the resulting parameterization

Curve parameter values are assumed to be in the range [0,1].

-}
parameterization : { tolerance : Float, derivativeMagnitude : Float -> Float, maxSecondDerivativeMagnitude : Float } -> Parameterization
parameterization config =
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

                    numLeaves =
                        numSegments / toFloat segmentsPerLeaf
                in
                max 0 (ceiling (logBase 2 numLeaves))
    in
    Parameterization (buildTree derivativeMagnitude 0 0 1 height)


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
                paramAtStart + 0.125 * paramDelta

            param2 =
                paramAtStart + 0.25 * paramDelta

            param3 =
                paramAtStart + 0.375 * paramDelta

            param4 =
                paramAtStart + 0.5 * paramDelta

            param5 =
                paramAtEnd - 0.375 * paramDelta

            param6 =
                paramAtEnd - 0.25 * paramDelta

            param7 =
                paramAtEnd - 0.125 * paramDelta

            param8 =
                paramAtEnd

            offset =
                0.0625 * paramDelta

            paramStep =
                0.125 * paramDelta

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

            length5 =
                length4 + derivativeMagnitude (param4 + offset) * paramStep

            length6 =
                length5 + derivativeMagnitude (param5 + offset) * paramStep

            length7 =
                length6 + derivativeMagnitude (param6 + offset) * paramStep

            length8 =
                length7 + derivativeMagnitude (param7 + offset) * paramStep
        in
        Leaf
            { param0 = param0
            , param1 = param1
            , param2 = param2
            , param3 = param3
            , param4 = param4
            , param5 = param5
            , param6 = param6
            , param7 = param7
            , param8 = param8
            , length0 = length0
            , length1 = length1
            , length2 = length2
            , length3 = length3
            , length4 = length4
            , length5 = length5
            , length6 = length6
            , length7 = length7
            , length8 = length8
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


{-| Convert an arc length to the corresponding parameter value. If the given
arc length is less than zero or greater than the total arc length of the curve,
returns `Nothing`.
-}
toParameterValue : Parameterization -> Float -> Maybe Float
toParameterValue (Parameterization tree) s =
    if s >= 0 && s <= lengthAtEnd tree then
        Just (unsafeToParameterValue tree s)
    else
        Nothing


unsafeToParameterValue : SegmentTree -> Float -> Float
unsafeToParameterValue tree s =
    case tree of
        Leaf { length0, length1, length2, length3, length4, length5, length6, length7, length8, param0, param1, param2, param3, param4, param5, param6, param7, param8 } ->
            if s <= length4 then
                if s <= length2 then
                    if s <= length1 then
                        -- 0 to 1
                        let
                            lengthFraction =
                                (s - length0) / (length1 - length0)
                        in
                        Scalar.interpolateFrom param0 param1 lengthFraction
                    else
                        -- 1 to 2
                        let
                            lengthFraction =
                                (s - length1) / (length2 - length1)
                        in
                        Scalar.interpolateFrom param1 param2 lengthFraction
                else if s <= length3 then
                    -- 2 to 3
                    let
                        lengthFraction =
                            (s - length2) / (length3 - length2)
                    in
                    Scalar.interpolateFrom param2 param3 lengthFraction
                else
                    -- 3 to 4
                    let
                        lengthFraction =
                            (s - length3) / (length4 - length3)
                    in
                    Scalar.interpolateFrom param3 param4 lengthFraction
            else if s <= length6 then
                if s <= length5 then
                    -- 4 to 5
                    let
                        lengthFraction =
                            (s - length4) / (length5 - length4)
                    in
                    Scalar.interpolateFrom param4 param5 lengthFraction
                else
                    -- 5 to 6
                    let
                        lengthFraction =
                            (s - length5) / (length6 - length5)
                    in
                    Scalar.interpolateFrom param5 param6 lengthFraction
            else if s <= length7 then
                -- 6 to 7
                let
                    lengthFraction =
                        (s - length6) / (length7 - length6)
                in
                Scalar.interpolateFrom param6 param7 lengthFraction
            else
                -- 7 to 8
                let
                    lengthFraction =
                        (s - length7) / (length8 - length7)
                in
                Scalar.interpolateFrom param7 param8 lengthFraction

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

        Leaf { length8 } ->
            length8


{-| Find the total arc length of some curve given its arc length
parameterization;

    ArcLength.fromParameterization parameterization

is similar to

    ArcLength.fromParameterValue parameterization 1

but is more efficient and returns a plain `Float` instead of a `Maybe Float`.

-}
fromParameterization : Parameterization -> Float
fromParameterization (Parameterization tree) =
    lengthAtEnd tree


{-| Convert a parameter value to the corresponding arc length. If the given
parameter value is less than zero or greater than one, returns `Nothing`.
-}
fromParameterValue : Parameterization -> Float -> Maybe Float
fromParameterValue (Parameterization tree) t =
    if t >= 0 && t <= 1 then
        Just (unsafeFromParameterValue tree t)
    else
        Nothing


unsafeFromParameterValue : SegmentTree -> Float -> Float
unsafeFromParameterValue tree t =
    case tree of
        Leaf { length0, length1, length2, length3, length4, length5, length6, length7, length8, param0, param1, param2, param3, param4, param5, param6, param7, param8 } ->
            if t <= param4 then
                if t <= param2 then
                    if t <= param1 then
                        -- 0 to 1
                        let
                            paramFraction =
                                (t - param0) / (param1 - param0)
                        in
                        Scalar.interpolateFrom length0 length1 paramFraction
                    else
                        -- 1 to 2
                        let
                            paramFraction =
                                (t - param1) / (param2 - param1)
                        in
                        Scalar.interpolateFrom length1 length2 paramFraction
                else if t <= param3 then
                    -- 2 to 3
                    let
                        paramFraction =
                            (t - param2) / (param3 - param2)
                    in
                    Scalar.interpolateFrom length2 length3 paramFraction
                else
                    -- 3 to 4
                    let
                        paramFraction =
                            (t - param3) / (param4 - param3)
                    in
                    Scalar.interpolateFrom length3 length4 paramFraction
            else if t <= param6 then
                if t <= param5 then
                    -- 4 to 5
                    let
                        paramFraction =
                            (t - param4) / (param5 - param4)
                    in
                    Scalar.interpolateFrom length4 length5 paramFraction
                else
                    -- 5 to 6
                    let
                        paramFraction =
                            (t - param5) / (param6 - param5)
                    in
                    Scalar.interpolateFrom length5 length6 paramFraction
            else if t <= param7 then
                -- 6 to 7
                let
                    paramFraction =
                        (t - param6) / (param7 - param6)
                in
                Scalar.interpolateFrom length6 length7 paramFraction
            else
                -- 7 to 8
                let
                    paramFraction =
                        (t - param7) / (param8 - param7)
                in
                Scalar.interpolateFrom length7 length8 paramFraction

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
