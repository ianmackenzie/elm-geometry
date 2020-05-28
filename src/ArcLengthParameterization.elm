--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module ArcLengthParameterization exposing
    ( ArcLengthParameterization
    , build
    , totalArcLength, arcLengthToParameterValue, parameterValueToArcLength
    )

{-| _You will likely never need to use this module directly._ In the vast
majority of cases the individual curve modules such as `QuadraticSpline2d`
should contain all the functionality you need to construct an arc length
parameterization and use it to do things like evaluate a curve at evenly-spaced
points. This module is primarily for use internally by those curve modules, but
may be useful if you want to do some fancy mapping between arc length and curve
parameter values.

@docs ArcLengthParameterization


# Constructing

@docs build


# Evaluating

@docs totalArcLength, arcLengthToParameterValue, parameterValueToArcLength

-}

import Float.Extra as Float
import Quantity exposing (Quantity(..), zero)
import Quantity.Extra as Quantity


{-| Contains a mapping from curve parameter value to arc length, and vice versa.
Parameter values are assumed to range from 0 to 1.
-}
type ArcLengthParameterization units
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

-}
build :
    { derivativeMagnitude : Float -> Quantity Float units
    , maxSecondDerivativeMagnitude : Quantity Float units
    , maxError : Quantity Float units
    }
    -> ArcLengthParameterization units
build { maxError, derivativeMagnitude, maxSecondDerivativeMagnitude } =
    let
        height =
            if maxError |> Quantity.lessThanOrEqualTo zero then
                0

            else
                let
                    numSegments =
                        Quantity.ratio maxSecondDerivativeMagnitude
                            (Quantity.multiplyBy 8 maxError)

                    numLeaves =
                        numSegments / toFloat segmentsPerLeaf
                in
                max 0 (ceiling (logBase 2 numLeaves))
    in
    ArcLengthParameterization (buildTree derivativeMagnitude 0 0 1 height)


buildTree : (Float -> Quantity Float units) -> Float -> Float -> Float -> Int -> SegmentTree
buildTree derivativeMagnitude lengthAtStart_ paramAtStart_ paramAtEnd height =
    let
        paramDelta =
            paramAtEnd - paramAtStart_
    in
    if height == 0 then
        let
            param0 =
                paramAtStart_

            param1 =
                paramAtStart_ + 0.125 * paramDelta

            param2 =
                paramAtStart_ + 0.25 * paramDelta

            param3 =
                paramAtStart_ + 0.375 * paramDelta

            param4 =
                paramAtStart_ + 0.5 * paramDelta

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

            (Quantity derivativeMagnitude0) =
                derivativeMagnitude (param0 + offset)

            (Quantity derivativeMagnitude1) =
                derivativeMagnitude (param1 + offset)

            (Quantity derivativeMagnitude2) =
                derivativeMagnitude (param2 + offset)

            (Quantity derivativeMagnitude3) =
                derivativeMagnitude (param3 + offset)

            (Quantity derivativeMagnitude4) =
                derivativeMagnitude (param4 + offset)

            (Quantity derivativeMagnitude5) =
                derivativeMagnitude (param5 + offset)

            (Quantity derivativeMagnitude6) =
                derivativeMagnitude (param6 + offset)

            (Quantity derivativeMagnitude7) =
                derivativeMagnitude (param7 + offset)

            length0 =
                lengthAtStart_

            length1 =
                length0 + paramStep * derivativeMagnitude0

            length2 =
                length1 + paramStep * derivativeMagnitude1

            length3 =
                length2 + paramStep * derivativeMagnitude2

            length4 =
                length3 + paramStep * derivativeMagnitude3

            length5 =
                length4 + paramStep * derivativeMagnitude4

            length6 =
                length5 + paramStep * derivativeMagnitude5

            length7 =
                length6 + paramStep * derivativeMagnitude6

            length8 =
                length7 + paramStep * derivativeMagnitude7
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
                paramAtStart_ + 0.5 * paramDelta

            leftBranch =
                buildTree derivativeMagnitude
                    lengthAtStart_
                    paramAtStart_
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
            { lengthAtStart = lengthAtStart_
            , lengthAtEnd = lengthAtEnd rightBranch
            , paramAtStart = paramAtStart_
            , leftBranch = leftBranch
            , rightBranch = rightBranch
            }


{-| Convert an arc length to the corresponding parameter value. The given arc
length will be clamped to the range [0, length].
-}
arcLengthToParameterValue : Quantity Float units -> ArcLengthParameterization units -> Float
arcLengthToParameterValue (Quantity s) (ArcLengthParameterization tree) =
    unsafeToParameterValue tree (clamp 0 (lengthAtEnd tree) s)


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
                        Float.interpolateFrom param0 param1 lengthFraction

                    else
                        -- 1 to 2
                        let
                            lengthFraction =
                                (s - length1) / (length2 - length1)
                        in
                        Float.interpolateFrom param1 param2 lengthFraction

                else if s <= length3 then
                    -- 2 to 3
                    let
                        lengthFraction =
                            (s - length2) / (length3 - length2)
                    in
                    Float.interpolateFrom param2 param3 lengthFraction

                else
                    -- 3 to 4
                    let
                        lengthFraction =
                            (s - length3) / (length4 - length3)
                    in
                    Float.interpolateFrom param3 param4 lengthFraction

            else if s <= length6 then
                if s <= length5 then
                    -- 4 to 5
                    let
                        lengthFraction =
                            (s - length4) / (length5 - length4)
                    in
                    Float.interpolateFrom param4 param5 lengthFraction

                else
                    -- 5 to 6
                    let
                        lengthFraction =
                            (s - length5) / (length6 - length5)
                    in
                    Float.interpolateFrom param5 param6 lengthFraction

            else if s <= length7 then
                -- 6 to 7
                let
                    lengthFraction =
                        (s - length6) / (length7 - length6)
                in
                Float.interpolateFrom param6 param7 lengthFraction

            else
                -- 7 to 8
                let
                    lengthFraction =
                        (s - length7) / (length8 - length7)
                in
                Float.interpolateFrom param7 param8 lengthFraction

        Node { leftBranch, rightBranch } ->
            if s < lengthAtStart rightBranch then
                unsafeToParameterValue leftBranch s

            else
                unsafeToParameterValue rightBranch s


lengthAtStart : SegmentTree -> Float
lengthAtStart tree =
    case tree of
        Node node ->
            node.lengthAtStart

        Leaf leaf ->
            leaf.length0


lengthAtEnd : SegmentTree -> Float
lengthAtEnd tree =
    case tree of
        Node node ->
            node.lengthAtEnd

        Leaf leaf ->
            leaf.length8


{-| Find the total arc length of some curve given its arc length
parameterization;

    ArcLengthParameterization.totalArcLength
        parameterization

is equivalent to

    ArcLengthParameterization.parameterValueToArcLength 1
        parameterization

but is more efficient. The arc length will be accurate to within the maximum
error specified when building the parameterization.

-}
totalArcLength : ArcLengthParameterization units -> Quantity Float units
totalArcLength (ArcLengthParameterization tree) =
    Quantity (lengthAtEnd tree)


{-| Convert a parameter value to the corresponding arc length. The parameter
value will be clamped to the range [0, 1].
-}
parameterValueToArcLength : Float -> ArcLengthParameterization units -> Quantity Float units
parameterValueToArcLength parameterValue (ArcLengthParameterization tree) =
    Quantity (unsafeToArcLength tree (clamp 0 1 parameterValue))


unsafeToArcLength : SegmentTree -> Float -> Float
unsafeToArcLength tree t =
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
                        Float.interpolateFrom length0 length1 paramFraction

                    else
                        -- 1 to 2
                        let
                            paramFraction =
                                (t - param1) / (param2 - param1)
                        in
                        Float.interpolateFrom length1 length2 paramFraction

                else if t <= param3 then
                    -- 2 to 3
                    let
                        paramFraction =
                            (t - param2) / (param3 - param2)
                    in
                    Float.interpolateFrom length2 length3 paramFraction

                else
                    -- 3 to 4
                    let
                        paramFraction =
                            (t - param3) / (param4 - param3)
                    in
                    Float.interpolateFrom length3 length4 paramFraction

            else if t <= param6 then
                if t <= param5 then
                    -- 4 to 5
                    let
                        paramFraction =
                            (t - param4) / (param5 - param4)
                    in
                    Float.interpolateFrom length4 length5 paramFraction

                else
                    -- 5 to 6
                    let
                        paramFraction =
                            (t - param5) / (param6 - param5)
                    in
                    Float.interpolateFrom length5 length6 paramFraction

            else if t <= param7 then
                -- 6 to 7
                let
                    paramFraction =
                        (t - param6) / (param7 - param6)
                in
                Float.interpolateFrom length6 length7 paramFraction

            else
                -- 7 to 8
                let
                    paramFraction =
                        (t - param7) / (param8 - param7)
                in
                Float.interpolateFrom length7 length8 paramFraction

        Node { leftBranch, rightBranch } ->
            if t < paramAtStart rightBranch then
                unsafeToArcLength leftBranch t

            else
                unsafeToArcLength rightBranch t


paramAtStart : SegmentTree -> Float
paramAtStart tree =
    case tree of
        Node node ->
            node.paramAtStart

        Leaf leaf ->
            leaf.param0
