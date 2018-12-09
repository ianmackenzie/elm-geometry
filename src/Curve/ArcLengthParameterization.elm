--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Curve.ArcLengthParameterization exposing
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

import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Float.Extra as Float
import Quantity exposing (Quantity)
import Quantity.Extra as Quantity


{-| Contains a mapping from curve parameter value to arc length, and vice versa.
-}
type ArcLengthParameterization units
    = ArcLengthParameterization (SegmentTree units)


type SegmentTree units
    = Node
        { lengthAtStart : Quantity Float units
        , lengthAtEnd : Quantity Float units
        , paramAtStart : Float
        , leftBranch : SegmentTree units
        , rightBranch : SegmentTree units
        }
    | Leaf
        { length0 : Quantity Float units
        , length1 : Quantity Float units
        , length2 : Quantity Float units
        , length3 : Quantity Float units
        , length4 : Quantity Float units
        , length5 : Quantity Float units
        , length6 : Quantity Float units
        , length7 : Quantity Float units
        , length8 : Quantity Float units
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
    { maxError : Quantity Float units
    , derivativeMagnitude : ParameterValue -> Quantity Float units
    , maxSecondDerivativeMagnitude : Quantity Float units
    }
    -> ArcLengthParameterization units
build { maxError, derivativeMagnitude, maxSecondDerivativeMagnitude } =
    let
        height =
            if maxError |> Quantity.lessThanOrEqualTo Quantity.zero then
                0

            else
                let
                    numSegments =
                        Quantity.ratio maxSecondDerivativeMagnitude
                            (Quantity.scaleBy 8 maxError)

                    numLeaves =
                        numSegments / toFloat segmentsPerLeaf
                in
                max 0 (ceiling (logBase 2 numLeaves))
    in
    ArcLengthParameterization (buildTree derivativeMagnitude Quantity.zero 0 1 height)


buildTree : (ParameterValue -> Quantity Float units) -> Quantity Float units -> Float -> Float -> Int -> SegmentTree units
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

            derivativeMagnitude0 =
                derivativeMagnitude (ParameterValue.unsafe (param0 + offset))

            derivativeMagnitude1 =
                derivativeMagnitude (ParameterValue.unsafe (param1 + offset))

            derivativeMagnitude2 =
                derivativeMagnitude (ParameterValue.unsafe (param2 + offset))

            derivativeMagnitude3 =
                derivativeMagnitude (ParameterValue.unsafe (param3 + offset))

            derivativeMagnitude4 =
                derivativeMagnitude (ParameterValue.unsafe (param4 + offset))

            derivativeMagnitude5 =
                derivativeMagnitude (ParameterValue.unsafe (param5 + offset))

            derivativeMagnitude6 =
                derivativeMagnitude (ParameterValue.unsafe (param6 + offset))

            derivativeMagnitude7 =
                derivativeMagnitude (ParameterValue.unsafe (param7 + offset))

            length0 =
                lengthAtStart_

            length1 =
                length0
                    |> Quantity.plus
                        (Quantity.scaleBy paramStep derivativeMagnitude0)

            length2 =
                length1
                    |> Quantity.plus
                        (Quantity.scaleBy paramStep derivativeMagnitude1)

            length3 =
                length2
                    |> Quantity.plus
                        (Quantity.scaleBy paramStep derivativeMagnitude2)

            length4 =
                length3
                    |> Quantity.plus
                        (Quantity.scaleBy paramStep derivativeMagnitude3)

            length5 =
                length4
                    |> Quantity.plus
                        (Quantity.scaleBy paramStep derivativeMagnitude4)

            length6 =
                length5
                    |> Quantity.plus
                        (Quantity.scaleBy paramStep derivativeMagnitude5)

            length7 =
                length6
                    |> Quantity.plus
                        (Quantity.scaleBy paramStep derivativeMagnitude6)

            length8 =
                length7
                    |> Quantity.plus
                        (Quantity.scaleBy paramStep derivativeMagnitude7)
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


{-| Convert an arc length to the corresponding parameter value. If the given
arc length is less than zero or greater than the total arc length of the curve
(as reported by `totalArcLength`), returns `Nothing`.
-}
arcLengthToParameterValue : Quantity Float units -> ArcLengthParameterization units -> Maybe ParameterValue
arcLengthToParameterValue s (ArcLengthParameterization tree) =
    if s == Quantity.zero then
        Just ParameterValue.zero

    else if
        (s |> Quantity.greaterThan Quantity.zero)
            && (s |> Quantity.lessThanOrEqualTo (lengthAtEnd tree))
    then
        Just (ParameterValue.clamped (unsafeToParameterValue tree s))

    else
        Nothing


unsafeToParameterValue : SegmentTree units -> Quantity Float units -> Float
unsafeToParameterValue tree s =
    case tree of
        Leaf { length0, length1, length2, length3, length4, length5, length6, length7, length8, param0, param1, param2, param3, param4, param5, param6, param7, param8 } ->
            if s |> Quantity.lessThanOrEqualTo length4 then
                if s |> Quantity.lessThanOrEqualTo length2 then
                    if s |> Quantity.lessThanOrEqualTo length1 then
                        -- 0 to 1
                        let
                            lengthFraction =
                                Quantity.ratio (s |> Quantity.minus length0)
                                    (length1 |> Quantity.minus length0)
                        in
                        Float.interpolateFrom param0 param1 lengthFraction

                    else
                        -- 1 to 2
                        let
                            lengthFraction =
                                Quantity.ratio (s |> Quantity.minus length1)
                                    (length2 |> Quantity.minus length1)
                        in
                        Float.interpolateFrom param1 param2 lengthFraction

                else if s |> Quantity.lessThanOrEqualTo length3 then
                    -- 2 to 3
                    let
                        lengthFraction =
                            Quantity.ratio (s |> Quantity.minus length2)
                                (length3 |> Quantity.minus length2)
                    in
                    Float.interpolateFrom param2 param3 lengthFraction

                else
                    -- 3 to 4
                    let
                        lengthFraction =
                            Quantity.ratio (s |> Quantity.minus length3)
                                (length4 |> Quantity.minus length3)
                    in
                    Float.interpolateFrom param3 param4 lengthFraction

            else if s |> Quantity.lessThanOrEqualTo length6 then
                if s |> Quantity.lessThanOrEqualTo length5 then
                    -- 4 to 5
                    let
                        lengthFraction =
                            Quantity.ratio (s |> Quantity.minus length4)
                                (length5 |> Quantity.minus length4)
                    in
                    Float.interpolateFrom param4 param5 lengthFraction

                else
                    -- 5 to 6
                    let
                        lengthFraction =
                            Quantity.ratio (s |> Quantity.minus length5)
                                (length6 |> Quantity.minus length5)
                    in
                    Float.interpolateFrom param5 param6 lengthFraction

            else if s |> Quantity.lessThanOrEqualTo length7 then
                -- 6 to 7
                let
                    lengthFraction =
                        Quantity.ratio (s |> Quantity.minus length6)
                            (length7 |> Quantity.minus length6)
                in
                Float.interpolateFrom param6 param7 lengthFraction

            else
                -- 7 to 8
                let
                    lengthFraction =
                        Quantity.ratio (s |> Quantity.minus length7)
                            (length8 |> Quantity.minus length7)
                in
                Float.interpolateFrom param7 param8 lengthFraction

        Node { leftBranch, rightBranch } ->
            if s |> Quantity.lessThan (lengthAtStart rightBranch) then
                unsafeToParameterValue leftBranch s

            else
                unsafeToParameterValue rightBranch s


lengthAtStart : SegmentTree units -> Quantity Float units
lengthAtStart tree =
    case tree of
        Node node ->
            node.lengthAtStart

        Leaf leaf ->
            leaf.length0


lengthAtEnd : SegmentTree units -> Quantity Float units
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

    ArcLengthParameterization.parameterValueToArcLength
        ParameterValue.one
        parameterization

but is more efficient.

-}
totalArcLength : ArcLengthParameterization units -> Quantity Float units
totalArcLength (ArcLengthParameterization tree) =
    lengthAtEnd tree


{-| Convert a parameter value to the corresponding arc length.
-}
parameterValueToArcLength : ParameterValue -> ArcLengthParameterization units -> Quantity Float units
parameterValueToArcLength parameterValue (ArcLengthParameterization tree) =
    if parameterValue == ParameterValue.zero then
        Quantity.zero

    else
        unsafeToArcLength tree (ParameterValue.value parameterValue)


unsafeToArcLength : SegmentTree units -> Float -> Quantity Float units
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
                        Quantity.interpolateFrom length0 length1 paramFraction

                    else
                        -- 1 to 2
                        let
                            paramFraction =
                                (t - param1) / (param2 - param1)
                        in
                        Quantity.interpolateFrom length1 length2 paramFraction

                else if t <= param3 then
                    -- 2 to 3
                    let
                        paramFraction =
                            (t - param2) / (param3 - param2)
                    in
                    Quantity.interpolateFrom length2 length3 paramFraction

                else
                    -- 3 to 4
                    let
                        paramFraction =
                            (t - param3) / (param4 - param3)
                    in
                    Quantity.interpolateFrom length3 length4 paramFraction

            else if t <= param6 then
                if t <= param5 then
                    -- 4 to 5
                    let
                        paramFraction =
                            (t - param4) / (param5 - param4)
                    in
                    Quantity.interpolateFrom length4 length5 paramFraction

                else
                    -- 5 to 6
                    let
                        paramFraction =
                            (t - param5) / (param6 - param5)
                    in
                    Quantity.interpolateFrom length5 length6 paramFraction

            else if t <= param7 then
                -- 6 to 7
                let
                    paramFraction =
                        (t - param6) / (param7 - param6)
                in
                Quantity.interpolateFrom length6 length7 paramFraction

            else
                -- 7 to 8
                let
                    paramFraction =
                        (t - param7) / (param8 - param7)
                in
                Quantity.interpolateFrom length7 length8 paramFraction

        Node { leftBranch, rightBranch } ->
            if t < paramAtStart rightBranch then
                unsafeToArcLength leftBranch t

            else
                unsafeToArcLength rightBranch t


paramAtStart : SegmentTree units -> Float
paramAtStart tree =
    case tree of
        Node node ->
            node.paramAtStart

        Leaf leaf ->
            leaf.param0
