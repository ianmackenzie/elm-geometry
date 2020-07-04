module Curve exposing (arcApproximationSegments, numApproximationSegments)

import Angle exposing (Angle)
import Quantity exposing (Quantity)


arcApproximationSegments : { maxError : Quantity Float units, radius : Quantity Float units, sweptAngle : Angle } -> Int
arcApproximationSegments { maxError, radius, sweptAngle } =
    if sweptAngle == Quantity.zero then
        1

    else if maxError |> Quantity.lessThanOrEqualTo Quantity.zero then
        0

    else if maxError |> Quantity.greaterThanOrEqualTo (Quantity.twice radius) then
        1

    else
        let
            maxSegmentAngle =
                Quantity.twice (Angle.acos (1 - Quantity.ratio maxError radius))
        in
        ceiling (Quantity.ratio (Quantity.abs sweptAngle) maxSegmentAngle)


numApproximationSegments : { maxError : Quantity Float units, maxSecondDerivativeMagnitude : Quantity Float units } -> Int
numApproximationSegments { maxError, maxSecondDerivativeMagnitude } =
    if maxError |> Quantity.greaterThan Quantity.zero then
        let
            computedNumSegments =
                sqrt (Quantity.ratio maxSecondDerivativeMagnitude (Quantity.multiplyBy 8 maxError))
        in
        max (ceiling computedNumSegments) 1

    else
        0
