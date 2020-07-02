module Curve exposing (numSegments)

import Quantity exposing (Quantity)


numSegments : { maxError : Quantity Float units } -> Quantity Float units -> Int
numSegments { maxError } maxSecondDerivativeMagnitude =
    if maxError |> Quantity.greaterThan Quantity.zero then
        let
            computedNumSegments =
                sqrt (Quantity.ratio maxSecondDerivativeMagnitude (Quantity.multiplyBy 8 maxError))
        in
        max (ceiling computedNumSegments) 1

    else
        0
