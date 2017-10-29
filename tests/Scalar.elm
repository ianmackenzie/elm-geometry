module Scalar exposing (roundTo)

import Expect
import Fuzz exposing (float, intRange)
import OpenSolid.Scalar as Scalar
import Test exposing (Test, describe, fuzz, fuzz2)


roundTo : Test
roundTo =
    describe "roundTo"
        [ fuzz float
            "(roundTo 0 x) is equivalent to (round x |> toFloat)"
            (\num ->
                Scalar.roundTo 0 num
                    |> Expect.equal (round num |> toFloat)
            )
        , fuzz2 (intRange 0 10)
            float
            "(roundTo x y) has a maximum of x decimal digits"
            (\to num ->
                Scalar.roundTo to num
                    |> toString
                    |> String.length
                    |> Expect.atMost ((floor num |> toString |> String.length) + 1 + to)
            )
        ]
