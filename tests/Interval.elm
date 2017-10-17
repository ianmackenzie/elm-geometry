module Interval
    exposing
        ( cosWorksProperly
        , sinWorksProperly
        )

import Fuzz
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Interval as Interval exposing (Interval)
import Test exposing (Test)


sinWorksProperly : Test
sinWorksProperly =
    Test.fuzz2
        Fuzz.interval
        (Fuzz.floatRange 0 1)
        "sin works as expected"
        (\interval t ->
            let
                valueInInterval =
                    Interval.interpolate t interval
            in
            sin valueInInterval |> Expect.valueIn (Interval.sin interval)
        )


cosWorksProperly : Test
cosWorksProperly =
    Test.fuzz2
        Fuzz.interval
        (Fuzz.floatRange 0 1)
        "cos works as expected"
        (\interval t ->
            let
                valueInInterval =
                    Interval.interpolate t interval
            in
            cos valueInInterval |> Expect.valueIn (Interval.cos interval)
        )
