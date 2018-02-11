module Tests.Interval
    exposing
        ( cosWorksProperly
        , jsonRoundTrips
        , sinWorksProperly
        )

import Fuzz
import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Interval exposing (Interval)
import Scalar
import Test exposing (Test)
import Tests.Generic as Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.interval
        Encode.interval
        Decode.interval


sinWorksProperly : Test
sinWorksProperly =
    Test.fuzz2
        Fuzz.interval
        (Fuzz.floatRange 0 1)
        "sin works as expected"
        (\interval t ->
            let
                valueInInterval =
                    Interval.interpolate interval t
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
                    Interval.interpolate interval t
            in
            cos valueInInterval |> Expect.valueIn (Interval.cos interval)
        )
