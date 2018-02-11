module Tests.Polyline2d
    exposing
        ( jsonRoundTrips
        )

import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Polyline2d
import Test exposing (Test)
import Tests.Generic as Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.polyline2d
        Encode.polyline2d
        Decode.polyline2d
