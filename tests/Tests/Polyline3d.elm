module Tests.Polyline3d
    exposing
        ( jsonRoundTrips
        )

import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Polyline3d
import Test exposing (Test)
import Tests.Generic as Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.polyline3d
        Encode.polyline3d
        Decode.polyline3d
