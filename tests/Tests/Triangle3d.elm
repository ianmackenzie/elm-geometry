module Tests.Triangle3d
    exposing
        ( jsonRoundTrips
        )

import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Tests.Generic as Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.triangle3d Encode.triangle3d Decode.triangle3d
