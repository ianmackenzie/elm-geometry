module Tests.Vector3d
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
    Generic.jsonRoundTrips Fuzz.vector3d Encode.vector3d Decode.vector3d
