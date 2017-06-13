module Polyline3d
    exposing
        ( jsonRoundTrips
        )

import Generic
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Polyline3d as Polyline3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.polyline3d
        Encode.polyline3d
        Decode.polyline3d
