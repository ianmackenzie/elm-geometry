module Plane3d
    exposing
        ( jsonRoundTrips
        )

import Generic
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Plane3d as Plane3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.plane3d Encode.plane3d Decode.plane3d
