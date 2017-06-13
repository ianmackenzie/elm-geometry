module Arc3d
    exposing
        ( jsonRoundTrips
        )

import Expect
import Generic
import OpenSolid.Arc3d as Arc3d
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.arc3d
        Encode.arc3d
        Decode.arc3d
