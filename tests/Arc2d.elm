module Arc2d
    exposing
        ( jsonRoundTrips
        )

import Expect
import Generic
import OpenSolid.Arc2d as Arc2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.arc2d
        Encode.arc2d
        Decode.arc2d
