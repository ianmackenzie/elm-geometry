module Tests.LineSegment3d
    exposing
        ( jsonRoundTrips
        )

import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import LineSegment3d
import Test exposing (Test)
import Tests.Generic as Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.lineSegment3d
        Encode.lineSegment3d
        Decode.lineSegment3d
