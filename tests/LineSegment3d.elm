module LineSegment3d
    exposing
        ( jsonRoundTrips
        )

import Generic
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.LineSegment3d as LineSegment3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.lineSegment3d
        Encode.lineSegment3d
        Decode.lineSegment3d
