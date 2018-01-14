module Polygon2d
    exposing
        ( jsonRoundTrips
        )

import Generic
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Fuzz as Fuzz
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.polygon2d
        Encode.polygon2d
        Decode.polygon2d
