module QuadraticSpline3d
    exposing
        ( jsonRoundTrips
        )

import Expect
import Generic
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.quadraticSpline3d
        Encode.quadraticSpline3d
        Decode.quadraticSpline3d
