module Tests.Vector3d
    exposing
        ( jsonRoundTrips
        )

import Expect
import Frame3d
import Fuzz
import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point3d
import Test exposing (Test)
import Tests.Generic as Generic
import Vector3d


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.vector3d Encode.vector3d Decode.vector3d
