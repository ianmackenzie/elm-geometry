module Vector3d
    exposing
        ( jsonRoundTrips
        )

import Expect
import Fuzz
import Generic
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.vector3d Encode.vector3d Decode.vector3d
