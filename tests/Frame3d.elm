module Frame3d
    exposing
        ( frameDirectionsAreOrthonormal
        , jsonRoundTrips
        )

import Generic
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Vector3d as Vector3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.frame3d Encode.frame3d Decode.frame3d


frameDirectionsAreOrthonormal : Test
frameDirectionsAreOrthonormal =
    Test.fuzz Fuzz.frame3d
        "Frame3d basis directions are orthonormal"
        (\frame ->
            let
                xDirectionVector =
                    Direction3d.toVector (Frame3d.xDirection frame)

                yDirectionVector =
                    Direction3d.toVector (Frame3d.yDirection frame)

                zDirectionVector =
                    Direction3d.toVector (Frame3d.zDirection frame)

                tripleProduct =
                    Vector3d.crossProduct xDirectionVector yDirectionVector
                        |> Vector3d.dotProduct zDirectionVector

                expectedTripleProduct =
                    if Frame3d.isRightHanded frame then
                        1
                    else
                        -1
            in
            Expect.approximately expectedTripleProduct tripleProduct
        )
