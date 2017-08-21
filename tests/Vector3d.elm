module Vector3d
    exposing
        ( jsonRoundTrips
        , orthonormalizeProducesValidFrameBasis
        , orthonormalizingCoplanarVectorsReturnsNothing
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


orthonormalizeProducesValidFrameBasis : Test
orthonormalizeProducesValidFrameBasis =
    Test.fuzz (Fuzz.tuple3 ( Fuzz.vector3d, Fuzz.vector3d, Fuzz.vector3d ))
        "orthonormalize produces a valid frame basis"
        (\vectors ->
            case Vector3d.orthonormalize vectors of
                Just ( xDirection, yDirection, zDirection ) ->
                    Expect.validFrame3d
                        (Frame3d.with
                            { originPoint = Point3d.origin
                            , xDirection = xDirection
                            , yDirection = yDirection
                            , zDirection = zDirection
                            }
                        )

                Nothing ->
                    let
                        ( v1, v2, v3 ) =
                            vectors

                        tripleProduct =
                            Vector3d.crossProduct v1 v2
                                |> Vector3d.dotProduct v3
                    in
                    Expect.approximately 0.0 tripleProduct
        )


orthonormalizingCoplanarVectorsReturnsNothing : Test
orthonormalizingCoplanarVectorsReturnsNothing =
    Test.test "orthonormalizing coplanar vectors returns Nothing"
        (\() ->
            let
                vectors =
                    ( Vector3d.withComponents ( 1, 0, 0 )
                    , Vector3d.withComponents ( 2, 3, 0 )
                    , Vector3d.withComponents ( -1, 2, 0 )
                    )
            in
            Expect.equal Nothing (Vector3d.orthonormalize vectors)
        )
