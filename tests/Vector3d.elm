--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Vector3d exposing (suite)

import Test exposing (Test)
import Fuzz
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


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
                        (Frame3d
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
                    ( Vector3d ( 1, 0, 0 )
                    , Vector3d ( 2, 3, 0 )
                    , Vector3d ( -1, 2, 0 )
                    )
            in
                Expect.equal Nothing (Vector3d.orthonormalize vectors)
        )


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Vector3d"
        [ jsonRoundTrips
        , orthonormalizeProducesValidFrameBasis
        , orthonormalizingCoplanarVectorsReturnsNothing
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
