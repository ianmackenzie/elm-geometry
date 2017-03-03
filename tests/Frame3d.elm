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


module Frame3d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as HtmlRunner
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


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


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Frame3d"
        [ jsonRoundTrips
        , frameDirectionsAreOrthonormal
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
