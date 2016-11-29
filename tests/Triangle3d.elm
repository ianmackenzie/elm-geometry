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


module Triangle3d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as HtmlRunner
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.triangle3d Encode.triangle3d Decode.triangle3d


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Triangle3d"
        [ jsonRoundTrips
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
