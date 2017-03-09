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


module Triangle2d exposing (suite)

import Test exposing (Test)
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.triangle2d Encode.triangle2d Decode.triangle2d


triangleContainsOwnCentroid : Test
triangleContainsOwnCentroid =
    Test.fuzz Fuzz.triangle2d
        "non-zero area triangle contains its own centroid"
        (\triangle ->
            let
                centroid =
                    Triangle2d.centroid triangle

                area =
                    Triangle2d.area triangle
            in
                Expect.true "non-zero area triangle did not contain its own centroid"
                    (area == 0.0 || Triangle2d.contains centroid triangle)
        )


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Triangle2d"
        [ jsonRoundTrips
        , triangleContainsOwnCentroid
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
