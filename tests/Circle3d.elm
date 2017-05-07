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


module Circle3d exposing (suite)

import Test exposing (Test)
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.Circle3d as Circle3d
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.circle3d
        Encode.circle3d
        Decode.circle3d


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    Test.fuzz Fuzz.circle3d
        "A circle's bounding box contains its center point"
        (\circle ->
            let
                boundingBox =
                    Circle3d.boundingBox circle

                centerPoint =
                    Circle3d.centerPoint circle
            in
                Expect.true
                    "Circle bounding box does not contain the center point"
                    (BoundingBox3d.contains centerPoint boundingBox)
        )


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Circle3d"
        [ jsonRoundTrips
        , boundingBoxContainsCenter
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
