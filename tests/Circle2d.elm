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


module Circle2d exposing (suite)

import Test exposing (Test)
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.Circle2d as Circle2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.circle2d
        Encode.circle2d
        Decode.circle2d


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    Test.fuzz Fuzz.circle2d
        "A circle's bounding box contains its center point"
        (\circle ->
            let
                boundingBox =
                    Circle2d.boundingBox circle

                centerPoint =
                    Circle2d.centerPoint circle
            in
                Expect.true
                    "Circle bounding box does not contain the center point"
                    (BoundingBox2d.contains centerPoint boundingBox)
        )


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Circle2d"
        [ jsonRoundTrips
        , boundingBoxContainsCenter
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
