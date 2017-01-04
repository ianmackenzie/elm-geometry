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


module Direction3d exposing (suite)

import Test exposing (Test)
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.direction3d
        Encode.direction3d
        Decode.direction3d


angleFromAndEqualWithinAreConsistent : Test
angleFromAndEqualWithinAreConsistent =
    Test.fuzz2 Fuzz.direction3d
        Fuzz.direction3d
        "angleFrom and equalWithin are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    Direction3d.angleFrom firstDirection secondDirection
            in
                Expect.true "Two directions should be equal to within the angle between them"
                    (Direction3d.equalWithin (angle + 1.0e-12)
                        firstDirection
                        secondDirection
                    )
        )


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Direction3d"
        [ jsonRoundTrips
        , angleFromAndEqualWithinAreConsistent
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
