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


module Direction2d exposing (suite)

import Test exposing (Test)
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.direction2d
        Encode.direction2d
        Decode.direction2d


angleFromAndEqualWithinAreConsistent : Test
angleFromAndEqualWithinAreConsistent =
    Test.fuzz2 Fuzz.direction2d
        Fuzz.direction2d
        "angleFrom and equalWithin are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    abs (Direction2d.angleFrom firstDirection secondDirection)
            in
                Expect.true "Two directions should be equal to within the angle between them"
                    (Direction2d.equalWithin (angle + 1.0e-12)
                        firstDirection
                        secondDirection
                    )
        )


angleFromAndRotateByAreConsistent : Test
angleFromAndRotateByAreConsistent =
    Test.fuzz2 Fuzz.direction2d
        Fuzz.direction2d
        "angleFrom and rotateBy are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    Direction2d.angleFrom firstDirection secondDirection
            in
                firstDirection
                    |> Direction2d.rotateBy angle
                    |> Expect.direction2d secondDirection
        )


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Direction2d"
        [ jsonRoundTrips
        , angleFromAndEqualWithinAreConsistent
        , angleFromAndRotateByAreConsistent
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
