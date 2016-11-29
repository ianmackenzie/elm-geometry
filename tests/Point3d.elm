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


module Point3d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as HtmlRunner
import OpenSolid.Point3d as Point3d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


rotationAboutAxisPreservesDistanceAlong : Test
rotationAboutAxisPreservesDistanceAlong =
    let
        description =
            "Rotation about axis preserves distance along that axis"

        expectation point axis angle =
            let
                distance =
                    Point3d.distanceAlong axis point

                rotatedPoint =
                    Point3d.rotateAround axis angle point

                rotatedDistance =
                    Point3d.distanceAlong axis rotatedPoint
            in
                Expect.approximately distance rotatedDistance
    in
        Test.fuzz3 Fuzz.point3d Fuzz.axis3d Fuzz.scalar description expectation


rotationAboutAxisPreservesRadialDistance : Test
rotationAboutAxisPreservesRadialDistance =
    let
        description =
            "Rotation about axis preserves radial distance from that axis"

        expectation point axis angle =
            let
                radialDistance =
                    Point3d.radialDistanceFrom axis point

                rotatedPoint =
                    Point3d.rotateAround axis angle point

                rotatedDistance =
                    Point3d.radialDistanceFrom axis rotatedPoint
            in
                Expect.approximately radialDistance rotatedDistance
    in
        Test.fuzz3 Fuzz.point3d Fuzz.axis3d Fuzz.scalar description expectation


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.point3d Encode.point3d Decode.point3d


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Point3d"
        [ rotationAboutAxisPreservesDistanceAlong
        , rotationAboutAxisPreservesRadialDistance
        , jsonRoundTrips
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
