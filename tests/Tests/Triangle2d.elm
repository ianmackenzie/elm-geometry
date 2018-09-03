--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Tests.Triangle2d exposing (triangleContainsOwnCentroid)

import Expect
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Triangle2d


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
