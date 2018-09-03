--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Tests.Circle2d exposing (boundingBoxContainsCenter)

import BoundingBox2d
import Circle2d
import Expect
import Geometry.Fuzz as Fuzz
import Test exposing (Test)


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
