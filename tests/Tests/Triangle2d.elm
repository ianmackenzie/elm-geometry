module Tests.Triangle2d exposing (triangleContainsOwnCentroid)

import Expect
import Geometry.Fuzz as Fuzz
import Quantity
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
                (area == Quantity.zero || Triangle2d.contains centroid triangle)
        )
