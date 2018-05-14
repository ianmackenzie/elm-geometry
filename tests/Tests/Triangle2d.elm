module Tests.Triangle2d
    exposing
        ( jsonRoundTrips
        , triangleContainsOwnCentroid
        )

import Expect
import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Tests.Generic as Generic
import Triangle2d


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
