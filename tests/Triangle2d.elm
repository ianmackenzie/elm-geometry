module Triangle2d
    exposing
        ( jsonRoundTrips
        , triangleContainsOwnCentroid
        )

import Expect
import Generic
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Triangle2d as Triangle2d
import Test exposing (Test)


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
