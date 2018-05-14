module Tests.Circle2d
    exposing
        ( boundingBoxContainsCenter
        , jsonRoundTrips
        )

import BoundingBox2d
import Circle2d
import Expect
import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Tests.Generic as Generic


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
