module Circle2d
    exposing
        ( boundingBoxContainsCenter
        , jsonRoundTrips
        )

import Expect
import Generic
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Circle2d as Circle2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import Test exposing (Test)


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
