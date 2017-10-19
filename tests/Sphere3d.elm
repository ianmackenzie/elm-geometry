module Sphere3d
    exposing
        ( boundingBoxContainsCenter
        , jsonRoundTrips
        )

import Expect
import Generic
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Circle3d as Circle3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.sphere3d
        Encode.sphere3d
        Decode.sphere3d


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    Test.fuzz Fuzz.circle3d
        "A circle's bounding box contains its center point"
        (\circle ->
            let
                boundingBox =
                    Circle3d.boundingBox circle

                centerPoint =
                    Circle3d.centerPoint circle
            in
            Expect.true
                "Circle bounding box does not contain the center point"
                (BoundingBox3d.contains centerPoint boundingBox)
        )



{- tests : Test
   tests =
       describe "Sphere3d example tests"
           <| List.map
               \points radius ->
                   test "" <|
                       \_ ->
                           Expect.
       test
           ""
           \_ ->
-}
