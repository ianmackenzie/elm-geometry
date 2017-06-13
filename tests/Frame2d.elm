module Frame2d
    exposing
        ( globalToGlobal
        , jsonRoundTrips
        , localToLocal
        )

import Generic
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point2d as Point2d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.frame2d Encode.frame2d Decode.frame2d


globalToGlobal : Test
globalToGlobal =
    let
        description =
            "Global -> local -> global conversion round-trips properly"

        expectation frame point =
            point
                |> Point2d.relativeTo frame
                |> Point2d.placeIn frame
                |> Expect.point2d point
    in
    Test.fuzz2 Fuzz.frame2d Fuzz.point2d description expectation


localToLocal : Test
localToLocal =
    let
        description =
            "Local -> global -> local conversion round-trips properly"

        expectation frame point =
            point
                |> Point2d.placeIn frame
                |> Point2d.relativeTo frame
                |> Expect.point2d point
    in
    Test.fuzz2 Fuzz.frame2d Fuzz.point2d description expectation
