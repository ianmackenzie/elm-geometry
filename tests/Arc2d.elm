module Arc2d
    exposing
        ( evaluateOneIsEndPoint
        , evaluateZeroIsStartPoint
        , jsonRoundTrips
        )

import Expect
import Generic
import OpenSolid.Arc2d as Arc2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.arc2d
        Encode.arc2d
        Decode.arc2d


evaluateZeroIsStartPoint : Test
evaluateZeroIsStartPoint =
    Test.fuzz Fuzz.arc2d
        "Evaluating at t=0 returns start point"
        (\arc -> Arc2d.pointOn arc 0 |> Expect.point2d (Arc2d.startPoint arc))


evaluateOneIsEndPoint : Test
evaluateOneIsEndPoint =
    Test.fuzz Fuzz.arc2d
        "Evaluating at t=1 returns end point"
        (\arc -> Arc2d.pointOn arc 1 |> Expect.point2d (Arc2d.endPoint arc))
