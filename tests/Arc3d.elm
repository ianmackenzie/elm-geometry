module Arc3d
    exposing
        ( evaluateOneIsEndPoint
        , evaluateZeroIsStartPoint
        , jsonRoundTrips
        , projectInto
        , reverseFlipsDirection
        )

import Fuzz
import Generic
import OpenSolid.Arc3d as Arc3d
import OpenSolid.EllipticalArc2d as EllipticalArc2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point3d as Point3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.arc3d
        Encode.arc3d
        Decode.arc3d


evaluateZeroIsStartPoint : Test
evaluateZeroIsStartPoint =
    Test.fuzz Fuzz.arc3d
        "Evaluating at t=0 returns start point"
        (\arc -> Arc3d.pointOn arc 0 |> Expect.point3d (Arc3d.startPoint arc))


evaluateOneIsEndPoint : Test
evaluateOneIsEndPoint =
    Test.fuzz Fuzz.arc3d
        "Evaluating at t=1 returns end point"
        (\arc -> Arc3d.pointOn arc 1 |> Expect.point3d (Arc3d.endPoint arc))


reverseFlipsDirection : Test
reverseFlipsDirection =
    Test.fuzz2 Fuzz.arc3d
        (Fuzz.floatRange 0 1)
        "Reversing an arc is consistent with reversed evaluation"
        (\arc t ->
            Arc3d.pointOn (Arc3d.reverse arc) t
                |> Expect.point3d (Arc3d.pointOn arc (1 - t))
        )


projectInto : Test
projectInto =
    Test.fuzz3
        Fuzz.arc3d
        Fuzz.sketchPlane3d
        (Fuzz.floatRange 0 1)
        "Projecting an arc works properly"
        (\arc sketchPlane parameterValue ->
            let
                projectedArc =
                    Arc3d.projectInto sketchPlane arc

                pointOnOriginalArc =
                    Arc3d.pointOn arc parameterValue

                pointOnProjectedArc =
                    EllipticalArc2d.pointOn projectedArc parameterValue

                projectedPoint =
                    Point3d.projectInto sketchPlane pointOnOriginalArc
            in
            pointOnProjectedArc |> Expect.point2d projectedPoint
        )
