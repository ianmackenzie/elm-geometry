module Tests.Arc3d exposing (..)

import Arc3d
import EllipticalArc2d
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point3d
import Test exposing (Test)
import Tests.Generic.Curve3d


evaluateZeroIsStartPoint : Test
evaluateZeroIsStartPoint =
    Test.fuzz Fuzz.arc3d
        "Evaluating at t=0 returns start point"
        (\arc ->
            Arc3d.pointOn arc 0
                |> Expect.just Expect.point3d (Arc3d.startPoint arc)
        )


evaluateOneIsEndPoint : Test
evaluateOneIsEndPoint =
    Test.fuzz Fuzz.arc3d
        "Evaluating at t=1 returns end point"
        (\arc ->
            Arc3d.pointOn arc 1
                |> Expect.just Expect.point3d (Arc3d.endPoint arc)
        )


reverseFlipsDirection : Test
reverseFlipsDirection =
    Test.fuzz2 Fuzz.arc3d
        (Fuzz.floatRange 0 1)
        "Reversing an arc is consistent with reversed evaluation"
        (\arc t ->
            Arc3d.pointOn (Arc3d.reverse arc) t
                |> Expect.maybe Expect.point3d (Arc3d.pointOn arc (1 - t))
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
                    pointOnOriginalArc
                        |> Maybe.map (Point3d.projectInto sketchPlane)
            in
            pointOnProjectedArc |> Expect.maybe Expect.point2d projectedPoint
        )


transformations : Test
transformations =
    Tests.Generic.Curve3d.transformations
        { fuzzer = Fuzz.arc3d
        , pointOn = Arc3d.pointOn
        , derivative = Arc3d.derivativeVector
        , scaleAbout = Arc3d.scaleAbout
        , translateBy = Arc3d.translateBy
        , rotateAround = Arc3d.rotateAround
        , mirrorAcross = Arc3d.mirrorAcross
        , relativeTo = Arc3d.relativeTo
        , placeIn = Arc3d.placeIn
        }
