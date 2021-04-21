module Tests.Arc3d exposing
    ( evaluateHalfIsMidpoint
    , evaluateOneIsEndPoint
    , evaluateZeroIsStartPoint
    , genericTests
    , projectInto
    , reverseFlipsDirection
    , reverseKeepsMidpoint
    )

import Angle
import Arc3d exposing (Arc3d)
import EllipticalArc2d
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Random as Random
import Length exposing (Meters)
import Point3d
import Test exposing (Test)
import Tests.Generic.Curve3d


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


evaluateHalfIsMidpoint : Test
evaluateHalfIsMidpoint =
    Test.fuzz Fuzz.arc3d
        "Evaluating at t=0.5 returns midpoint"
        (\arc -> Arc3d.pointOn arc 0.5 |> Expect.point3d (Arc3d.midpoint arc))


reverseKeepsMidpoint : Test
reverseKeepsMidpoint =
    Test.fuzz Fuzz.arc3d
        "Reversing an arc keeps the midpoint"
        (\arc ->
            Arc3d.midpoint (Arc3d.reverse arc)
                |> Expect.point3d (Arc3d.midpoint arc)
        )


reverseFlipsDirection : Test
reverseFlipsDirection =
    Test.fuzz2 Fuzz.arc3d
        Fuzz.parameterValue
        "Reversing an arc is consistent with reversed evaluation"
        (\arc parameterValue ->
            Arc3d.pointOn (Arc3d.reverse arc) parameterValue
                |> Expect.point3d
                    (Arc3d.pointOn arc (1 - parameterValue))
        )


projectInto : Test
projectInto =
    Test.fuzz3
        Fuzz.arc3d
        Fuzz.sketchPlane3d
        Fuzz.parameterValue
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
                    pointOnOriginalArc |> Point3d.projectInto sketchPlane
            in
            pointOnProjectedArc |> Expect.point2d projectedPoint
        )


curveOperations : Tests.Generic.Curve3d.Operations (Arc3d Meters coordinates) coordinates
curveOperations =
    { generator = Random.arc3d
    , pointOn = Arc3d.pointOn
    , boundingBox = Arc3d.boundingBox
    , firstDerivative = Arc3d.firstDerivative
    , firstDerivativeBoundingBox = Arc3d.firstDerivativeBoundingBox
    , scaleAbout = Arc3d.scaleAbout
    , translateBy = Arc3d.translateBy
    , rotateAround = Arc3d.rotateAround
    , mirrorAcross = Arc3d.mirrorAcross
    , numApproximationSegments = Arc3d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve3d.tests
        curveOperations
        curveOperations
        Arc3d.placeIn
        Arc3d.relativeTo
