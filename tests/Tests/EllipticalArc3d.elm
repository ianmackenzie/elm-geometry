module Tests.EllipticalArc3d exposing
    ( evaluateOneIsEndPoint
    , evaluateZeroIsStartPoint
    , projectInto
    , signedDistanceAlong
    )

import Angle
import EllipticalArc2d
import EllipticalArc3d exposing (EllipticalArc3d)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters)
import Point3d
import Test exposing (Test)
import Test.Check as Test
import Tests.Generic.Curve3d


evaluateZeroIsStartPoint : Test
evaluateZeroIsStartPoint =
    Test.check "Evaluating at t=0 returns start point"
        Random.ellipticalArc3d
        (\arc -> EllipticalArc3d.pointOn arc 0 |> Expect.point3d (EllipticalArc3d.startPoint arc))


evaluateOneIsEndPoint : Test
evaluateOneIsEndPoint =
    Test.check "Evaluating at t=1 returns end point"
        Random.ellipticalArc3d
        (\arc -> EllipticalArc3d.pointOn arc 1 |> Expect.point3d (EllipticalArc3d.endPoint arc))


projectInto : Test
projectInto =
    Test.check3 "Projecting an arc works properly"
        Random.ellipticalArc3d
        Random.sketchPlane3d
        Random.parameterValue
        (\arc sketchPlane parameterValue ->
            let
                projectedArc =
                    EllipticalArc3d.projectInto sketchPlane arc

                pointOnOriginalArc =
                    EllipticalArc3d.pointOn arc parameterValue

                pointOnProjectedArc =
                    EllipticalArc2d.pointOn projectedArc parameterValue

                projectedPoint =
                    pointOnOriginalArc |> Point3d.projectInto sketchPlane
            in
            pointOnProjectedArc |> Expect.point2d projectedPoint
        )


curveOperations : Tests.Generic.Curve3d.Operations (EllipticalArc3d Meters coordinates) coordinates
curveOperations =
    { generator = Random.ellipticalArc3d
    , pointOn = EllipticalArc3d.pointOn
    , boundingBox = EllipticalArc3d.boundingBox
    , firstDerivative = EllipticalArc3d.firstDerivative
    , firstDerivativeBoundingBox = EllipticalArc3d.firstDerivativeBoundingBox
    , scaleAbout = EllipticalArc3d.scaleAbout
    , translateBy = EllipticalArc3d.translateBy
    , rotateAround = EllipticalArc3d.rotateAround
    , mirrorAcross = EllipticalArc3d.mirrorAcross
    , numApproximationSegments = EllipticalArc3d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve3d.tests
        curveOperations
        curveOperations
        EllipticalArc3d.placeIn
        EllipticalArc3d.relativeTo


signedDistanceAlong : Test
signedDistanceAlong =
    Test.check3 "signedDistanceAlong"
        Random.ellipticalArc3d
        Random.axis3d
        Random.parameterValue
        (\arc axis parameterValue ->
            let
                distanceInterval =
                    EllipticalArc3d.signedDistanceAlong axis arc

                projectedDistance =
                    Point3d.signedDistanceAlong axis
                        (EllipticalArc3d.pointOn arc parameterValue)
            in
            projectedDistance |> Expect.quantityContainedIn distanceInterval
        )
