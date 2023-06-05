module Tests.EllipticalArc2d exposing
    ( fromEndpointsReplicatesArc
    , genericTests
    , reproducibleArc
    , reverseKeepsMidpoint
    , signedDistanceAlong
    )

import Angle
import Arc2d exposing (Arc2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters, meters)
import Point2d
import Quantity exposing (zero)
import Random exposing (Generator)
import SweptAngle
import Test exposing (Test)
import Test.Check as Test
import Tests.Generic.Curve2d
import Vector2d


reproducibleArc : Generator (Arc2d Meters coordinates)
reproducibleArc =
    Random.map4
        (\centerPoint startDirection radius sweptAngle ->
            let
                startPoint =
                    centerPoint |> Point2d.translateIn startDirection radius
            in
            startPoint |> Arc2d.sweptAround centerPoint sweptAngle
        )
        Random.point2d
        Random.direction2d
        (Random.map meters (Random.float 0.1 10))
        (Random.map Angle.degrees <|
            Random.oneOf
                (Random.float 1 179)
                [ Random.float 181 359
                , Random.float -179 -1
                , Random.float -359 -181
                ]
        )


fromEndpointsReplicatesArc : Test
fromEndpointsReplicatesArc =
    Test.check2 "fromEndpoints accurately replicates circular arcs"
        reproducibleArc
        Random.direction2d
        (\arc xDirection ->
            let
                radius =
                    Arc2d.radius arc

                arcSweptAngle =
                    Arc2d.sweptAngle arc

                sweptAngle =
                    if arcSweptAngle |> Quantity.greaterThanOrEqualTo (Angle.radians pi) then
                        SweptAngle.largePositive

                    else if arcSweptAngle |> Quantity.greaterThanOrEqualTo zero then
                        SweptAngle.smallPositive

                    else if arcSweptAngle |> Quantity.greaterThanOrEqualTo (Angle.radians -pi) then
                        SweptAngle.smallNegative

                    else
                        SweptAngle.largeNegative

                result =
                    EllipticalArc2d.fromEndpoints
                        { startPoint = Arc2d.startPoint arc
                        , endPoint = Arc2d.endPoint arc
                        , xRadius = radius
                        , yRadius = radius
                        , xDirection = xDirection
                        , sweptAngle = sweptAngle
                        }
            in
            case result of
                Nothing ->
                    Expect.fail "fromEndpoints could not reproduce arc"

                Just ellipticalArc ->
                    EllipticalArc2d.centerPoint ellipticalArc
                        |> Expect.point2d (Arc2d.centerPoint arc)
        )


reverseKeepsMidpoint : Test
reverseKeepsMidpoint =
    Test.check1 "Reversing an elliptical arc keeps the midpoint"
        Random.ellipticalArc2d
        (\arc ->
            case
                ( EllipticalArc2d.nondegenerate arc
                , EllipticalArc2d.nondegenerate (EllipticalArc2d.reverse arc)
                )
            of
                ( Ok nondegenerateArc, Ok nondegenerateReversedArc ) ->
                    let
                        parametrizedArc =
                            nondegenerateArc
                                |> EllipticalArc2d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }

                        parametrizedReversedArc =
                            nondegenerateReversedArc
                                |> EllipticalArc2d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }
                    in
                    EllipticalArc2d.midpoint parametrizedArc
                        |> Expect.point2dWithin (meters 1.0e-3)
                            (EllipticalArc2d.midpoint parametrizedReversedArc)

                _ ->
                    Expect.pass
        )


curveOperations : Tests.Generic.Curve2d.Operations (EllipticalArc2d Meters coordinates) coordinates
curveOperations =
    { generator = Random.ellipticalArc2d
    , pointOn = EllipticalArc2d.pointOn
    , boundingBox = EllipticalArc2d.boundingBox
    , firstDerivative = EllipticalArc2d.firstDerivative
    , firstDerivativeBoundingBox = EllipticalArc2d.firstDerivativeBoundingBox
    , scaleAbout = EllipticalArc2d.scaleAbout
    , translateBy = EllipticalArc2d.translateBy
    , rotateAround = EllipticalArc2d.rotateAround
    , mirrorAcross = EllipticalArc2d.mirrorAcross
    , numApproximationSegments = EllipticalArc2d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve2d.tests
        curveOperations
        curveOperations
        EllipticalArc2d.placeIn
        EllipticalArc2d.relativeTo


signedDistanceAlong : Test
signedDistanceAlong =
    Test.check3 "signedDistanceAlong"
        Random.ellipticalArc2d
        Random.axis2d
        Random.parameterValue
        (\arc axis parameterValue ->
            let
                distanceInterval =
                    EllipticalArc2d.signedDistanceAlong axis arc

                projectedDistance =
                    Point2d.signedDistanceAlong axis
                        (EllipticalArc2d.pointOn arc parameterValue)
            in
            projectedDistance |> Expect.quantityContainedIn distanceInterval
        )
