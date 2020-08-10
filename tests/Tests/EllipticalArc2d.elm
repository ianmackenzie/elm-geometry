module Tests.EllipticalArc2d exposing
    ( boundingBox
    , fromEndpointsReplicatesArc
    , reproducibleArc
    , reverseKeepsMidpoint
    , transformations
    )

import Angle
import Arc2d exposing (Arc2d)
import EllipticalArc2d
import Expect
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Meters, meters)
import Point2d
import Quantity exposing (zero)
import SweptAngle
import Test exposing (Test)
import Tests.Generic.Curve2d
import Vector2d


reproducibleArc : Fuzzer (Arc2d Meters coordinates)
reproducibleArc =
    Fuzz.map4
        (\centerPoint startDirection radius sweptAngle ->
            let
                startPoint =
                    centerPoint |> Point2d.translateIn startDirection radius
            in
            startPoint |> Arc2d.sweptAround centerPoint sweptAngle
        )
        Fuzz.point2d
        Fuzz.direction2d
        (Fuzz.map meters (Fuzz.floatRange 0.1 10))
        (Fuzz.map Angle.degrees <|
            Fuzz.oneOf
                [ Fuzz.floatRange 1 179
                , Fuzz.floatRange 181 359
                , Fuzz.floatRange -179 -1
                , Fuzz.floatRange -359 -181
                ]
        )


fromEndpointsReplicatesArc : Test
fromEndpointsReplicatesArc =
    Test.fuzz2
        reproducibleArc
        Fuzz.direction2d
        "fromEndpoints accurately replicates circular arcs"
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
    Test.fuzz Fuzz.ellipticalArc2d
        "Reversing an elliptical arc keeps the midpoint"
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


transformations : Test
transformations =
    let
        curveOperations =
            { fuzzer = Fuzz.ellipticalArc2d
            , pointOn = EllipticalArc2d.pointOn
            , firstDerivative = EllipticalArc2d.firstDerivative
            , scaleAbout = EllipticalArc2d.scaleAbout
            , translateBy = EllipticalArc2d.translateBy
            , rotateAround = EllipticalArc2d.rotateAround
            , mirrorAcross = EllipticalArc2d.mirrorAcross
            }
    in
    Tests.Generic.Curve2d.transformations
        curveOperations
        curveOperations
        EllipticalArc2d.placeIn
        EllipticalArc2d.relativeTo


boundingBox : Test
boundingBox =
    Test.fuzz2
        Fuzz.ellipticalArc2d
        (Fuzz.floatRange 0 1)
        "Every point on an arc is within its bounding box"
        (\arc parameterValue ->
            EllipticalArc2d.pointOn arc parameterValue
                |> Expect.point2dContainedIn (EllipticalArc2d.boundingBox arc)
        )
