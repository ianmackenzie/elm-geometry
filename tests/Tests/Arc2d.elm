module Tests.Arc2d exposing
    ( evaluateHalfIsMidpoint
    , evaluateOneIsEndPoint
    , evaluateZeroIsStartPoint
    , from
    , genericTests
    , mirroredCenterPoint
    , reverseFlipsDirection
    , reverseKeepsMidpoint
    , withRadius
    )

import Angle
import Arc2d exposing (Arc2d)
import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Random as Random
import Length exposing (Meters, meters)
import Point2d
import Quantity exposing (zero)
import SweptAngle
import Test exposing (Test)
import Tests.Generic.Curve2d


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


evaluateHalfIsMidpoint : Test
evaluateHalfIsMidpoint =
    Test.fuzz Fuzz.arc2d
        "Evaluating at t=0.5 returns midpoint"
        (\arc -> Arc2d.pointOn arc 0.5 |> Expect.point2d (Arc2d.midpoint arc))


reverseKeepsMidpoint : Test
reverseKeepsMidpoint =
    Test.fuzz Fuzz.arc2d
        "Reversing an arc keeps the midpoint"
        (\arc ->
            Arc2d.midpoint (Arc2d.reverse arc)
                |> Expect.point2d (Arc2d.midpoint arc)
        )


reverseFlipsDirection : Test
reverseFlipsDirection =
    Test.fuzz2 Fuzz.arc2d
        Fuzz.parameterValue
        "Reversing an arc is consistent with reversed evaluation"
        (\arc parameterValue ->
            Arc2d.pointOn (Arc2d.reverse arc) parameterValue
                |> Expect.point2d
                    (Arc2d.pointOn arc (1 - parameterValue))
        )


from : Test
from =
    let
        validAngle =
            Fuzz.map Angle.degrees <|
                Fuzz.oneOf
                    [ Fuzz.floatRange -359 359
                    , Fuzz.floatRange 361 719
                    , Fuzz.floatRange -719 -361
                    ]
    in
    Test.fuzz3
        Fuzz.point2d
        Fuzz.point2d
        validAngle
        "Arc2d.from produces the expected end point and swept angle"
        (\startPoint endPoint sweptAngle ->
            Arc2d.from startPoint endPoint sweptAngle
                |> Expect.all
                    [ Arc2d.endPoint >> Expect.point2d endPoint
                    , Arc2d.sweptAngle >> Expect.quantity sweptAngle
                    ]
        )


withRadius : Test
withRadius =
    let
        sweptAngleTypes =
            List.map Fuzz.constant
                [ SweptAngle.smallPositive
                , SweptAngle.smallNegative
                , SweptAngle.largePositive
                , SweptAngle.largeNegative
                ]
    in
    Test.fuzz3
        Fuzz.positiveLength
        (Fuzz.oneOf sweptAngleTypes)
        (Fuzz.tuple ( Fuzz.point2d, Fuzz.point2d ))
        "Arc2d.withRadius produces the expected end point"
        (\radius sweptAngleType ( startPoint, endPoint ) ->
            case Arc2d.withRadius radius sweptAngleType startPoint endPoint of
                Just arc ->
                    arc |> Arc2d.endPoint |> Expect.point2d endPoint

                Nothing ->
                    let
                        distance =
                            Point2d.distanceFrom startPoint endPoint
                    in
                    if distance == zero then
                        Expect.pass

                    else
                        distance |> Expect.quantityGreaterThan (Quantity.multiplyBy 2 radius)
        )


curveOperations : Tests.Generic.Curve2d.Operations (Arc2d Meters coordinates) coordinates
curveOperations =
    { generator = Random.arc2d
    , pointOn = Arc2d.pointOn
    , boundingBox = Arc2d.boundingBox
    , firstDerivative = Arc2d.firstDerivative
    , firstDerivativeBoundingBox = Arc2d.firstDerivativeBoundingBox
    , scaleAbout = Arc2d.scaleAbout
    , translateBy = Arc2d.translateBy
    , rotateAround = Arc2d.rotateAround
    , mirrorAcross = Arc2d.mirrorAcross
    , numApproximationSegments = Arc2d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve2d.tests
        curveOperations
        curveOperations
        Arc2d.placeIn
        Arc2d.relativeTo


mirroredCenterPoint : Test
mirroredCenterPoint =
    Test.fuzz2
        Fuzz.arc2d
        Fuzz.axis2d
        "Center point of a mirrored finite-radius arc is the mirror of the original center point"
        (\arc axis ->
            if Arc2d.radius arc |> Quantity.lessThan (meters 100) then
                let
                    mirroredArc =
                        Arc2d.mirrorAcross axis arc
                in
                Arc2d.centerPoint mirroredArc
                    |> Expect.point2d
                        (Point2d.mirrorAcross axis (Arc2d.centerPoint arc))

            else
                Expect.pass
        )
