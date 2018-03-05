module Tests.Arc2d
    exposing
        ( evaluateOneIsEndPoint
        , evaluateZeroIsStartPoint
        , jsonRoundTrips
        , reverseFlipsDirection
        , withRadius
        , withSweptAngle
        )

import Arc2d
import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.SweptAngle as SweptAngle
import Point2d
import Test exposing (Test)


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


reverseFlipsDirection : Test
reverseFlipsDirection =
    Test.fuzz2 Fuzz.arc2d
        (Fuzz.floatRange 0 1)
        "Reversing an arc is consistent with reversed evaluation"
        (\arc t ->
            Arc2d.pointOn (Arc2d.reverse arc) t
                |> Expect.point2d (Arc2d.pointOn arc (1 - t))
        )


withSweptAngle : Test
withSweptAngle =
    let
        validAngle =
            Fuzz.oneOf
                [ Fuzz.floatRange (degrees 1) (degrees 359)
                , Fuzz.floatRange (degrees -359) (degrees -1)
                ]
    in
    Test.fuzz3
        validAngle
        Fuzz.point2d
        Fuzz.point2d
        "Arc2d.withSweptAngle produces the expected end point and swept angle"
        (\sweptAngle startPoint endPoint ->
            case Arc2d.withSweptAngle sweptAngle startPoint endPoint of
                Just arc ->
                    arc
                        |> Expect.all
                            [ Arc2d.endPoint
                                >> Expect.point2d endPoint
                            , Arc2d.sweptAngle
                                >> Expect.approximately sweptAngle
                            ]

                Nothing ->
                    Expect.fail
                        "Arc2d.withSweptAngle did not produce a valid arc"
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
    Test.fuzz4
        (Fuzz.map abs Fuzz.scalar)
        (Fuzz.oneOf sweptAngleTypes)
        Fuzz.point2d
        Fuzz.point2d
        "Arc2d.withRadius produces the expected end point"
        (\radius sweptAngleType startPoint endPoint ->
            case Arc2d.withRadius radius sweptAngleType startPoint endPoint of
                Just arc ->
                    arc |> Arc2d.endPoint |> Expect.point2d endPoint

                Nothing ->
                    Point2d.distanceFrom startPoint endPoint
                        |> Expect.greaterThan (2 * radius)
        )
