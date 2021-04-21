module Tests.RationalCubicSpline3d exposing
    ( bSplines
    , genericTests
    , secondDerivativeBoundingBox
    , splitAt
    )

import CubicSpline3d
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Random as Random
import Interval
import Length exposing (Length, Meters)
import Point3d exposing (Point3d)
import Quantity
import Random
import RationalCubicSpline3d exposing (RationalCubicSpline3d)
import Shrink
import Test exposing (Test)
import Tests.Generic.Curve3d
import Vector3d


curveOperations : Tests.Generic.Curve3d.Operations (RationalCubicSpline3d Meters coordinates) coordinates
curveOperations =
    { generator = Random.rationalCubicSpline3d
    , pointOn = RationalCubicSpline3d.pointOn
    , boundingBox = RationalCubicSpline3d.boundingBox
    , firstDerivative = RationalCubicSpline3d.firstDerivative
    , firstDerivativeBoundingBox = RationalCubicSpline3d.firstDerivativeBoundingBox
    , scaleAbout = RationalCubicSpline3d.scaleAbout
    , translateBy = RationalCubicSpline3d.translateBy
    , rotateAround = RationalCubicSpline3d.rotateAround
    , mirrorAcross = RationalCubicSpline3d.mirrorAcross
    , numApproximationSegments = RationalCubicSpline3d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve3d.tests
        curveOperations
        curveOperations
        RationalCubicSpline3d.placeIn
        RationalCubicSpline3d.relativeTo


expectAll : List Expectation -> Expectation
expectAll expectations =
    () |> Expect.all (List.map always expectations)


forEach : (a -> Expectation) -> List a -> Expectation
forEach toExpectation values =
    case values of
        [] ->
            Expect.pass

        _ ->
            () |> Expect.all (List.map (\value () -> toExpectation value) values)


bSplines : Test
bSplines =
    Test.test "B-splines are continuous" <|
        \() ->
            let
                knots =
                    [ 0, 0, 0, 1, 2, 4, 4, 5, 8, 10, 10, 10 ]

                weightedControlPoints =
                    [ ( Point3d.meters 0 0 1, 1 )
                    , ( Point3d.meters 1 8 2, 2 )
                    , ( Point3d.meters 4 4 3, 5 )
                    , ( Point3d.meters 2 4 2, 1 )
                    , ( Point3d.meters 4 1 1, 3 )
                    , ( Point3d.meters 8 2 2, 1 )
                    , ( Point3d.meters 5 6 3, 5 )
                    , ( Point3d.meters 8 9 2, 1 )
                    , ( Point3d.meters 9 7 1, 2 )
                    , ( Point3d.meters 9 4 2, 1 )
                    ]

                splines =
                    RationalCubicSpline3d.bSplineSegments knots weightedControlPoints

                knotIntervals =
                    RationalCubicSpline3d.bSplineIntervals knots

                segments =
                    List.map2 Tuple.pair splines knotIntervals

                pairs =
                    List.map2 Tuple.pair segments (List.drop 1 segments)
            in
            pairs
                |> forEach
                    (\( ( firstSpline, firstInterval ), ( secondSpline, secondInterval ) ) ->
                        let
                            firstEndPoint =
                                RationalCubicSpline3d.endPoint firstSpline

                            secondStartPoint =
                                RationalCubicSpline3d.startPoint secondSpline

                            firstEndDerivative =
                                RationalCubicSpline3d.endDerivative firstSpline
                                    |> Vector3d.scaleBy (1.0 / Interval.width firstInterval)

                            secondStartDerivative =
                                RationalCubicSpline3d.startDerivative secondSpline
                                    |> Vector3d.scaleBy (1.0 / Interval.width secondInterval)
                        in
                        expectAll
                            [ firstEndPoint |> Expect.point3d secondStartPoint
                            , firstEndDerivative |> Expect.vector3d secondStartDerivative
                            ]
                    )


splitAt : Test
splitAt =
    Test.describe "splitAt"
        [ Test.fuzz3
            Fuzz.rationalCubicSpline3d
            Fuzz.parameterValue
            Fuzz.parameterValue
            "first"
            (\spline t0 t1 ->
                let
                    ( first, _ ) =
                        RationalCubicSpline3d.splitAt t0 spline
                in
                RationalCubicSpline3d.pointOn first t1
                    |> Expect.point3d
                        (RationalCubicSpline3d.pointOn spline (t1 * t0))
            )
        , Test.fuzz3
            Fuzz.rationalCubicSpline3d
            Fuzz.parameterValue
            Fuzz.parameterValue
            "second"
            (\spline t0 t1 ->
                let
                    ( _, second ) =
                        RationalCubicSpline3d.splitAt t0 spline
                in
                RationalCubicSpline3d.pointOn second t1
                    |> Expect.point3d
                        (RationalCubicSpline3d.pointOn spline (t0 + t1 * (1 - t0)))
            )
        ]


secondDerivativeBoundingBox : Test
secondDerivativeBoundingBox =
    Tests.Generic.Curve3d.secondDerivativeBoundingBox
        { generator = Random.rationalCubicSpline3d
        , secondDerivative = RationalCubicSpline3d.secondDerivative
        , secondDerivativeBoundingBox = RationalCubicSpline3d.secondDerivativeBoundingBox
        }
