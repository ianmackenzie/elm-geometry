module Tests.RationalQuadraticSpline3d exposing
    ( bSplines
    , genericTests
    , secondDerivativeBoundingBox
    , splitAt
    )

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Random as Random
import Interval
import Length exposing (Meters)
import Point3d
import Quantity
import Random
import RationalQuadraticSpline3d exposing (RationalQuadraticSpline3d)
import Shrink
import Test exposing (Test)
import Tests.Generic.Curve3d
import Vector3d


curveOperations : Tests.Generic.Curve3d.Operations (RationalQuadraticSpline3d Meters coordinates) coordinates
curveOperations =
    { generator = Random.rationalQuadraticSpline3d
    , pointOn = RationalQuadraticSpline3d.pointOn
    , boundingBox = RationalQuadraticSpline3d.boundingBox
    , firstDerivative = RationalQuadraticSpline3d.firstDerivative
    , firstDerivativeBoundingBox = RationalQuadraticSpline3d.firstDerivativeBoundingBox
    , scaleAbout = RationalQuadraticSpline3d.scaleAbout
    , translateBy = RationalQuadraticSpline3d.translateBy
    , rotateAround = RationalQuadraticSpline3d.rotateAround
    , mirrorAcross = RationalQuadraticSpline3d.mirrorAcross
    , numApproximationSegments = RationalQuadraticSpline3d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve3d.tests
        curveOperations
        curveOperations
        RationalQuadraticSpline3d.placeIn
        RationalQuadraticSpline3d.relativeTo


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
                    [ 0, 0, 1, 2, 3, 4, 5, 8, 10, 10 ]

                weightedControlPoints =
                    [ ( Point3d.meters 1 8 1, 2 )
                    , ( Point3d.meters 4 4 2, 5 )
                    , ( Point3d.meters 2 4 1, 1 )
                    , ( Point3d.meters 4 1 2, 3 )
                    , ( Point3d.meters 8 2 1, 1 )
                    , ( Point3d.meters 5 6 2, 5 )
                    , ( Point3d.meters 8 9 1, 1 )
                    , ( Point3d.meters 9 7 2, 2 )
                    , ( Point3d.meters 9 4 1, 1 )
                    ]

                splines =
                    RationalQuadraticSpline3d.bSplineSegments knots weightedControlPoints

                knotIntervals =
                    RationalQuadraticSpline3d.bSplineIntervals knots

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
                                RationalQuadraticSpline3d.endPoint firstSpline

                            secondStartPoint =
                                RationalQuadraticSpline3d.startPoint secondSpline

                            firstEndDerivative =
                                RationalQuadraticSpline3d.endDerivative firstSpline
                                    |> Vector3d.scaleBy (1.0 / Interval.width firstInterval)

                            secondStartDerivative =
                                RationalQuadraticSpline3d.startDerivative secondSpline
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
            Fuzz.rationalQuadraticSpline3d
            Fuzz.parameterValue
            Fuzz.parameterValue
            "first"
            (\spline t0 t1 ->
                let
                    ( first, _ ) =
                        RationalQuadraticSpline3d.splitAt t0 spline
                in
                RationalQuadraticSpline3d.pointOn first t1
                    |> Expect.point3d
                        (RationalQuadraticSpline3d.pointOn spline (t1 * t0))
            )
        , Test.fuzz3
            Fuzz.rationalQuadraticSpline3d
            Fuzz.parameterValue
            Fuzz.parameterValue
            "second"
            (\spline t0 t1 ->
                let
                    ( _, second ) =
                        RationalQuadraticSpline3d.splitAt t0 spline
                in
                RationalQuadraticSpline3d.pointOn second t1
                    |> Expect.point3d
                        (RationalQuadraticSpline3d.pointOn spline (t0 + t1 * (1 - t0)))
            )
        ]


secondDerivativeBoundingBox : Test
secondDerivativeBoundingBox =
    Tests.Generic.Curve3d.secondDerivativeBoundingBox
        { generator = Random.rationalQuadraticSpline3d
        , secondDerivative = RationalQuadraticSpline3d.secondDerivative
        , secondDerivativeBoundingBox = RationalQuadraticSpline3d.secondDerivativeBoundingBox
        }
