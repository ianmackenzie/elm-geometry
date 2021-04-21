module Tests.RationalQuadraticSpline2d exposing
    ( bSplines
    , genericTests
    , secondDerivativeBoundingBox
    , splitAt
    )

import CubicSpline2d
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Random as Random
import Interval
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Quantity
import Random
import RationalQuadraticSpline2d exposing (RationalQuadraticSpline2d)
import Shrink
import Test exposing (Test)
import Tests.Generic.Curve2d
import Vector2d


curveOperations : Tests.Generic.Curve2d.Operations (RationalQuadraticSpline2d Meters coordinates) coordinates
curveOperations =
    { generator = Random.rationalQuadraticSpline2d
    , pointOn = RationalQuadraticSpline2d.pointOn
    , boundingBox = RationalQuadraticSpline2d.boundingBox
    , firstDerivative = RationalQuadraticSpline2d.firstDerivative
    , firstDerivativeBoundingBox = RationalQuadraticSpline2d.firstDerivativeBoundingBox
    , scaleAbout = RationalQuadraticSpline2d.scaleAbout
    , translateBy = RationalQuadraticSpline2d.translateBy
    , rotateAround = RationalQuadraticSpline2d.rotateAround
    , mirrorAcross = RationalQuadraticSpline2d.mirrorAcross
    , numApproximationSegments = RationalQuadraticSpline2d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve2d.tests
        curveOperations
        curveOperations
        RationalQuadraticSpline2d.placeIn
        RationalQuadraticSpline2d.relativeTo


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
                    [ ( Point2d.meters 1 8, 2 )
                    , ( Point2d.meters 4 4, 5 )
                    , ( Point2d.meters 2 4, 1 )
                    , ( Point2d.meters 4 1, 3 )
                    , ( Point2d.meters 8 2, 1 )
                    , ( Point2d.meters 5 6, 5 )
                    , ( Point2d.meters 8 9, 1 )
                    , ( Point2d.meters 9 7, 2 )
                    , ( Point2d.meters 9 4, 1 )
                    ]

                splines =
                    RationalQuadraticSpline2d.bSplineSegments knots weightedControlPoints

                knotIntervals =
                    RationalQuadraticSpline2d.bSplineIntervals knots

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
                                RationalQuadraticSpline2d.endPoint firstSpline

                            secondStartPoint =
                                RationalQuadraticSpline2d.startPoint secondSpline

                            firstEndDerivative =
                                RationalQuadraticSpline2d.endDerivative firstSpline
                                    |> Vector2d.scaleBy (1.0 / Interval.width firstInterval)

                            secondStartDerivative =
                                RationalQuadraticSpline2d.startDerivative secondSpline
                                    |> Vector2d.scaleBy (1.0 / Interval.width secondInterval)
                        in
                        expectAll
                            [ firstEndPoint |> Expect.point2d secondStartPoint
                            , firstEndDerivative |> Expect.vector2d secondStartDerivative
                            ]
                    )


splitAt : Test
splitAt =
    Test.describe "splitAt"
        [ Test.fuzz3
            Fuzz.rationalQuadraticSpline2d
            Fuzz.parameterValue
            Fuzz.parameterValue
            "first"
            (\spline t0 t1 ->
                let
                    ( first, _ ) =
                        RationalQuadraticSpline2d.splitAt t0 spline
                in
                RationalQuadraticSpline2d.pointOn first t1
                    |> Expect.point2d
                        (RationalQuadraticSpline2d.pointOn spline (t1 * t0))
            )
        , Test.fuzz3
            Fuzz.rationalQuadraticSpline2d
            Fuzz.parameterValue
            Fuzz.parameterValue
            "second"
            (\spline t0 t1 ->
                let
                    ( _, second ) =
                        RationalQuadraticSpline2d.splitAt t0 spline
                in
                RationalQuadraticSpline2d.pointOn second t1
                    |> Expect.point2d
                        (RationalQuadraticSpline2d.pointOn spline (t0 + t1 * (1 - t0)))
            )
        ]


secondDerivativeBoundingBox : Test
secondDerivativeBoundingBox =
    Tests.Generic.Curve2d.secondDerivativeBoundingBox
        { generator = Random.rationalQuadraticSpline2d
        , secondDerivative = RationalQuadraticSpline2d.secondDerivative
        , secondDerivativeBoundingBox = RationalQuadraticSpline2d.secondDerivativeBoundingBox
        }
