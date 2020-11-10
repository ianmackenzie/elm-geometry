module Tests.RationalCubicSpline2d exposing (bSplines, firstDerivative, transformations)

import CubicSpline2d
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Interval
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Quantity
import Random
import RationalCubicSpline2d exposing (RationalCubicSpline2d)
import Shrink
import Test exposing (Test)
import Tests.Generic.Curve2d
import Vector2d


curveOperations : Tests.Generic.Curve2d.Operations (RationalCubicSpline2d Meters coordinates) coordinates
curveOperations =
    { fuzzer = Fuzz.rationalCubicSpline2d
    , pointOn = RationalCubicSpline2d.pointOn
    , firstDerivative = RationalCubicSpline2d.firstDerivative
    , scaleAbout = RationalCubicSpline2d.scaleAbout
    , translateBy = RationalCubicSpline2d.translateBy
    , rotateAround = RationalCubicSpline2d.rotateAround
    , mirrorAcross = RationalCubicSpline2d.mirrorAcross
    }


transformations : Test
transformations =
    Tests.Generic.Curve2d.transformations
        curveOperations
        curveOperations
        RationalCubicSpline2d.placeIn
        RationalCubicSpline2d.relativeTo


firstDerivative : Test
firstDerivative =
    Tests.Generic.Curve2d.firstDerivative curveOperations


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
                    [ ( Point2d.meters 0 0, 1 )
                    , ( Point2d.meters 1 8, 20 )
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
                    RationalCubicSpline2d.bSplineSegments knots weightedControlPoints

                knotIntervals =
                    RationalCubicSpline2d.bSplineIntervals knots

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
                                RationalCubicSpline2d.endPoint firstSpline

                            secondStartPoint =
                                RationalCubicSpline2d.startPoint secondSpline

                            firstEndDerivative =
                                RationalCubicSpline2d.endDerivative firstSpline
                                    |> Vector2d.scaleBy (1.0 / Interval.width firstInterval)

                            secondStartDerivative =
                                RationalCubicSpline2d.startDerivative secondSpline
                                    |> Vector2d.scaleBy (1.0 / Interval.width secondInterval)
                        in
                        expectAll
                            [ firstEndPoint |> Expect.point2d secondStartPoint
                            , firstEndDerivative |> Expect.vector2d secondStartDerivative
                            ]
                    )
