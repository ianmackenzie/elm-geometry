module Tests.CubicSpline2d exposing
    ( arcLengthMatchesAnalytical
    , bSplineReproducesSpline
    , bSplinesAreContinuous
    , fromEndpointsReproducesSpline
    , genericTests
    , pointAtArcLengthIsEnd
    , pointAtZeroLengthIsStart
    , secondDerivativeBoundingBox
    )

import CubicSpline2d exposing (CubicSpline2d)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Random as Random
import Interval
import Length exposing (Meters, meters)
import Point2d
import Quantity exposing (zero)
import Test exposing (Test)
import Tests.Generic.Curve2d
import Tests.QuadraticSpline2d
import Vector2d


curveOperations : Tests.Generic.Curve2d.Operations (CubicSpline2d Meters coordinates) coordinates
curveOperations =
    { generator = Random.cubicSpline2d
    , pointOn = CubicSpline2d.pointOn
    , boundingBox = CubicSpline2d.boundingBox
    , firstDerivative = CubicSpline2d.firstDerivative
    , firstDerivativeBoundingBox = CubicSpline2d.firstDerivativeBoundingBox
    , scaleAbout = CubicSpline2d.scaleAbout
    , translateBy = CubicSpline2d.translateBy
    , rotateAround = CubicSpline2d.rotateAround
    , mirrorAcross = CubicSpline2d.mirrorAcross
    , numApproximationSegments = CubicSpline2d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve2d.tests
        curveOperations
        curveOperations
        CubicSpline2d.placeIn
        CubicSpline2d.relativeTo


fromEndpointsReproducesSpline : Test
fromEndpointsReproducesSpline =
    Test.fuzz Fuzz.cubicSpline2d
        "CubicSpline2d.fromEndpoints reproduces original spline"
        (\spline ->
            let
                startPoint =
                    CubicSpline2d.startPoint spline

                endPoint =
                    CubicSpline2d.endPoint spline

                startDerivative =
                    CubicSpline2d.startDerivative spline

                endDerivative =
                    CubicSpline2d.endDerivative spline
            in
            CubicSpline2d.fromEndpoints startPoint startDerivative endPoint endDerivative
                |> Expect.cubicSpline2d spline
        )


arcLengthMatchesAnalytical : Test
arcLengthMatchesAnalytical =
    Test.fuzz Tests.QuadraticSpline2d.curvedSpline
        "arc length matches analytical formula"
        (\quadraticSpline ->
            quadraticSpline
                |> CubicSpline2d.fromQuadraticSpline
                |> CubicSpline2d.nondegenerate
                |> Result.map
                    (CubicSpline2d.arcLengthParameterized
                        { maxError = meters 0.001 }
                        >> CubicSpline2d.arcLength
                    )
                |> Result.withDefault zero
                |> Expect.quantityWithin (meters 0.001)
                    (Tests.QuadraticSpline2d.analyticalLength quadraticSpline)
        )


pointAtZeroLengthIsStart : Test
pointAtZeroLengthIsStart =
    Test.fuzz Fuzz.cubicSpline2d
        "point along spline at zero length is start point"
        (\spline ->
            case CubicSpline2d.nondegenerate spline of
                Ok nondegenerateSpline ->
                    let
                        parameterizedSpline =
                            nondegenerateSpline
                                |> CubicSpline2d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }
                    in
                    CubicSpline2d.pointAlong parameterizedSpline (meters 0)
                        |> Expect.point2d (CubicSpline2d.startPoint spline)

                Err _ ->
                    Expect.pass
        )


pointAtArcLengthIsEnd : Test
pointAtArcLengthIsEnd =
    Test.fuzz Fuzz.cubicSpline2d
        "point along spline at arc length is end point"
        (\spline ->
            case CubicSpline2d.nondegenerate spline of
                Ok nondegenerateSpline ->
                    let
                        parameterizedSpline =
                            nondegenerateSpline
                                |> CubicSpline2d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }

                        arcLength =
                            CubicSpline2d.arcLength parameterizedSpline
                    in
                    CubicSpline2d.pointAlong parameterizedSpline arcLength
                        |> Expect.point2d (CubicSpline2d.endPoint spline)

                Err _ ->
                    Expect.pass
        )


bSplineReproducesSpline : Test
bSplineReproducesSpline =
    Test.fuzz Fuzz.cubicSpline2d
        "Can reconstruct a cubic spline with a B-spline with repeated knots"
        (\spline ->
            let
                p1 =
                    CubicSpline2d.firstControlPoint spline

                p2 =
                    CubicSpline2d.secondControlPoint spline

                p3 =
                    CubicSpline2d.thirdControlPoint spline

                p4 =
                    CubicSpline2d.fourthControlPoint spline

                bSplineSegments =
                    CubicSpline2d.bSplineSegments [ 0, 0, 0, 1, 1, 1 ]
                        [ p1, p2, p3, p4 ]
            in
            case bSplineSegments of
                [ singleSegment ] ->
                    singleSegment |> Expect.cubicSpline2d spline

                _ ->
                    Expect.fail "Expected a single B-spline segment"
        )


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


bSplinesAreContinuous : Test
bSplinesAreContinuous =
    Test.test "B-splines are continuous" <|
        \() ->
            let
                knots =
                    [ 0, 0, 0, 1, 2, 4, 4, 5, 8, 10, 10, 10 ]

                controlPoints =
                    [ Point2d.meters 0 0
                    , Point2d.meters 1 8
                    , Point2d.meters 4 4
                    , Point2d.meters 2 4
                    , Point2d.meters 4 1
                    , Point2d.meters 8 2
                    , Point2d.meters 5 6
                    , Point2d.meters 8 9
                    , Point2d.meters 9 7
                    , Point2d.meters 9 4
                    ]

                splines =
                    CubicSpline2d.bSplineSegments knots controlPoints

                knotIntervals =
                    CubicSpline2d.bSplineIntervals knots

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
                                CubicSpline2d.endPoint firstSpline

                            secondStartPoint =
                                CubicSpline2d.startPoint secondSpline

                            firstEndDerivative =
                                CubicSpline2d.endDerivative firstSpline
                                    |> Vector2d.scaleBy (1.0 / Interval.width firstInterval)

                            secondStartDerivative =
                                CubicSpline2d.startDerivative secondSpline
                                    |> Vector2d.scaleBy (1.0 / Interval.width secondInterval)
                        in
                        expectAll
                            [ firstEndPoint |> Expect.point2d secondStartPoint
                            , firstEndDerivative |> Expect.vector2d secondStartDerivative
                            ]
                    )


secondDerivativeBoundingBox : Test
secondDerivativeBoundingBox =
    Tests.Generic.Curve2d.secondDerivativeBoundingBox
        { generator = Random.cubicSpline2d
        , secondDerivative = CubicSpline2d.secondDerivative
        , secondDerivativeBoundingBox = CubicSpline2d.secondDerivativeBoundingBox
        }
