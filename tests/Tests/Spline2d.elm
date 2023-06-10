module Tests.Spline2d exposing
    ( bSplineSegmentsReproducesCubicSpline
    , bSplineSegmentsReproducesQuadraticSpline
    , consistentWithCubicBSpline
    , consistentWithCubicBSplineIntervals
    , consistentWithQuadraticBSpline
    , consistentWithQuadraticBSplineIntervals
    , curveOperations
    , genericTests
    , reproducesCubicSpline
    , reproducesCubicSplineDerivative
    , reproducesCubicSplineSecondDerivative
    , reproducesLine
    , reproducesLineDerivative
    , reproducesLineSecondDerivative
    , reproducesPoint
    , reproducesPointDerivative
    , reproducesPointSecondDerivative
    , reproducesQuadraticSpline
    , reproducesQuadraticSplineDerivative
    , reproducesQuadraticSplineSecondDerivative
    )

import CubicSpline2d exposing (CubicSpline2d, bSplineSegments)
import Expect exposing (Expectation)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Geometry.Types exposing (QuadraticSpline2d)
import Length exposing (Meters)
import LineSegment2d
import QuadraticSpline1d exposing (QuadraticSpline1d)
import QuadraticSpline2d
import Random
import Spline2d exposing (Spline2d)
import Test exposing (Test)
import Test.Check as Test
import Tests.Generic.Curve2d
import Vector2d


curveOperations : Tests.Generic.Curve2d.Operations (Spline2d Meters coordinates) coordinates
curveOperations =
    { generator = Random.spline2d
    , pointOn = Spline2d.pointOn
    , boundingBox = Spline2d.boundingBox
    , firstDerivative = Spline2d.firstDerivative
    , firstDerivativeBoundingBox = Spline2d.firstDerivativeBoundingBox
    , scaleAbout = Spline2d.scaleAbout
    , translateBy = Spline2d.translateBy
    , rotateAround = Spline2d.rotateAround
    , mirrorAcross = Spline2d.mirrorAcross
    , numApproximationSegments = Spline2d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve2d.tests
        curveOperations
        curveOperations
        Spline2d.placeIn
        Spline2d.relativeTo


reproducesPoint : Test
reproducesPoint =
    Test.check2 "Spline2d reproduces point"
        Random.point2d
        Random.parameterValue
        (\point t ->
            let
                spline =
                    Spline2d.fromControlPoints point []
            in
            Spline2d.pointOn spline t
                |> Expect.point2d point
        )


reproducesPointDerivative : Test
reproducesPointDerivative =
    Test.check2 "Spline2d reproduces point derivative"
        Random.point2d
        Random.parameterValue
        (\point t ->
            let
                spline =
                    Spline2d.fromControlPoints point []
            in
            Spline2d.firstDerivative spline t |> Expect.vector2d Vector2d.zero
        )


reproducesPointSecondDerivative : Test
reproducesPointSecondDerivative =
    Test.check2 "Spline2d reproduces point second derivative"
        Random.point2d
        Random.parameterValue
        (\point t ->
            let
                spline =
                    Spline2d.fromControlPoints point []
            in
            Spline2d.secondDerivative spline t |> Expect.vector2d Vector2d.zero
        )


reproducesLine : Test
reproducesLine =
    Test.check2 "Spline2d reproduces line"
        Random.lineSegment2d
        Random.parameterValue
        (\lineSegment t ->
            let
                spline =
                    Spline2d.fromControlPoints
                        (LineSegment2d.startPoint lineSegment)
                        [ LineSegment2d.endPoint lineSegment ]
            in
            Spline2d.pointOn spline t
                |> Expect.point2d (LineSegment2d.interpolate lineSegment t)
        )


reproducesLineDerivative : Test
reproducesLineDerivative =
    Test.check2 "Spline2d reproduces line derivative"
        Random.lineSegment2d
        Random.parameterValue
        (\lineSegment t ->
            let
                spline =
                    Spline2d.fromControlPoints
                        (LineSegment2d.startPoint lineSegment)
                        [ LineSegment2d.endPoint lineSegment ]
            in
            Spline2d.firstDerivative spline t
                |> Expect.vector2d (LineSegment2d.vector lineSegment)
        )


reproducesLineSecondDerivative : Test
reproducesLineSecondDerivative =
    Test.check2 "Spline2d reproduces line second derivative"
        Random.lineSegment2d
        Random.parameterValue
        (\lineSegment t ->
            let
                spline =
                    Spline2d.fromControlPoints
                        (LineSegment2d.startPoint lineSegment)
                        [ LineSegment2d.endPoint lineSegment ]
            in
            Spline2d.secondDerivative spline t
                |> Expect.vector2d Vector2d.zero
        )


reproducesQuadraticSpline : Test
reproducesQuadraticSpline =
    Test.check2 "Spline2d reproduces quadratic spline"
        Random.quadraticSpline2d
        Random.parameterValue
        (\quadraticSpline t ->
            let
                spline =
                    Spline2d.fromQuadraticSpline quadraticSpline
            in
            Spline2d.pointOn spline t
                |> Expect.point2d (QuadraticSpline2d.pointOn quadraticSpline t)
        )


reproducesQuadraticSplineDerivative : Test
reproducesQuadraticSplineDerivative =
    Test.check2 "Spline2d reproduces quadratic spline derivative"
        Random.quadraticSpline2d
        Random.parameterValue
        (\quadraticSpline t ->
            let
                spline =
                    Spline2d.fromQuadraticSpline quadraticSpline
            in
            Spline2d.firstDerivative spline t
                |> Expect.vector2d (QuadraticSpline2d.firstDerivative quadraticSpline t)
        )


reproducesQuadraticSplineSecondDerivative : Test
reproducesQuadraticSplineSecondDerivative =
    Test.check2 "Spline2d reproduces quadratic spline second derivative"
        Random.quadraticSpline2d
        Random.parameterValue
        (\quadraticSpline t ->
            let
                spline =
                    Spline2d.fromQuadraticSpline quadraticSpline
            in
            Spline2d.secondDerivative spline t
                |> Expect.vector2d (QuadraticSpline2d.secondDerivative quadraticSpline)
        )


reproducesCubicSpline : Test
reproducesCubicSpline =
    Test.check2 "Spline2d reproduces cubic spline"
        Random.cubicSpline2d
        Random.parameterValue
        (\cubicSpline t ->
            let
                spline =
                    Spline2d.fromCubicSpline cubicSpline
            in
            Spline2d.pointOn spline t
                |> Expect.point2d (CubicSpline2d.pointOn cubicSpline t)
        )


reproducesCubicSplineDerivative : Test
reproducesCubicSplineDerivative =
    Test.check2 "Spline2d reproduces cubic spline derivative"
        Random.cubicSpline2d
        Random.parameterValue
        (\cubicSpline t ->
            let
                spline =
                    Spline2d.fromCubicSpline cubicSpline
            in
            Spline2d.firstDerivative spline t
                |> Expect.vector2d (CubicSpline2d.firstDerivative cubicSpline t)
        )


reproducesCubicSplineSecondDerivative : Test
reproducesCubicSplineSecondDerivative =
    Test.check2 "Spline2d reproduces cubic spline second derivative"
        Random.cubicSpline2d
        Random.parameterValue
        (\cubicSpline t ->
            let
                spline =
                    Spline2d.fromCubicSpline cubicSpline
            in
            Spline2d.secondDerivative spline t
                |> Expect.vector2d (CubicSpline2d.secondDerivative cubicSpline t)
        )


matchesCubicSpline : Spline2d units coordinates -> CubicSpline2d units coordinates -> Expectation
matchesCubicSpline spline cubicSpline =
    Expect.list Expect.point2d
        (Spline2d.controlPoints spline)
        [ CubicSpline2d.firstControlPoint cubicSpline
        , CubicSpline2d.secondControlPoint cubicSpline
        , CubicSpline2d.thirdControlPoint cubicSpline
        , CubicSpline2d.fourthControlPoint cubicSpline
        ]


consistentWithCubicBSpline : Test
consistentWithCubicBSpline =
    Test.check2 "Spline2d B-spline is consistent with cubic B-spline"
        (Random.map List.sort (Random.list 12 (Random.float 0 10)))
        (Random.list 10 Random.point2d)
        (\knots controlPoints ->
            let
                cubicSplineSegments =
                    CubicSpline2d.bSplineSegments knots controlPoints

                splineSegments =
                    Spline2d.bSplineSegments 3 knots controlPoints
            in
            Expect.list matchesCubicSpline splineSegments cubicSplineSegments
        )


matchesQuadraticSpline : Spline2d units coordinates -> QuadraticSpline2d units coordinates -> Expectation
matchesQuadraticSpline spline quadraticSpline =
    Expect.list Expect.point2d
        (Spline2d.controlPoints spline)
        [ QuadraticSpline2d.firstControlPoint quadraticSpline
        , QuadraticSpline2d.secondControlPoint quadraticSpline
        , QuadraticSpline2d.thirdControlPoint quadraticSpline
        ]


consistentWithQuadraticBSpline : Test
consistentWithQuadraticBSpline =
    Test.check2 "Spline2d B-spline is consistent with quadratic B-spline"
        (Random.map List.sort (Random.list 11 (Random.float 0 10)))
        (Random.list 10 Random.point2d)
        (\knots controlPoints ->
            let
                quadraticSplineSegments =
                    QuadraticSpline2d.bSplineSegments knots controlPoints

                splineSegments =
                    Spline2d.bSplineSegments 2 knots controlPoints
            in
            Expect.list matchesQuadraticSpline splineSegments quadraticSplineSegments
        )


consistentWithQuadraticBSplineIntervals : Test
consistentWithQuadraticBSplineIntervals =
    Test.check "Spline2d.bSplineIntervals is consistent with QuadraticSpline2d.bSplineIntervals"
        (Random.map List.sort (Random.list 12 (Random.float 0 10)))
        (\knots ->
            Spline2d.bSplineIntervals 2 knots
                |> Expect.equal (QuadraticSpline2d.bSplineIntervals knots)
        )


consistentWithCubicBSplineIntervals : Test
consistentWithCubicBSplineIntervals =
    Test.check "Spline2d.bSplineIntervals is consistent with CubicSpline2d.bSplineIntervals"
        (Random.map List.sort (Random.list 12 (Random.float 0 10)))
        (\knots ->
            Spline2d.bSplineIntervals 3 knots
                |> Expect.equal (CubicSpline2d.bSplineIntervals knots)
        )


bSplineSegmentsReproducesQuadraticSpline : Test
bSplineSegmentsReproducesQuadraticSpline =
    Test.check4 "Spline2d.bSplineSegments correctly reproduces a single quadratic spline"
        Random.point2d
        Random.point2d
        Random.point2d
        (Random.float 0 1)
        (\p1 p2 p3 t ->
            let
                quadraticSpline =
                    QuadraticSpline2d.fromControlPoints p1 p2 p3
            in
            case Spline2d.bSplineSegments 2 [ 0, 0, 1, 1 ] [ p1, p2, p3 ] of
                [ segment ] ->
                    let
                        pointOnSpline =
                            QuadraticSpline2d.pointOn quadraticSpline t

                        pointOnSegment =
                            Spline2d.pointOn segment t
                    in
                    pointOnSegment |> Expect.point2d pointOnSpline

                _ ->
                    Expect.fail "Expected a single B-spline segment"
        )


bSplineSegmentsReproducesCubicSpline : Test
bSplineSegmentsReproducesCubicSpline =
    Test.check5 "Spline2d.bSplineSegments correctly reproduces a single cubic spline"
        Random.point2d
        Random.point2d
        Random.point2d
        Random.point2d
        (Random.float 0 1)
        (\p1 p2 p3 p4 t ->
            let
                cubicSpline =
                    CubicSpline2d.fromControlPoints p1 p2 p3 p4
            in
            case Spline2d.bSplineSegments 3 [ 0, 0, 0, 1, 1, 1 ] [ p1, p2, p3, p4 ] of
                [ segment ] ->
                    let
                        pointOnSpline =
                            CubicSpline2d.pointOn cubicSpline t

                        pointOnSegment =
                            Spline2d.pointOn segment t
                    in
                    pointOnSegment |> Expect.point2d pointOnSpline

                _ ->
                    Expect.fail "Expected a single B-spline segment"
        )
