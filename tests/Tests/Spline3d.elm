module Tests.Spline3d exposing
    ( bSplineSegmentsReproducesCubicSpline
    , bSplineSegmentsReproducesQuadraticSpline
    , consistentWithCubicBSpline
    , consistentWithCubicBSplineIntervals
    , consistentWithQuadraticBSpline
    , consistentWithQuadraticBSplineIntervals
    , genericTests
    , projectInto
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

import CubicSpline3d exposing (CubicSpline3d)
import Expect exposing (Expectation)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Geometry.Types exposing (QuadraticSpline3d)
import Length exposing (Meters)
import LineSegment3d
import QuadraticSpline1d exposing (QuadraticSpline1d)
import QuadraticSpline3d
import Random
import Spline3d exposing (Spline3d)
import Test exposing (Test)
import Test.Check as Test
import Tests.Generic.Curve3d
import Tests.Spline2d
import Vector3d


curveOperations : Tests.Generic.Curve3d.Operations (Spline3d Meters coordinates) coordinates
curveOperations =
    { generator = Random.spline3d
    , pointOn = Spline3d.pointOn
    , boundingBox = Spline3d.boundingBox
    , firstDerivative = Spline3d.firstDerivative
    , firstDerivativeBoundingBox = Spline3d.firstDerivativeBoundingBox
    , scaleAbout = Spline3d.scaleAbout
    , translateBy = Spline3d.translateBy
    , rotateAround = Spline3d.rotateAround
    , mirrorAcross = Spline3d.mirrorAcross
    , numApproximationSegments = Spline3d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve3d.tests
        curveOperations
        curveOperations
        Spline3d.placeIn
        Spline3d.relativeTo


reproducesPoint : Test
reproducesPoint =
    Test.check2 "Spline3d reproduces point"
        Random.point3d
        Random.parameterValue
        (\point t ->
            let
                spline =
                    Spline3d.fromControlPoints point []
            in
            Spline3d.pointOn spline t
                |> Expect.point3d point
        )


reproducesPointDerivative : Test
reproducesPointDerivative =
    Test.check2 "Spline3d reproduces point derivative"
        Random.point3d
        Random.parameterValue
        (\point t ->
            let
                spline =
                    Spline3d.fromControlPoints point []
            in
            Spline3d.firstDerivative spline t |> Expect.vector3d Vector3d.zero
        )


reproducesPointSecondDerivative : Test
reproducesPointSecondDerivative =
    Test.check2 "Spline3d reproduces point second derivative"
        Random.point3d
        Random.parameterValue
        (\point t ->
            let
                spline =
                    Spline3d.fromControlPoints point []
            in
            Spline3d.secondDerivative spline t |> Expect.vector3d Vector3d.zero
        )


reproducesLine : Test
reproducesLine =
    Test.check2 "Spline3d reproduces line"
        Random.lineSegment3d
        Random.parameterValue
        (\lineSegment t ->
            let
                spline =
                    Spline3d.fromControlPoints
                        (LineSegment3d.startPoint lineSegment)
                        [ LineSegment3d.endPoint lineSegment ]
            in
            Spline3d.pointOn spline t
                |> Expect.point3d (LineSegment3d.interpolate lineSegment t)
        )


reproducesLineDerivative : Test
reproducesLineDerivative =
    Test.check2 "Spline3d reproduces line derivative"
        Random.lineSegment3d
        Random.parameterValue
        (\lineSegment t ->
            let
                spline =
                    Spline3d.fromControlPoints
                        (LineSegment3d.startPoint lineSegment)
                        [ LineSegment3d.endPoint lineSegment ]
            in
            Spline3d.firstDerivative spline t
                |> Expect.vector3d (LineSegment3d.vector lineSegment)
        )


reproducesLineSecondDerivative : Test
reproducesLineSecondDerivative =
    Test.check2 "Spline3d reproduces line second derivative"
        Random.lineSegment3d
        Random.parameterValue
        (\lineSegment t ->
            let
                spline =
                    Spline3d.fromControlPoints
                        (LineSegment3d.startPoint lineSegment)
                        [ LineSegment3d.endPoint lineSegment ]
            in
            Spline3d.secondDerivative spline t
                |> Expect.vector3d Vector3d.zero
        )


reproducesQuadraticSpline : Test
reproducesQuadraticSpline =
    Test.check2 "Spline3d reproduces quadratic spline"
        Random.quadraticSpline3d
        Random.parameterValue
        (\quadraticSpline t ->
            let
                spline =
                    Spline3d.fromQuadraticSpline quadraticSpline
            in
            Spline3d.pointOn spline t
                |> Expect.point3d (QuadraticSpline3d.pointOn quadraticSpline t)
        )


reproducesQuadraticSplineDerivative : Test
reproducesQuadraticSplineDerivative =
    Test.check2 "Spline3d reproduces quadratic spline derivative"
        Random.quadraticSpline3d
        Random.parameterValue
        (\quadraticSpline t ->
            let
                spline =
                    Spline3d.fromQuadraticSpline quadraticSpline
            in
            Spline3d.firstDerivative spline t
                |> Expect.vector3d (QuadraticSpline3d.firstDerivative quadraticSpline t)
        )


reproducesQuadraticSplineSecondDerivative : Test
reproducesQuadraticSplineSecondDerivative =
    Test.check2 "Spline3d reproduces quadratic spline second derivative"
        Random.quadraticSpline3d
        Random.parameterValue
        (\quadraticSpline t ->
            let
                spline =
                    Spline3d.fromQuadraticSpline quadraticSpline
            in
            Spline3d.secondDerivative spline t
                |> Expect.vector3d (QuadraticSpline3d.secondDerivative quadraticSpline)
        )


reproducesCubicSpline : Test
reproducesCubicSpline =
    Test.check2 "Spline3d reproduces cubic spline"
        Random.cubicSpline3d
        Random.parameterValue
        (\cubicSpline t ->
            let
                spline =
                    Spline3d.fromCubicSpline cubicSpline
            in
            Spline3d.pointOn spline t
                |> Expect.point3d (CubicSpline3d.pointOn cubicSpline t)
        )


reproducesCubicSplineDerivative : Test
reproducesCubicSplineDerivative =
    Test.check2 "Spline3d reproduces cubic spline derivative"
        Random.cubicSpline3d
        Random.parameterValue
        (\cubicSpline t ->
            let
                spline =
                    Spline3d.fromCubicSpline cubicSpline
            in
            Spline3d.firstDerivative spline t
                |> Expect.vector3d (CubicSpline3d.firstDerivative cubicSpline t)
        )


reproducesCubicSplineSecondDerivative : Test
reproducesCubicSplineSecondDerivative =
    Test.check2 "Spline3d reproduces cubic spline second derivative"
        Random.cubicSpline3d
        Random.parameterValue
        (\cubicSpline t ->
            let
                spline =
                    Spline3d.fromCubicSpline cubicSpline
            in
            Spline3d.secondDerivative spline t
                |> Expect.vector3d (CubicSpline3d.secondDerivative cubicSpline t)
        )


matchesCubicSpline : Spline3d units coordinates -> CubicSpline3d units coordinates -> Expectation
matchesCubicSpline spline cubicSpline =
    Expect.list Expect.point3d
        (Spline3d.controlPoints spline)
        [ CubicSpline3d.firstControlPoint cubicSpline
        , CubicSpline3d.secondControlPoint cubicSpline
        , CubicSpline3d.thirdControlPoint cubicSpline
        , CubicSpline3d.fourthControlPoint cubicSpline
        ]


consistentWithCubicBSpline : Test
consistentWithCubicBSpline =
    Test.check2 "Spline3d B-spline is consistent with cubic B-spline"
        (Random.map List.sort (Random.list 12 (Random.float 0 10)))
        (Random.list 10 Random.point3d)
        (\knots controlPoints ->
            let
                cubicSplineSegments =
                    CubicSpline3d.bSplineSegments knots controlPoints

                splineSegments =
                    Spline3d.bSplineSegments 3 knots controlPoints
            in
            Expect.list matchesCubicSpline splineSegments cubicSplineSegments
        )


matchesQuadraticSpline : Spline3d units coordinates -> QuadraticSpline3d units coordinates -> Expectation
matchesQuadraticSpline spline quadraticSpline =
    Expect.list Expect.point3d
        (Spline3d.controlPoints spline)
        [ QuadraticSpline3d.firstControlPoint quadraticSpline
        , QuadraticSpline3d.secondControlPoint quadraticSpline
        , QuadraticSpline3d.thirdControlPoint quadraticSpline
        ]


consistentWithQuadraticBSpline : Test
consistentWithQuadraticBSpline =
    Test.check2 "Spline3d B-spline is consistent with quadratic B-spline"
        (Random.map List.sort (Random.list 11 (Random.float 0 10)))
        (Random.list 10 Random.point3d)
        (\knots controlPoints ->
            let
                quadraticSplineSegments =
                    QuadraticSpline3d.bSplineSegments knots controlPoints

                splineSegments =
                    Spline3d.bSplineSegments 2 knots controlPoints
            in
            Expect.list matchesQuadraticSpline splineSegments quadraticSplineSegments
        )


consistentWithQuadraticBSplineIntervals : Test
consistentWithQuadraticBSplineIntervals =
    Test.check "Spline3d.bSplineIntervals is consistent with QuadraticSpline3d.bSplineIntervals"
        (Random.map List.sort (Random.list 12 (Random.float 0 10)))
        (\knots ->
            Spline3d.bSplineIntervals 2 knots
                |> Expect.equal (QuadraticSpline3d.bSplineIntervals knots)
        )


consistentWithCubicBSplineIntervals : Test
consistentWithCubicBSplineIntervals =
    Test.check "Spline3d.bSplineIntervals is consistent with CubicSpline3d.bSplineIntervals"
        (Random.map List.sort (Random.list 12 (Random.float 0 10)))
        (\knots ->
            Spline3d.bSplineIntervals 3 knots
                |> Expect.equal (CubicSpline3d.bSplineIntervals knots)
        )


projectInto : Test
projectInto =
    Tests.Generic.Curve3d.projectInto
        curveOperations
        Spline3d.projectInto
        Tests.Spline2d.curveOperations


bSplineSegmentsReproducesQuadraticSpline : Test
bSplineSegmentsReproducesQuadraticSpline =
    Test.check4 "Spline3d.bSplineSegments correctly reproduces a single quadratic spline"
        Random.point3d
        Random.point3d
        Random.point3d
        (Random.float 0 1)
        (\p1 p2 p3 t ->
            let
                quadraticSpline =
                    QuadraticSpline3d.fromControlPoints p1 p2 p3
            in
            case Spline3d.bSplineSegments 2 [ 0, 0, 1, 1 ] [ p1, p2, p3 ] of
                [ segment ] ->
                    let
                        pointOnSpline =
                            QuadraticSpline3d.pointOn quadraticSpline t

                        pointOnSegment =
                            Spline3d.pointOn segment t
                    in
                    pointOnSegment |> Expect.point3d pointOnSpline

                _ ->
                    Expect.fail "Expected a single B-spline segment"
        )


bSplineSegmentsReproducesCubicSpline : Test
bSplineSegmentsReproducesCubicSpline =
    Test.check5 "Spline3d.bSplineSegments correctly reproduces a single cubic spline"
        Random.point3d
        Random.point3d
        Random.point3d
        Random.point3d
        (Random.float 0 1)
        (\p1 p2 p3 p4 t ->
            let
                cubicSpline =
                    CubicSpline3d.fromControlPoints p1 p2 p3 p4
            in
            case Spline3d.bSplineSegments 3 [ 0, 0, 0, 1, 1, 1 ] [ p1, p2, p3, p4 ] of
                [ segment ] ->
                    let
                        pointOnSpline =
                            CubicSpline3d.pointOn cubicSpline t

                        pointOnSegment =
                            Spline3d.pointOn segment t
                    in
                    pointOnSegment |> Expect.point3d pointOnSpline

                _ ->
                    Expect.fail "Expected a single B-spline segment"
        )
