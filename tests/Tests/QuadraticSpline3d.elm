module Tests.QuadraticSpline3d exposing
    ( analyticalLength
    , arcLengthMatchesAnalytical
    , bSplineReproducesSpline
    , curvedSpline
    , genericTests
    , pointAtArcLengthIsEnd
    , pointAtZeroLengthIsStart
    )

import Angle exposing (degrees, radians)
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Random as Random
import Geometry.Types exposing (QuadraticSpline3d)
import Length exposing (Length, Meters, inMeters, meters)
import Point3d
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (zero)
import Test exposing (Test)
import Tests.Generic.Curve3d
import Tests.QuadraticSpline2d


curveOperations : Tests.Generic.Curve3d.Operations (QuadraticSpline3d Meters coordinates) coordinates
curveOperations =
    { generator = Random.quadraticSpline3d
    , pointOn = QuadraticSpline3d.pointOn
    , boundingBox = QuadraticSpline3d.boundingBox
    , firstDerivative = QuadraticSpline3d.firstDerivative
    , firstDerivativeBoundingBox = QuadraticSpline3d.firstDerivativeBoundingBox
    , scaleAbout = QuadraticSpline3d.scaleAbout
    , translateBy = QuadraticSpline3d.translateBy
    , rotateAround = QuadraticSpline3d.rotateAround
    , mirrorAcross = QuadraticSpline3d.mirrorAcross
    , numApproximationSegments = QuadraticSpline3d.numApproximationSegments
    }


genericTests : Test
genericTests =
    Tests.Generic.Curve3d.tests
        curveOperations
        curveOperations
        QuadraticSpline3d.placeIn
        QuadraticSpline3d.relativeTo


analyticalLength : QuadraticSpline3d Meters coordinates -> Length
analyticalLength spline =
    let
        p0 =
            QuadraticSpline3d.firstControlPoint spline

        p1 =
            QuadraticSpline3d.secondControlPoint spline

        p2 =
            QuadraticSpline3d.thirdControlPoint spline

        ( x0, y0, z0 ) =
            Point3d.toTuple inMeters p0

        ( x1, y1, z1 ) =
            Point3d.toTuple inMeters p1

        ( x2, y2, z2 ) =
            Point3d.toTuple inMeters p2

        ax =
            x0 - 2 * x1 + x2

        ay =
            y0 - 2 * y1 + y2

        az =
            z0 - 2 * z1 + z2

        bx =
            2 * x1 - 2 * x0

        by =
            2 * y1 - 2 * y0

        bz =
            2 * z1 - 2 * z0

        a =
            4 * (ax * ax + ay * ay + az * az)

        b =
            4 * (ax * bx + ay * by + az * bz)

        c =
            bx * bx + by * by + bz * bz

        s_abc =
            2 * sqrt (a + b + c)

        a_2 =
            sqrt a

        a_32 =
            2 * a * a_2

        c_2 =
            2 * sqrt c

        ba =
            b / a_2
    in
    meters ((a_32 * s_abc + a_2 * b * (s_abc - c_2) + (4 * c * a - b * b) * logBase e ((2 * a_2 + ba + s_abc) / (ba + c_2))) / (4 * a_32))


curvedSpline : Fuzzer (QuadraticSpline3d Meters coordinates)
curvedSpline =
    Fuzz.map2 QuadraticSpline3d.on
        Fuzz.sketchPlane3d
        Tests.QuadraticSpline2d.curvedSpline


arcLengthMatchesAnalytical : Test
arcLengthMatchesAnalytical =
    Test.fuzz curvedSpline
        "arc length matches analytical formula"
        (\spline ->
            let
                tolerance =
                    meters 0.001

                measuredArcLength =
                    spline
                        |> QuadraticSpline3d.nondegenerate
                        |> Result.map
                            (QuadraticSpline3d.arcLengthParameterized
                                { maxError = tolerance }
                                >> QuadraticSpline3d.arcLength
                            )
                        |> Result.withDefault zero
            in
            measuredArcLength
                |> Expect.quantityWithin tolerance (analyticalLength spline)
        )


pointAtZeroLengthIsStart : Test
pointAtZeroLengthIsStart =
    Test.fuzz Fuzz.quadraticSpline3d
        "point along spline at zero length is start point"
        (\spline ->
            case QuadraticSpline3d.nondegenerate spline of
                Ok nondegenerateSpline ->
                    let
                        parameterizedSpline =
                            nondegenerateSpline
                                |> QuadraticSpline3d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }
                    in
                    QuadraticSpline3d.pointAlong parameterizedSpline (meters 0)
                        |> Expect.point3d (QuadraticSpline3d.startPoint spline)

                Err _ ->
                    Expect.pass
        )


pointAtArcLengthIsEnd : Test
pointAtArcLengthIsEnd =
    Test.fuzz Fuzz.quadraticSpline3d
        "point along spline at arc length is end point"
        (\spline ->
            case QuadraticSpline3d.nondegenerate spline of
                Ok nondegenerateSpline ->
                    let
                        parameterizedSpline =
                            nondegenerateSpline
                                |> QuadraticSpline3d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }

                        arcLength =
                            QuadraticSpline3d.arcLength parameterizedSpline
                    in
                    QuadraticSpline3d.pointAlong parameterizedSpline arcLength
                        |> Expect.point3d (QuadraticSpline3d.endPoint spline)

                Err _ ->
                    Expect.pass
        )


bSplineReproducesSpline : Test
bSplineReproducesSpline =
    Test.fuzz Fuzz.quadraticSpline3d
        "Can reconstruct a quadratic spline with a B-spline with repeated knots"
        (\spline ->
            let
                p1 =
                    QuadraticSpline3d.firstControlPoint spline

                p2 =
                    QuadraticSpline3d.secondControlPoint spline

                p3 =
                    QuadraticSpline3d.thirdControlPoint spline

                bSplineSegments =
                    QuadraticSpline3d.bSplineSegments [ 0, 0, 1, 1 ]
                        [ p1, p2, p3 ]
            in
            case bSplineSegments of
                [ singleSegment ] ->
                    singleSegment |> Expect.quadraticSpline3d spline

                _ ->
                    Expect.fail "Expected a single B-spline segment"
        )
