module Tests.QuadraticSpline3d exposing
    ( analyticalLength
    , arcLengthMatchesAnalytical
    , curvedSpline
    , pointAtArcLengthIsEnd
    , pointAtZeroLengthIsStart
    )

import Angle exposing (degrees, radians)
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Test exposing (..)
import Length exposing (Length, inMeters, meters)
import Point3d
import QuadraticSpline3d
import Quantity exposing (zero)
import Test exposing (Test)
import Tests.QuadraticSpline2d


analyticalLength : QuadraticSpline3d coordinates -> Length
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


curvedSpline : Fuzzer (QuadraticSpline3d coordinates)
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
