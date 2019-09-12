module Tests.CubicSpline3d exposing
    ( arcLengthMatchesAnalytical
    , fromEndpointsReproducesSpline
    , pointAtArcLengthIsEnd
    , pointAtZeroLengthIsStart
    )

import CubicSpline3d
import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (meters)
import Quantity exposing (zero)
import Test exposing (Test)
import Tests.QuadraticSpline3d
import Vector3d


fromEndpointsReproducesSpline : Test
fromEndpointsReproducesSpline =
    Test.fuzz Fuzz.cubicSpline3d
        "CubicSpline3d.fromEndpoints reproduces original spline"
        (\spline ->
            let
                startPoint =
                    CubicSpline3d.startPoint spline

                endPoint =
                    CubicSpline3d.endPoint spline

                startDerivative =
                    CubicSpline3d.startDerivative spline

                endDerivative =
                    CubicSpline3d.endDerivative spline
            in
            CubicSpline3d.fromEndpoints startPoint startDerivative endPoint endDerivative
                |> Expect.cubicSpline3d spline
        )


arcLengthMatchesAnalytical : Test
arcLengthMatchesAnalytical =
    Test.fuzz Tests.QuadraticSpline3d.curvedSpline
        "arc length matches analytical formula"
        (\quadraticSpline ->
            quadraticSpline
                |> CubicSpline3d.fromQuadraticSpline
                |> CubicSpline3d.nondegenerate
                |> Result.map
                    (CubicSpline3d.arcLengthParameterized
                        { maxError = meters 0.001 }
                        >> CubicSpline3d.arcLength
                    )
                |> Result.withDefault zero
                |> Expect.quantityWithin (meters 0.001)
                    (Tests.QuadraticSpline3d.analyticalLength quadraticSpline)
        )


pointAtZeroLengthIsStart : Test
pointAtZeroLengthIsStart =
    Test.fuzz Fuzz.cubicSpline3d
        "point along spline at zero length is start point"
        (\spline ->
            case CubicSpline3d.nondegenerate spline of
                Ok nondegenerateSpline ->
                    let
                        parameterizedSpline =
                            nondegenerateSpline
                                |> CubicSpline3d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }
                    in
                    CubicSpline3d.pointAlong parameterizedSpline (meters 0)
                        |> Expect.point3d (CubicSpline3d.startPoint spline)

                Err _ ->
                    Expect.pass
        )


pointAtArcLengthIsEnd : Test
pointAtArcLengthIsEnd =
    Test.fuzz Fuzz.cubicSpline3d
        "point along spline at arc length is end point"
        (\spline ->
            case CubicSpline3d.nondegenerate spline of
                Ok nondegenerateSpline ->
                    let
                        parameterizedSpline =
                            nondegenerateSpline
                                |> CubicSpline3d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }

                        arcLength =
                            CubicSpline3d.arcLength parameterizedSpline
                    in
                    CubicSpline3d.pointAlong parameterizedSpline arcLength
                        |> Expect.point3d (CubicSpline3d.endPoint spline)

                Err _ ->
                    Expect.pass
        )
